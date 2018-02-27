{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.ChickHandler where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Freer.Trace
import           Control.Monad.Loops (whileM)
import           Control.Monad.Representable.Reader (ask)
import           Data.Aeson
-- import qualified Data.ByteString.UTF8 as BSU
-- import qualified Data.ByteString.Lazy as BSL
import           Data.IORef
import qualified Data.IntMap as IM
import qualified Data.Text as T
import           GHC.Generics
import           Prelude hiding (init)
import           Snap.Core
import           Snap.Extras.JSON
import           Snap.Snaplet
import           Snap.Snaplet.Session hiding (touchSession)
import           Snap.Snaplet.Session.SessionManager ()
import           System.Directory (doesFileExist)
import           System.IO
import           System.Process
import           System.Random
import           Text.Megaparsec (parseMaybe)
import           Text.Printf

import qualified Diff.Guess.Script as ΔGS
import           Parsing.Script
import           PrettyPrinting.PrettyPrintable
import           PrettyPrinting.PrettyPrintableUnannotated
import           Repair.Benchmark
import           Server.Chick
import           Server.Session
import           Utils

type ChickHandler a = Handler Chick Chick a

data GuessInput = GuessInput
  { before :: String
  , after  :: String
  }
  deriving (Generic)

instance FromJSON GuessInput where

chickGuessHandler :: ChickHandler ()
chickGuessHandler = do
  r <- getRequest
  case rqContentLength r of
    Nothing -> return ()
    Just bodyLength -> do
      body <- readRequestBody bodyLength
      res <- case decode body of
        Nothing -> return $ Just "decode body failed"
        Just (GuessInput { before, after }) -> do
          case (parseMaybe scriptP before, parseMaybe scriptP after) of
            (Just bef, Just aft) -> do
              liftIO $ do
                putStrLn $ "BEFORE:"
                putStrLn $ prettyStrU $ bef
                putStrLn $ "AFTER:"
                putStrLn $ prettyStrU $ aft
              δ <- liftIO $ runTrace $ ΔGS.guess bef aft
              liftIO $ putStrLn $ printf "GUESSED:\n%s" (show δ)
              liftIO (runTrace (repairScript bef δ)) >>= \case
                Left e -> return $ Just e
                Right patched ->
                  return $ Just $ prettyStrU patched
                  -- ++ "\n(*"
                  -- ++ show δ
                  -- ++ "*)"
            (Nothing, Just _)  ->
              return $ Just $ "Parsing before failed"
            (Just _,  Nothing) ->
              return $ Just $ "Parsing after  failed"
            (Nothing, Nothing) ->
              return $ Just $ "Parsing before and after failed"
      writeJSON res

runCommandWithoutBuffering :: String -> IO (Handle, Handle, Handle, ProcessHandle)
runCommandWithoutBuffering cmd = do
  (hi, ho, he, ph) <- runInteractiveCommand cmd
  hSetBuffering hi NoBuffering
  hSetBuffering ho NoBuffering
  hSetBuffering he NoBuffering
  return (hi, ho, he, ph)

getGlobalState :: ChickHandler GlobalState
getGlobalState = do
  globRef <- with lGlobRef ask
  liftIO $ readIORef globRef

modifyGlobalState :: (GlobalState -> (GlobalState, a)) -> ChickHandler a
modifyGlobalState f = do
  globRef <- with lGlobRef ask
  liftIO $ atomicModifyIORef' globRef f

modifySessionState :: (SessionState -> (SessionState, a)) -> ChickHandler a
modifySessionState f = do
  m <- view gActiveSessions <$> getGlobalState
  mapKey <- withSession lSession getSessionKey
  case IM.lookup mapKey m of
    Nothing -> do
      let (s, res) = f (SessionState True)
      modifyGlobalState $ insertSession mapKey s
      --logAction hash $ "NEWSESSION " ++ show sessionIdentity
      return res
    Just s -> do
      -- update the timestamp
      let (s', res) = f s
      modifyGlobalState . adjustSession (touchSession . const s') $ mapKey
      return res

getSessionState :: ChickHandler SessionState
getSessionState = modifySessionState (\ s -> (s, s))

insertSession :: Int -> SessionState -> GlobalState -> (GlobalState, ())
insertSession mapKey s gs =
  (
    over gNextSession ((+) 1)
    . over gActiveSessions (IM.insert mapKey s)
    $ gs
  , ()
  )

--logAction :: String -> String -> IO ()
--logAction hash message = infoM rootLoggerName (hash ++ " " ++ message)

getGitCommitHash :: IO String
getGitCommitHash = do
  -- let's not use git to be more portable
  --strip <$> readProcess "git" ["rev-parse", "HEAD"] ""
  let fileName = ".git/refs/heads/master"
  b <- doesFileExist fileName
  if b
  then readFile fileName
  else return "Commit # unavailable"

getSessionKey :: ChickHandler IM.Key
getSessionKey = with lSession $ do
  mkey <- getFromSession keyField
  case mkey of
    Nothing -> do
      key <- liftIO randomIO
      setInSession keyField (T.pack . show $ key)
      --liftIO . logAction $ "No session key found, initializing: " ++ show key
      return key
    Just key -> do
      --liftIO . logAction $ "Session key found: " ++ show key
      return . read . T.unpack $ key
  where
    keyField :: T.Text
    keyField = "key"

hWrite :: Handle -> String -> IO ()
hWrite hi input = do
  putStrLn $ input
  catchError
    (hPutStrLn hi input)
    (\e -> do
      putStrLn $ "CATCH: Write failed with exception: " ++ show e
    )
  return ()

hRead :: Handle -> IO [String]
hRead ho = do
  -- flush stderr if anything is there... (should report to user?)
  -- putStrLn "Trying to flush he"
  -- whileM_ (hReady he) $ hGetLine he >>= putStrLn

  -- putStrLn "Trying to read ho"
  ls <- whileM (hReady ho) $ do
    -- putStrLn "Trying to read a line:"
    l <- catchError
      (hGetLine ho)
      ((\e -> do
        putStrLn $ "CATCH: Read failed with exception: " ++ show e
        return ""
      ))
    putStrLn $ "OUTPUT: " ++ l
    return l
  -- putStrLn "Done"

  return ls
