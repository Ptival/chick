{-# LANGUAGE OverloadedStrings #-}

module Server.ChickHandler where

import           Control.Lens                              ( over, view )
import           Control.Monad.Except                      ( catchError, liftIO )
import           Control.Monad.Loops                       ( whileM )
import           Control.Monad.Representable.Reader        ( ask )
import           Data.Aeson                                ( FromJSON, decode )
import           Polysemy                                  ( runM )
import           Polysemy.Trace                            ( traceToIO )
-- import qualified Data.ByteString.UTF8 as BSU
-- import qualified Data.ByteString.Lazy as BSL
import           Data.IORef                                ( atomicModifyIORef', readIORef )
import qualified Data.IntMap                               as IM
import qualified Data.Text                                 as T
import           GHC.Generics
import           Prelude                                   hiding ( init )
import qualified Snap.Core                                 as Snap
import           Snap.Extras.JSON                          ( writeJSON )
import           Snap.Snaplet                              ( Handler, with )
import           Snap.Snaplet.Session                      hiding ( touchSession )
import           Snap.Snaplet.Session.SessionManager       (  )
import           System.Directory                          ( doesFileExist )
import qualified System.IO                                 as IO
import           System.Process                            ( ProcessHandle, runInteractiveCommand )
import           System.Random                             ( randomIO )
import           Text.Megaparsec                           ( parseMaybe )
import           Text.Printf                               ( printf )

import qualified Diff.Guess.Script                         as ΔGS
import qualified Language                                  ( Language(Chick) )
import           Parsing.Script
import           PrettyPrinting.PrettyPrintableUnannotated
import           Repair.Benchmark
import           Server.Chick
import           Server.Session

type ChickHandler a = Handler Chick Chick a

data GuessInput = GuessInput
  { before :: String
  , after  :: String
  }
  deriving (Generic)

instance FromJSON GuessInput where

chickGuessHandler :: ChickHandler ()
chickGuessHandler = do
  r <- Snap.getRequest
  case Snap.rqContentLength r of
    Nothing -> return ()
    Just bodyLength -> do
      body <- Snap.readRequestBody bodyLength
      res <- case decode body of
        Nothing -> return $ Just "decode body failed"
        Just GuessInput{..} ->
          case (parseMaybe scriptP before, parseMaybe scriptP after) of
            (Just bef, Just aft) -> do
              liftIO $ do
                putStrLn "BEFORE:"
                putStrLn $ prettyStrU @'Language.Chick bef
                putStrLn "AFTER:"
                putStrLn $ prettyStrU @'Language.Chick aft
              δ <- liftIO $ runM $ traceToIO $ ΔGS.guess bef aft
              liftIO $ putStrLn $ printf "GUESSED:\n%s" (show δ)
              liftIO (runM . traceToIO $ repairScript bef δ) >>= \case
                Left e -> return $ Just e
                Right patched ->
                  return $ Just $ prettyStrU @'Language.Chick patched
                  -- ++ "\n(*"
                  -- ++ show δ
                  -- ++ "*)"
            (Nothing, Just _)  ->
              return . Just $ "Parsing before failed"
            (Just _,  Nothing) ->
              return . Just $ "Parsing after  failed"
            (Nothing, Nothing) ->
              return . Just $ "Parsing before and after failed"
      writeJSON res

runCommandWithoutBuffering :: String -> IO (IO.Handle, IO.Handle, IO.Handle, ProcessHandle)
runCommandWithoutBuffering cmd = do
  (hi, ho, he, ph) <- runInteractiveCommand cmd
  IO.hSetBuffering hi IO.NoBuffering
  IO.hSetBuffering ho IO.NoBuffering
  IO.hSetBuffering he IO.NoBuffering
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
    over gNextSession (+ 1)
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
    Just key ->
      --liftIO . logAction $ "Session key found: " ++ show key
      return . read . T.unpack $ key
  where
    keyField :: T.Text
    keyField = "key"

hWrite :: IO.Handle -> String -> IO ()
hWrite hi input = do
  putStrLn input
  catchError
    (IO.hPutStrLn hi input)
    (\e ->
      putStrLn $ "CATCH: Write failed with exception: " ++ show e
    )
  return ()

hRead :: IO.Handle -> IO [String]
hRead ho =
  -- flush stderr if anything is there... (should report to user?)
  -- putStrLn "Trying to flush he"
  -- whileM_ (hReady he) $ hGetLine he >>= putStrLn

  -- putStrLn "Trying to read ho"
  whileM (IO.hReady ho) $ do
    -- putStrLn "Trying to read a line:"
    l <- catchError
      (IO.hGetLine ho)
      (\e -> do
        putStrLn $ "CATCH: Read failed with exception: " ++ show e
        return ""
      )
    putStrLn $ "OUTPUT: " ++ l
    return l
  -- putStrLn "Done"
