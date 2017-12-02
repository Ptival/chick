{-# LANGUAGE OverloadedStrings #-}

module Server.Server where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Lens
import           Control.Monad (forever, forM)
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString, append)
import qualified Data.HashMap.Strict as HM (map)
import           Data.IORef
import qualified Data.IntMap as IM
import           Data.String.Utils
import           Data.Time.Format
import           Data.Time.LocalTime
import           Network.Socket
import           Prelude hiding (log)
import           Snap.Core
import           Snap.Http.Server.Config
import           Snap.Snaplet
import           Snap.Snaplet.Session hiding (touchSession)
import           Snap.Snaplet.Session.Backends.CookieSession (initCookieSessionManager)
import           Snap.Snaplet.Session.SessionManager ()
import           Snap.Util.FileServe
import           System.Directory
--import           System.IO
import           System.Log.Formatter
import           System.Log.Handler (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Logger
--import           System.Process

import           Server.Chick
import           Server.ChickHandler
import           Server.Session

{- Configuration -}

configFile :: String
configFile = ".ChickConfig.hs"

sessionTimeoutMinutes :: Int
sessionTimeoutMinutes = 15

disableCaching :: Handler b v a -> Handler b v a
disableCaching h = do
  modifyResponse $ setHeader "Cache-Control" "no-cache, no-store, must-revalidate"
  modifyResponse $ setHeader "Expires" "0"
  h

chickRoutes :: String -> [(ByteString, ChickHandler ())]
chickRoutes directoryToServe =
  --[ ("coqtop", handlerCoqtop)
  --, ("ping", handlerPing) -- we need this because we use HTTP
  [ ("chickGuess", chickGuessHandler)
  , ("/", disableCaching $ serveDirectoryWith myDirConfig directoryToServe)
  ]

{- End of configuration -}

data ChickConfig =
  ChickConfig
  { configLogPath          :: FilePath
  , configDirectoryToServe :: String
  }
  deriving (Read, Show)

serverConfig :: MonadSnap m => ChickConfig -> String -> Config m a
serverConfig (ChickConfig { configLogPath = l }) nowString =
  setStartupHook hook -- figures out which port was used and prints it
  . setPort 0         -- 0 means that unless specified, pick a random port
  . setAccessLog (ConfigFileLog $ prefix ++ "access.log")
  . setErrorLog (ConfigFileLog $ prefix ++ "error.log")
  $ defaultConfig
  where
    prefix = l ++ "/" ++ nowString ++ "-"
    hook dat = do
      port <- socketPort . head $ getStartupSockets dat
      putStrLn $ "Server listening on port: " ++ show port
      --putStrLn $ "On recycle, visit: http://recycle.cs.washington.edu:" ++ show port
      --putStrLn $ "On attu, visit: http://attu.cs.washington.edu:" ++ show port
      --putStrLn $ "Otherwise, visit: http://localhost:" ++ show port

serve :: IO ()
serve = do
  now <- getZonedTime
  let nowString = formatTime defaultTimeLocale "%F-%H-%M-%S" now
  homeDir <- getHomeDirectory
  fileString <- readFile (homeDir ++ "/" ++ configFile)
  let configString = unwords . filter (not <$> startswith "--") $ lines fileString
  let config@(ChickConfig { configLogPath          = l
                          , configDirectoryToServe = serveDir
                          }) = read configString
  handler <- fileHandler
            (l ++ "/" ++ nowString ++ ".log")
            loggingPriority
  let format = simpleLogFormatter "[$time] $msg"
  let fHandler = setFormatter handler format
  updateGlobalLogger rootLoggerName (setLevel loggingPriority . addHandler fHandler)
  serveSnaplet (serverConfig config nowString) (chickSnaplet serveDir)

sessionTimeoutSeconds :: Int
sessionTimeoutSeconds = 60 * sessionTimeoutMinutes

sessionTimeoutMicroseconds :: Int
sessionTimeoutMicroseconds = sessionTimeoutSeconds * 1000 * 1000

loggingPriority :: Priority
loggingPriority = INFO

closeSession :: SessionState -> IO ()
closeSession _s = return ()

cleanStaleSessions :: IORef GlobalState -> IO ()
cleanStaleSessions globRef = forever $ do
  sessionsToClose <- atomicModifyIORef' globRef markAndSweep
  _ <- forM sessionsToClose closeSession
  threadDelay sessionTimeoutMicroseconds
  where
    markAndSweep :: GlobalState -> (GlobalState, [SessionState])
    markAndSweep gs =
      let (alive, stale) = IM.partition isAlive (view gActiveSessions gs) in
      (set gActiveSessions (IM.map markStale alive) gs, IM.elems stale)

newChickGlobalState :: IO (IORef GlobalState)
newChickGlobalState = liftIO $ do
  globRef <- newIORef $ GlobalState 0 IM.empty
  -- spawn a parallel thread to regularly clean up
  _ <- forkIO $ cleanStaleSessions globRef
  return globRef

globRefInit :: IORef GlobalState -> SnapletInit Chick ChickGlobRef
globRefInit globRef =
  makeSnaplet "globRef" "Holds Chick's global state IORef" Nothing $ do
    return globRef

hashInit :: String -> SnapletInit Chick ChickHash
hashInit hash =
  makeSnaplet "hash" "Holds the current git commit hash" Nothing $ do
    return hash

chickSnaplet :: String -> SnapletInit Chick Chick
chickSnaplet directoryToServe = makeSnaplet "Chick" "Chick" Nothing $ do
  hash <- liftIO $ getGitCommitHash
  globRef <- liftIO $ newChickGlobalState
  g <- nestSnaplet "globRef" lGlobRef $ globRefInit globRef
  h <- nestSnaplet "hash" lHash $ hashInit hash
  s <- nestSnaplet "session" lSession cookieSessionManager
  addRoutes $ chickRoutes directoryToServe
  return $ Chick g h s
  where
    cookieSessionManager :: SnapletInit Chick SessionManager
    cookieSessionManager =
      initCookieSessionManager "encryption_key" "chick_session" Nothing Nothing

myDirConfig :: DirectoryConfig (Handler Chick Chick)
myDirConfig =
  defaultDirectoryConfig
  { mimeTypes = HM.map (\ m -> append m "; charset=utf-8") defaultMimeTypes
  , indexFiles = ["index.html"]
  }
