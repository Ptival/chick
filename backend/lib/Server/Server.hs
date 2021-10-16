{-# LANGUAGE OverloadedStrings #-}

module Server.Server where

import Control.Concurrent (forkIO, threadDelay)
import Control.Lens (set, view)
import Control.Monad (forM_, forever)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString, append)
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.IntMap as IM
import Data.String.Utils (startswith)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)
import Network.Socket (socketPort)
import qualified Server.Chick as Chick
import Server.ChickHandler (ChickHandler, chickGuessHandler, getGitCommitHash)
import Server.Session (isAlive, markStale)
import qualified Snap 
import qualified Snap.Snaplet.Session as Snap
import qualified Snap.Snaplet.Session.Backends.CookieSession as Snap
import Snap.Snaplet.Session.SessionManager ()
import qualified Snap.Util.FileServe as Snap
import System.Directory (getHomeDirectory)
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (fileHandler)
import qualified System.Log.Logger as Logger
import Prelude hiding (log)

{- Configuration -}

configFile :: String
configFile = ".ChickConfig.hs"

sessionTimeoutMinutes :: Int
sessionTimeoutMinutes = 15

disableCaching :: Snap.Handler b v a -> Snap.Handler b v a
disableCaching h = do
  Snap.modifyResponse $ Snap.setHeader "Cache-Control" "no-cache, no-store, must-revalidate"
  Snap.modifyResponse $ Snap.setHeader "Expires" "0"
  h

chickRoutes :: String -> [(ByteString, ChickHandler ())]
chickRoutes directoryToServe =
  --[ ("coqtop", handlerCoqtop)
  --, ("ping", handlerPing) -- we need this because we use HTTP
  [ ("chickGuess", chickGuessHandler),
    ("/", disableCaching $ Snap.serveDirectoryWith myDirConfig directoryToServe)
  ]

{- End of configuration -}

data ChickConfig = ChickConfig
  { configLogPath :: FilePath,
    configDirectoryToServe :: String
  }
  deriving (Read, Show)

serverConfig :: Snap.MonadSnap m => ChickConfig -> String -> Snap.Config m a
serverConfig ChickConfig {..} nowString =
  Snap.setStartupHook hook -- figures out which port was used and prints it
    . Snap.setPort 0 -- 0 means that unless specified, pick a random port
    . Snap.setAccessLog (Snap.ConfigFileLog $ prefix ++ "access.log")
    . Snap.setErrorLog (Snap.ConfigFileLog $ prefix ++ "error.log")
    $ Snap.defaultConfig
  where
    prefix = configLogPath ++ "/" ++ nowString ++ "-"
    hook dat = do
      port <- socketPort . head $ Snap.getStartupSockets dat
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
  let config@ChickConfig {..} = read configString
  handler <-
    fileHandler
      (configLogPath ++ "/" ++ nowString ++ ".log")
      loggingPriority
  let format = simpleLogFormatter "[$time] $msg"
  let fHandler = setFormatter handler format
  Logger.updateGlobalLogger Logger.rootLoggerName (Logger.setLevel loggingPriority . Logger.addHandler fHandler)
  Snap.serveSnaplet (serverConfig config nowString) (chickSnaplet configDirectoryToServe)

sessionTimeoutSeconds :: Int
sessionTimeoutSeconds = 60 * sessionTimeoutMinutes

sessionTimeoutMicroseconds :: Int
sessionTimeoutMicroseconds = sessionTimeoutSeconds * 1000 * 1000

loggingPriority :: Logger.Priority
loggingPriority = Logger.INFO

closeSession :: Chick.SessionState -> IO ()
closeSession _s = return ()

cleanStaleSessions :: IORef Chick.GlobalState -> IO ()
cleanStaleSessions globRef = forever $ do
  sessionsToClose <- atomicModifyIORef' globRef markAndSweep
  forM_ sessionsToClose closeSession
  threadDelay sessionTimeoutMicroseconds
  where
    markAndSweep :: Chick.GlobalState -> (Chick.GlobalState, [Chick.SessionState])
    markAndSweep gs =
      let (alive, stale) = IM.partition isAlive (view Chick.gActiveSessions gs)
       in (set Chick.gActiveSessions (IM.map markStale alive) gs, IM.elems stale)

newChickGlobalState :: IO (IORef Chick.GlobalState)
newChickGlobalState = liftIO $ do
  globRef <- newIORef $ Chick.GlobalState 0 IM.empty
  -- spawn a parallel thread to regularly clean up
  _ <- forkIO $ cleanStaleSessions globRef
  return globRef

globRefInit :: IORef Chick.GlobalState -> Snap.SnapletInit Chick.Chick Chick.ChickGlobRef
globRefInit globRef =
  Snap.makeSnaplet "globRef" "Holds Chick's global state IORef" Nothing $
    return globRef

hashInit :: String -> Snap.SnapletInit Chick.Chick Chick.ChickHash
hashInit hash =
  Snap.makeSnaplet "hash" "Holds the current git commit hash" Nothing $
    return hash

chickSnaplet :: String -> Snap.SnapletInit Chick.Chick Chick.Chick
chickSnaplet directoryToServe = Snap.makeSnaplet "Chick" "Chick" Nothing $ do
  hash <- liftIO getGitCommitHash
  globRef <- liftIO newChickGlobalState
  g <- Snap.nestSnaplet "globRef" Chick.lGlobRef $ globRefInit globRef
  h <- Snap.nestSnaplet "hash" Chick.lHash $ hashInit hash
  s <- Snap.nestSnaplet "session" Chick.lSession cookieSessionManager
  Snap.addRoutes $ chickRoutes directoryToServe
  return $ Chick.Chick g h s
  where
    cookieSessionManager :: Snap.SnapletInit Chick.Chick Snap.SessionManager
    cookieSessionManager =
      Snap.initCookieSessionManager "encryption_key" "chick_session" Nothing Nothing

myDirConfig :: Snap.DirectoryConfig (Snap.Handler Chick.Chick Chick.Chick)
myDirConfig =
  Snap.defaultDirectoryConfig
    { Snap.mimeTypes = HM.map (`append` "; charset=utf-8") Snap.defaultMimeTypes,
      Snap.indexFiles = ["index.html"]
    }
