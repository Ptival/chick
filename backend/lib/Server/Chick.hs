{-# LANGUAGE TemplateHaskell #-}

module Server.Chick where

import Control.Lens (makeLenses)
import Data.IORef (IORef)
import Data.IntMap (IntMap)
import Snap (Snaplet)
import Snap.Snaplet.Session (SessionManager)
import System.IO (Handle)
import System.Process (ProcessHandle)

type Handles = (Handle, Handle, Handle, ProcessHandle)

newtype SessionState = SessionState
  { _sAlive :: Bool -- True while the session is alive
  }

makeLenses ''SessionState

-- Global state must be used in thread-safe way
data GlobalState = GlobalState
  { _gNextSession :: Int, -- number to assign to the next session
    _gActiveSessions :: IntMap SessionState
  }

makeLenses ''GlobalState

type ChickGlobRef = (IORef GlobalState)

type ChickHash = String

type ChickSession = SessionManager

-- Each thread gets a separate copy of this, fields must be read-only
data Chick = Chick
  { _lGlobRef :: Snaplet ChickGlobRef,
    _lHash :: Snaplet ChickHash,
    _lSession :: Snaplet ChickSession
  }

-- Fields are lenses to separate concerns, use "Handler Chick <Lens> a"
makeLenses ''Chick
