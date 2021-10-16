module Server.Session where

import Control.Lens (over, set, view)
import Data.IntMap (adjust)
import Server.Chick
  ( GlobalState,
    SessionState,
    gActiveSessions,
    sAlive,
  )

isAlive :: SessionState -> Bool
isAlive = view sAlive

markStale :: SessionState -> SessionState
markStale = set sAlive False

touchSession :: SessionState -> SessionState
touchSession = set sAlive True

adjustSession ::
  (SessionState -> SessionState) -> Int -> GlobalState -> (GlobalState, ())
adjustSession f mapKey gs =
  (over gActiveSessions (adjust f mapKey) gs, ())
