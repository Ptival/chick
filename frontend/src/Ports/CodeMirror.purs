module Ports.CodeMirror where

import Prelude
import Control.Monad.Eff.Uncurried as EU
import Ports.CodeMirror.Configuration as CFG
import Ports.CodeMirror.TextMarkerOptions as TMO
import Ports.CodeMirror.SetSelectionOptions as SSO
import CodeMirror.Position (Position)
import Control.Monad.Eff (Eff)
import DOM.HTML.Types (HTMLElement)
import Data.Foreign (Foreign)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Data.Options (opt, options, (:=))

type KeyMap = {}

data CodeMirror
data Doc
data TextMarker

foreign import _addKeyMap :: ∀ e. EU.EffFn3 e CodeMirror Foreign Boolean Unit
addKeyMap :: ∀ e. CodeMirror -> String -> Boolean -> (Unit -> Eff e Unit) -> Eff e Unit
addKeyMap cm key b k = EU.runEffFn3 _addKeyMap cm (options (opt key := EU.mkEffFn1 k)) b

foreign import _clearTextMarker :: ∀ e. EU.EffFn1 e TextMarker Unit
clearTextMarker :: ∀ e. TextMarker -> Eff e Unit
clearTextMarker = EU.runEffFn1 _clearTextMarker

foreign import _codeMirror :: ∀ e. EU.EffFn2 e HTMLElement CFG.RawConfiguration CodeMirror
codeMirror :: ∀ e. HTMLElement -> CFG.Configuration -> Eff e CodeMirror
codeMirror e c = EU.runEffFn2 _codeMirror e (CFG.toRaw c)

data EndSelector
  = From
  | To
  | Head
  | Anchor

foreign import _getCursor :: ∀ e. EU.EffFn2 e Doc (Nullable String) Position
getCursor :: ∀ e. Doc -> Maybe EndSelector -> Eff e Position
getCursor d s = EU.runEffFn2 _getCursor d (toNullable $ stringOfEndSelector <$> s)
  where
    stringOfEndSelector = case _ of
      From   -> "from"
      To     -> "to"
      Head   -> "head"
      Anchor -> "anchor"

foreign import _getDoc :: ∀ e. EU.EffFn1 e CodeMirror Doc
getDoc :: ∀ e. CodeMirror -> Eff e Doc
getDoc = EU.runEffFn1 _getDoc

foreign import _getRange :: ∀ e. EU.EffFn4 e Doc Position Position (Nullable String) String
getRange :: ∀ e. Doc -> Position -> Position -> Maybe String -> Eff e String
getRange d f t m = EU.runEffFn4 _getRange d f t (toNullable m)

foreign import _getValue :: ∀ e. EU.EffFn2 e Doc (Nullable String) String
getValue :: ∀ e. Doc -> Maybe String -> Eff e String
getValue d m = EU.runEffFn2 _getValue d (toNullable m)

foreign import _hasFocus :: ∀ e. EU.EffFn1 e CodeMirror Boolean
hasFocus :: ∀ e. CodeMirror -> Eff e Boolean
hasFocus = EU.runEffFn1 _hasFocus

foreign import _lineCount :: ∀ e. EU.EffFn1 e Doc Int
lineCount :: ∀ e. Doc -> Eff e Int
lineCount = EU.runEffFn1 _lineCount

foreign import _markText ::
  ∀ e. EU.EffFn4 e Doc Position Position (Nullable TMO.RawTextMarkerOptions) TextMarker
markText :: ∀ e. Doc -> Position -> Position -> Maybe TMO.TextMarkerOptions -> Eff e TextMarker
markText d p1 p2 m = EU.runEffFn4 _markText d p1 p2 (toNullable $ TMO.toRaw <$> m)

foreign import _scrollIntoView :: ∀ e. EU.EffFn1 e CodeMirror Unit
scrollIntoView :: ∀ e. CodeMirror -> Eff e Unit
scrollIntoView = EU.runEffFn1 _scrollIntoView

foreign import _scrollIntoViewPosition :: ∀ e. EU.EffFn2 e CodeMirror Position Unit
scrollIntoViewPosition :: ∀ e. CodeMirror -> Position -> Eff e Unit
scrollIntoViewPosition = EU.runEffFn2 _scrollIntoViewPosition

scrollIntoViewLastLine :: ∀ e. CodeMirror -> Eff e Unit
scrollIntoViewLastLine cm = do
  d <- getDoc cm
  c <- lineCount d
  scrollIntoViewPosition cm { line : c - 1, ch : 0 }

foreign import _setCursor :: ∀ e. EU.EffFn3 e Doc Position (Nullable SSO.RawSetSelectionOptions) Unit
setCursor :: ∀ e. Doc -> Position -> Maybe SSO.SetSelectionOptions -> Eff e Unit
setCursor d p m = EU.runEffFn3 _setCursor d p (toNullable $ SSO.toRaw <$> m)

foreign import _setSize :: ∀ e. EU.EffFn3 e CodeMirror String String Unit
setSize :: ∀ e. CodeMirror -> String -> String -> Eff e Unit
setSize = EU.runEffFn3 _setSize

foreign import _setValue :: ∀ e. EU.EffFn2 e Doc String Unit
setValue :: ∀ e. Doc -> String -> Eff e Unit
setValue d v = EU.runEffFn2 _setValue d v

-- | All the `on` stuff is down here

type ChangeObj =
  { from    :: Position
  , to      :: Position
  , text    :: Array String
  , removed :: String
  , origin  :: String
  }

type WithInstance  r = ( instance  :: CodeMirror | r)
type WithChangeObj r = ( changeObj :: ChangeObj  | r)

type CodeMirrorChange = Record (WithInstance (WithChangeObj ()))
foreign import _onCodeMirrorChange ::
  ∀ e. EU.EffFn2 e CodeMirror (CodeMirrorChange-> Eff e Unit) Unit
onCodeMirrorChange :: ∀ e. CodeMirror -> (CodeMirrorChange-> Eff e Unit) -> Eff e Unit
onCodeMirrorChange cm h = EU.runEffFn2 _onCodeMirrorChange cm h

type CodeMirrorCursorActivity = Record (WithInstance ())
foreign import _onCodeMirrorCursorActivity ::
  ∀ e. EU.EffFn2 e CodeMirror (CodeMirrorCursorActivity -> Eff e Unit) Unit
onCodeMirrorCursorActivity :: ∀ e. CodeMirror -> (CodeMirrorCursorActivity -> Eff e Unit) -> Eff e Unit
onCodeMirrorCursorActivity cm h = EU.runEffFn2 _onCodeMirrorCursorActivity cm h
