module Ports.CodeMirror.TextMarkerOptions where

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)

type TextMarkerOptionsF f =
  { css       :: f String
  , className :: f String
  , readOnly  :: f Boolean
  }

type RawTextMarkerOptions = TextMarkerOptionsF Nullable
type TextMarkerOptions    = TextMarkerOptionsF Maybe

toRaw :: TextMarkerOptions -> RawTextMarkerOptions
toRaw o =
  { css       : toNullable o.css
  , className : toNullable o.className
  , readOnly  : toNullable o.readOnly
  }

def :: TextMarkerOptions
def =
  { css       : Nothing
  , className : Nothing
  , readOnly  : Nothing
  }
