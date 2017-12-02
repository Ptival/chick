module Ports.CodeMirror.SetSelectionOptions where

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)

type SetSelectionOptionsF f =
  { bias   :: f Int
  , origin :: f String
  , scroll :: f Boolean
  }

type RawSetSelectionOptions = SetSelectionOptionsF Nullable
type SetSelectionOptions    = SetSelectionOptionsF Maybe

toRaw :: SetSelectionOptions -> RawSetSelectionOptions
toRaw o =
  { bias   : toNullable o.bias
  , origin : toNullable o.origin
  , scroll : toNullable o.scroll
  }

def :: SetSelectionOptions
def =
  { bias   : Nothing
  , origin : Nothing
  , scroll : Nothing
  }
