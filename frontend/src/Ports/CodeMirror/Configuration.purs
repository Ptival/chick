module Ports.CodeMirror.Configuration where

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)

type ConfigurationF f =
  { autofocus      :: f Boolean
  , lineNumbers    :: f Boolean
  , lineSeparator  :: f String
  , lineWrapping   :: f Boolean
  , mode           :: f String
  , value          :: f String
  , viewportMargin :: f Number
  }

type RawConfiguration = ConfigurationF Nullable
type Configuration    = ConfigurationF Maybe

toRaw :: Configuration -> RawConfiguration
toRaw c =
  { autofocus      : toNullable c.autofocus
  , lineNumbers    : toNullable c.lineNumbers
  , lineSeparator  : toNullable c.lineSeparator
  , lineWrapping   : toNullable c.lineWrapping
  , mode           : toNullable c.mode
  , value          : toNullable c.value
  , viewportMargin : toNullable c.viewportMargin
  }

def :: Configuration
def =
  { autofocus      : Nothing
  , lineNumbers    : Nothing
  , lineSeparator  : Nothing
  , lineWrapping   : Nothing
  , mode           : Nothing
  , value          : Nothing
  , viewportMargin : Nothing
  }
