{-# LANGUAGE TemplateHaskell #-}

module Diff.State
  ( State (..)
  , context
  , contextDiff
  ) where

import           Control.Lens.TH

import qualified Diff.LocalContext as DLC
import qualified Term.Raw as Raw
import           Term.Variable
import           Typing.LocalContext

data State = State
  { _context     :: LocalContext Raw.Raw Variable
  , _contextDiff :: DLC.Diff Raw.Raw
  }

makeLenses ''State
