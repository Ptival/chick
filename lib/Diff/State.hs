{-# LANGUAGE TemplateHaskell #-}

module Diff.State
  ( State (..)
  , context
  , δcontext
  , environment
  , δenvironment
  ) where

import           Control.Lens.TH

import qualified Diff.GlobalEnvironment as DGE
import qualified Diff.LocalContext as DLC
import qualified Term.Raw as Raw
import           Term.Variable
import           Typing.GlobalEnvironment
import           Typing.LocalContext

data State = State
  { _context      :: LocalContext      Raw.Raw Variable
  , _δcontext     :: DLC.Diff          Raw.Raw
  , _environment  :: GlobalEnvironment Raw.Raw Variable
  , _δenvironment :: DGE.Diff          Raw.Raw
  }

makeLenses ''State
