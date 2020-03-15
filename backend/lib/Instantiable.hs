{-# LANGUAGE FlexibleInstances #-}

module Instantiable where

import           Control.Monad.Reader.Class
import           Control.Monad.State.Class

import           Inductive.Inductive
import qualified Term.TypeChecked           as C
import           Term.Variable
import           Typing.GlobalEnvironment
import           Typing.LocalContext

class Instantiable t where
  instantiate ::
    ( MonadReader (GlobalEnvironment ξ (C.Checked Variable)) m
    , MonadState (LocalContext ξ (C.Checked Variable)) m
    ) => t -> m (C.Term Variable)

instance Instantiable (C.Term Variable) where
  instantiate t = return t

instance Instantiable (Constructor α (C.Checked Variable)) where
  instantiate Constructor{..} = _
