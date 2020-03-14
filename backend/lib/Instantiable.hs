{-# LANGUAGE FlexibleInstances #-}

module Instantiable where

import Control.Monad.Reader.Class
import Control.Monad.State.Class

import Inductive.Constructor
--import Term.Term
import Term.TypeChecked         as TypeChecked
import Typing.GlobalEnvironment
import Typing.LocalContext

class Instantiable t where
  instantiate ::
    ( MonadReader (GlobalEnvironment TypeChecked) m
    , MonadState (LocalContext TypeChecked) m
    ) => t -> m TypeChecked.Term

instance Instantiable TypeChecked.Term where
  instantiate t = return t

instance Instantiable (Constructor TypeChecked) where
  instantiate (Constructor n as is) = _
