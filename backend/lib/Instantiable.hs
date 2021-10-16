{-# LANGUAGE FlexibleInstances #-}

module Instantiable where

import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Inductive.Inductive (Constructor (..))
import qualified Term.TypeChecked as C
import Term.Variable (Variable)
import Typing.GlobalEnvironment (GlobalEnvironment)
import Typing.LocalContext (LocalContext)

class Instantiable t where
  instantiate ::
    ( MonadReader (GlobalEnvironment ξ (C.Checked Variable)) m,
      MonadState (LocalContext ξ (C.Checked Variable)) m
    ) =>
    t ->
    m (C.Term Variable)

instance Instantiable (C.Term Variable) where
  instantiate t = return t

instance Instantiable (Constructor α (C.Checked Variable)) where
  instantiate Constructor {..} = _
