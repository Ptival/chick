{-# language FlexibleContexts #-}

module Inductive.Constructor where

import Control.Monad.Except
import Control.Monad.State

import Context
import PrettyPrinting
import Term.Raw            as Raw
import Term.Term
import Term.TypeChecked    as TypeChecked
import Text.Printf
import Work

data Constructor ξ =
  Constructor
  { name      :: Variable
  , arguments :: [(Binder, TypeX ξ)]
  , indices   :: [TypeX ξ]
  }

addVariable :: MonadState TypeCheckedContext m => (Variable, TypeChecked.Type) -> m ()
addVariable x = modify $ (:) x

addBinder :: MonadState TypeCheckedContext m => (Binder, TypeChecked.Type) -> m ()
addBinder x = modify $ (+:) x

checkConstructor ::
  (MonadState TypeCheckedContext m, MonadError String m) =>
  [(Binder, TypeChecked.Type)] -> [TypeChecked.Type] -> Constructor Raw ->
  m (Constructor TypeChecked)
checkConstructor ps is (Constructor n args inds) = do

  -- constructors should specify all indices
  let nbIndices = length is
  when (length inds /= nbIndices)
    $ throwError
    $ printf "Constructor %s should have %s indices"
    (prettyVariable n) (show nbIndices)

  -- add all parameters as local variables
  forM_ ps addBinder

  -- check the arguments
  args' <- forM args $ \ (b, τ) -> do
    ctxt <- get
    case tc (checkF ctxt τ (Type () :: Raw.Type) id) of
      Left  l ->
        throwError $
        printf "In constructor %s: could not typecheck argument %s\nFail: %s"
        (prettyVariable n) (prettyTerm τ) (show l)
      Right r -> do
        addBinder (b, r)
        return (b, r)

  -- check the indices
  ctxt <- get
  inds' <- forM (zip is inds) $ \ (τ, t) -> do
    case tc (checkF ctxt t τ id) of
      Left  l ->
        throwError $
        printf "In constructor %s: could not typecheck index %s at type %s\nFail: %s"
        (prettyVariable n) (prettyTerm t) (prettyTerm τ) (show l)
      Right r -> return r

  return $ Constructor n args' inds'
