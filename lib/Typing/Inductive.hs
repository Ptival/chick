{-# language ConstraintKinds #-}
{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}

module Typing.Inductive where

import Control.Monad.Except
import Control.Monad.State

import Inductive.Constructor
import Inductive.Inductive
import PrettyPrinting.LocalContext
import PrettyPrinting.Term
import PrettyPrinting.Variable
import Term.Raw                    as Raw
import Term.Term
import Term.TypeChecked            as TypeChecked
import Text.Printf
import Typing.GlobalEnvironment
import Typing.LocalContext
import Work

addVariable ::
  MonadState TypeCheckedLocalContext m =>
  (Variable, TypeChecked.Type) -> m ()
addVariable (v, τ) = modify $ addLocalAssum (Binder (Just v), τ)

addBinder :: MonadState TypeCheckedLocalContext m => (Binder, TypeChecked.Type) -> m ()
addBinder = modify . addLocalAssum

checkConstructor ::
  (MonadState TypeCheckedLocalContext m, MonadError String m) =>
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

checkInductive ::
  (MonadState TypeCheckedLocalContext m, MonadError String m) =>
  Inductive Raw -> m (Inductive TypeChecked)
checkInductive (Inductive n ps is cs) = do

  -- checking the parameters
  ps' <- forM ps $ \ (b, p) -> do
    ctxt <- get
    case tc (checkF ctxt p (Type () :: Raw.Type) id) of
      Left  _ ->
        throwError $
        printf "In inductive %s: could not typecheck parameter %s"
        (prettyVariable n) (prettyTerm p)
      Right r -> return (b, r)

  -- checking the indices
  is' <- forM is $ \ i -> do
    ctxt <- get
    case tc (checkF ctxt i (Type () :: Raw.Type) id) of
      Left  _ ->
        throwError $
        printf "In inductive %s: could not typecheck index type %s in context %s"
        (prettyVariable n) (prettyTerm i) (prettyLocalContext ctxt)
      Right r -> return r

  -- adding the inductive type to the global environment, so that constructors
  -- may refer to it
  addVariable (n, inductiveType ps' is' (Type ()))

  ctxt <- get
  -- checking all constructors (they should not change the global context)
  cs' <- forM cs $ \ c -> do
    (c', _) <- runStateT (checkConstructor ps' is' c) ctxt
    return c'

  -- now adding all the constructors to the global environment
  forM_ cs' $ \ (Constructor cn cps cis) ->
    addVariable (cn, constructorType n ps' cps cis)

  return (Inductive n ps' is' cs')

addInductive ::
  Inductive Raw -> GlobalEnvironment TypeChecked ->
  Either String (GlobalEnvironment TypeChecked)
addInductive i ge =
  case runStateT (checkInductive i) (toLocalContext ge) of
  Left  l       -> Left l
  Right (i', _) -> Right $ GlobalInd i' : ge

-- add inductives from left to right
addInductives ::
  [Inductive Raw] -> GlobalEnvironment TypeChecked ->
  Either String (GlobalEnvironment TypeChecked)
addInductives = flip $ foldM (flip addInductive)
