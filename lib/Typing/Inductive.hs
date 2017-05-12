{-# language ConstraintKinds #-}
{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}

module Typing.Inductive where

import Control.Monad.Except
import Control.Monad.State

import Inductive.Constructor
import Inductive.Inductive
import PrettyPrinting.PrettyPrintable
import PrettyPrinting.PrettyPrintableUnannotated
import Term.Binder
import Term.Raw                       as Raw
import Term.Term
import Term.TypeChecked               as TypeChecked
import Term.Variable
import Text.Printf
import Typing.GlobalEnvironment
import Typing.LocalContext
import Work

addVariable ::
  MonadState (TypeCheckedLocalContext Variable) m =>
  (Variable, TypeChecked.Type Variable) -> m ()
addVariable (v, τ) = modify $ addLocalAssum (Binder (Just v), τ)

addBinder :: MonadState (TypeCheckedLocalContext ν) m => (Binder ν, TypeChecked.Type ν) -> m ()
addBinder = modify . addLocalAssum

checkConstructor ::
  (MonadState (TypeCheckedLocalContext Variable) m, MonadError String m) =>
  [(Binder Variable, TypeChecked.Type Variable)] -> [TypeChecked.Type Variable] -> Constructor Raw Variable ->
  m (Constructor (Checked Variable) Variable)
checkConstructor ps is (Constructor n args inds) = do

  -- constructors should specify all indices
  let nbIndices = length is
  when (length inds /= nbIndices)
    $ throwError
    $ printf "Constructor %s should have %s indices"
    (prettyStr n) (show nbIndices)

  -- add all parameters as local variables
  forM_ ps addBinder

  -- check the arguments
  args' <- forM args $ \ (b, τ) -> do
    ctxt <- get
    case tc (checkF ctxt τ Type id) of
      Left  l ->
        throwError $
        printf "In constructor %s: could not typecheck argument %s\nFail: %s"
        (prettyStr n) (prettyStrU τ) (show l)
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
        (prettyStr n) (prettyStrU t) (prettyStrU τ) (show l)
      Right r -> return r

  return $ Constructor n args' inds'

checkInductive ::
  (MonadState (TypeCheckedLocalContext Variable) m, MonadError String m) =>
  Inductive Raw Variable -> m (Inductive (Checked Variable) Variable)
checkInductive (Inductive n ps is cs) = do

  -- checking the parameters
  ps' <- forM ps $ \ (b, p) -> do
    ctxt <- get
    case tc (checkF ctxt p Type id) of
      Left  _ ->
        throwError $
        printf "In inductive %s: could not typecheck parameter %s"
        (prettyStr n) (prettyStrU p)
      Right r -> return (b, r)

  -- checking the indices
  is' <- forM is $ \ i -> do
    ctxt <- get
    case tc (checkF ctxt i Type id) of
      Left  _ ->
        throwError $
        printf "In inductive %s: could not typecheck index type %s in context %s"
        (prettyStr n) (prettyStrU i) (prettyStrU ctxt)
      Right r -> return r

  -- adding the inductive type to the global environment, so that constructors
  -- may refer to it
  addVariable (n, inductiveType ps' is' Type)

  ctxt <- get
  -- checking all constructors (they should not change the global context)
  cs' <- forM cs $ \ c -> do
    (c', _) <- runStateT (checkConstructor ps' is' c) ctxt
    return c'

  -- now adding all the constructors to the global environment
  forM_ cs' $ \ (Constructor cn cps cis) ->
    addVariable (cn, constructorTypeChecked n ps' cps cis)

  return (Inductive n ps' is' cs')

addInductive ::
  Inductive Raw Variable -> GlobalEnvironment (Checked Variable) Variable ->
  Either String (GlobalEnvironment (Checked Variable) Variable)
addInductive i ge =
  case runStateT (checkInductive i) (toLocalContext ge) of
  Left  l       -> Left l
  Right (i', _) -> Right $ addGlobalInd i' ge

-- add inductives from left to right
addInductives ::
  [Inductive Raw Variable] -> GlobalEnvironment (Checked Variable) Variable ->
  Either String (GlobalEnvironment (Checked Variable) Variable)
addInductives = flip $ foldM (flip addInductive)
