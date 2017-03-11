{-# language ConstraintKinds #-}
{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}

module Inductive.Inductive where

import Control.Monad.Except
import Control.Monad.State

import Context
import Inductive.Constructor
import PrettyPrinting
import Term.Raw              as Raw
import Term.Term
import Term.TypeChecked      as TypeChecked
import Text.Printf
import Work

data Inductive ξ =
  Inductive
  { name         :: Variable
  , parameters   :: [(Binder, TypeX ξ)]
  , indices      :: [TypeX ξ]
  , constructors :: [Constructor ξ]
  }

inductiveType ::
  [(Binder, TypeChecked.Type)] -> [TypeChecked.Type] -> TypeChecked.Type ->
  TypeChecked.Type
inductiveType ps is o =
  foldr onParam (foldr onIndex o is) ps
  where
    onIndex :: TypeChecked.Type -> TypeChecked.Type -> TypeChecked.Type
    onIndex i      t = Pi (Type ()) (Binder Nothing)  i t
    onParam :: (Binder, TypeChecked.Type) -> TypeChecked.Type -> TypeChecked.Type
    onParam (b, p) t = Pi (Type ()) b p t

constructorType ::
  Variable -> [(Binder, TypeChecked.Type)] -> [TypeChecked.Type] ->
  TypeChecked.Type
constructorType τ ps is =
  foldr onParam (foldr onIndex (Var (Type ()) τ) is) ps
  where
    onIndex :: TypeChecked.Type -> TypeChecked.Type -> TypeChecked.Type
    onIndex i      t = App (Type ()) t i
    onParam :: (Binder, TypeChecked.Type) -> TypeChecked.Type -> TypeChecked.Type
    onParam (b, p) t = Pi (Type ()) b p t

checkInductive ::
  (MonadState TypeCheckedContext m, MonadError String m) =>
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
        (prettyVariable n) (prettyTerm i) (prettyContext ctxt)
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
    addVariable (cn, constructorType n cps cis)

  return (Inductive n ps' is' cs')

addInductive :: Inductive Raw -> TypeCheckedContext -> Either String TypeCheckedContext
addInductive i c = snd <$> runStateT (checkInductive i) c

-- add inductives from left to right
addInductives :: [Inductive Raw] -> TypeCheckedContext -> Either String TypeCheckedContext
addInductives = flip $ foldM (flip addInductive)
