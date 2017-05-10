{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Typing.GlobalEnvironment where

--import           Control.Applicative
import           Control.Monad
import           Data.List

import           Inductive.Constructor
import           Inductive.Inductive
import           Typing.LocalContext
import           Typing.LocalDeclaration
import           Typing.GlobalDeclaration
import           Term.Binder
import           Term.Term
import           Term.TypeChecked as TypeChecked
import           Term.Variable
import qualified Typing.LocalContext as LocalContext

newtype GlobalEnvironment ξ ν
  = GlobalEnvironment
    { unGlobalEnvironment :: [GlobalDeclaration ξ ν]
    }

addGlobalAssum :: (Binder ν, TypeX ξ ν) -> GlobalEnvironment ξ ν -> GlobalEnvironment ξ ν
addGlobalAssum (Binder Nothing , _) γ = γ
addGlobalAssum (Binder (Just v), τ) (GlobalEnvironment γ) =
  GlobalEnvironment ((GlobalAssum v τ) : γ)

-- type TypeCheckedGlobalEnvironment = GlobalEnvironment TypeChecked

{-|
An inductive brings into scope:

- the inductive type it defines

- inductive definitions for its constructors

TODO: - recursors
-}
unwrapInductive ::
  Inductive (TypeChecked Variable) Variable -> [GlobalDeclaration (TypeChecked Variable) Variable]
unwrapInductive (Inductive v ps is cs) =
  GlobalAssum v (inductiveType ps is (Type ())) : map constructorAssumption cs
  where
    constructorAssumption (Constructor cv cps cis) =
      GlobalAssum cv (constructorTypeChecked v ps cps cis)

{-| 'lookupDecl v ge' tries to find a top-level declaration named 'v' in
'ge'.  It will find either a global assumption, a global declaration,
or an inductive type, but will not find constructors of an inductive
type.
-}
lookupDecl ::
  Eq ν =>
  ν -> GlobalEnvironment (TypeChecked ν) ν -> Maybe (GlobalDeclaration (TypeChecked ν) ν)
lookupDecl target = find found . unGlobalEnvironment
  where
    found = \case
      GlobalAssum v _   | v                          == target -> True
      GlobalDef   v _ _ | v                          == target -> True
      GlobalInd   i     | Inductive.Inductive.name i == target -> True
      _ -> False

{-| 'lookupType v ge' tries to find a construct named 'v' in 'ge.  It
will find either a global assumption, a global declaration, an
inductive type, or a constructor, and will return its type.
-}
lookupType ::
  Variable -> GlobalEnvironment (TypeChecked Variable) Variable ->
  Maybe (TypeChecked.Type Variable)
lookupType target = go . unGlobalEnvironment
  where
    go = msum . map found
    found = \case
      GlobalAssum v τ   | v == target -> Just τ
      GlobalDef   v _ τ | v == target -> Just τ
      GlobalInd   i -> go (unwrapInductive i)
      _ -> Nothing

toLocalContext ::
  GlobalEnvironment (TypeChecked Variable) Variable ->
  LocalContext.LocalContext (TypeChecked Variable) Variable
toLocalContext =
  LocalContext . foldr (\ g acc -> localize g ++ acc) [] . unGlobalEnvironment
  where
    localize ::
      GlobalDeclaration (TypeChecked Variable) Variable ->
      [LocalDeclaration (TypeChecked Variable) Variable]
    localize = \case
      GlobalAssum v τ   -> [LocalAssum v τ]
      GlobalDef   v t τ -> [LocalDef v t τ]
      GlobalInd   i     -> concatMap localize (unwrapInductive i)

envInductives :: GlobalEnvironment ξ ν -> [Inductive ξ ν]
envInductives = foldr addInductive [] . unGlobalEnvironment
  where
    addInductive :: GlobalDeclaration ξ ν -> [Inductive ξ ν] -> [Inductive ξ ν]
    addInductive (GlobalInd i) a = i : a
    addInductive _             a = a

{-| 'isInductive' checks whether the type is an instance of some
inductive type in the global environment.  For instance, given '(List
T)', it should return the inductive 'Some (Inductive for List)'.
Given '(A -> List T)', it should return 'None'.
-}
isInductive ::
  Eq ν =>
  GlobalEnvironment (TypeChecked ν) ν -> TypeChecked.Type ν ->
  Maybe (Inductive (TypeChecked ν) ν)
isInductive ge = go
  where
    go τ =
      case τ of
        App _ l _ -> go l
        Var v   ->
          case lookupDecl v ge of
            Just (GlobalInd i) -> Just i
            _ -> Nothing
        _ -> Nothing
