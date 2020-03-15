{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Typing.GlobalEnvironment
  ( GlobalEnvironment(..)
  , addConstructor
  , addGlobalAssum
  , addGlobalDef
  , addGlobalInd
  , addInductiveType
  , isInductive
  , lookupInductiveByConstructors
  , lookupInductiveByName
  , lookupRawType
  , toLocalContext
  ) where

--import           Control.Applicative
import           Control.Monad
import           Data.List

import qualified Inductive.Inductive      as I
import           Term.Binder
import qualified Term.Raw                 as Raw
import           Term.Term
import qualified Term.TypeChecked         as C
import           Typing.GlobalDeclaration
import qualified Typing.LocalContext      as LC
import           Typing.LocalDeclaration

newtype GlobalEnvironment ξ ν
  = GlobalEnvironment
    { unGlobalEnvironment :: [GlobalDeclaration ξ ν]
    }

addGlobalAssum :: (Binder ν, TypeX ξ ν) -> GlobalEnvironment ξ ν -> GlobalEnvironment ξ ν
addGlobalAssum (Binder Nothing,  _) γ = γ
addGlobalAssum (Binder (Just v), τ) (GlobalEnvironment γ) =
  GlobalEnvironment (GlobalAssum v τ : γ)

addGlobalDef :: (Binder ν, TypeX ξ ν, TermX ξ ν) -> GlobalEnvironment ξ ν -> GlobalEnvironment ξ ν
addGlobalDef (Binder Nothing,  _, _) γ = γ
addGlobalDef (Binder (Just v), τ, t) (GlobalEnvironment γ) =
  GlobalEnvironment (GlobalDef v τ t : γ)

addGlobalInd :: I.Inductive ξ ν -> GlobalEnvironment ξ ν -> GlobalEnvironment ξ ν
addGlobalInd i (GlobalEnvironment γ) = GlobalEnvironment (GlobalInd i : γ)

unwrapRawInductive :: I.Inductive Raw.Raw Variable -> [GlobalDeclaration Raw.Raw Variable]
unwrapRawInductive ind@(I.Inductive indName _ _ _ cs) =
  GlobalAssum indName (I.inductiveRawType ind) : map constructorAssumption cs
  where
    constructorAssumption cons@(I.Constructor _ cv _ _) =
      GlobalAssum cv (I.constructorRawType True cons)

{-|
An inductive brings into scope:

- the inductive type it defines

- inductive definitions for its constructors

TODO: - recursors
-}
unwrapInductive ::
  I.Inductive (C.Checked Variable) Variable -> [GlobalDeclaration (C.Checked Variable) Variable]
unwrapInductive ind@(I.Inductive n _ _ _ cs) =
  GlobalAssum n (I.inductiveType ind) : map constructorAssumption cs
  where
    constructorAssumption cons@(I.Constructor _ cv _ _) =
      GlobalAssum cv (I.constructorCheckedType True cons)

{-| 'lookupDecl v ge' tries to find a top-level declaration named 'v' in
'ge'.  It will find either a global assumption, a global declaration,
or an inductive type, but will not find constructors of an inductive
type.
-}
lookupDecl ::
  Eq ν =>
  ν -> GlobalEnvironment (C.Checked ν) ν -> Maybe (GlobalDeclaration (C.Checked ν) ν)
lookupDecl target = find found . unGlobalEnvironment
  where
    found = \case
      GlobalAssum v _   | v                 == target -> True
      GlobalDef   v _ _ | v                 == target -> True
      GlobalInd   i     | I.inductiveName i == target -> True
      _ -> False

lookupInductiveByName ::
  Eq ν => ν -> GlobalEnvironment α ν -> Maybe (I.Inductive α ν)
lookupInductiveByName target =
  find found . concatMap selectInductives . unGlobalEnvironment
  where
    selectInductives = \case
      GlobalInd i -> [i]
      _ -> []
    found i = I.inductiveName i == target

lookupInductiveByConstructors ::
  Eq ν => ν -> GlobalEnvironment α ν -> Maybe (I.Inductive α ν)
lookupInductiveByConstructors target =
  find foundInductive . concatMap selectInductives . unGlobalEnvironment
  where
    selectInductives = \case
      GlobalInd i -> [i]
      _ -> []
    foundInductive   i = any foundConstructor (I.inductiveConstructors i)
    foundConstructor c = I.constructorName c == target

lookupRawType ::
  Variable -> GlobalEnvironment Raw.Raw Variable ->
  Maybe (TypeX Raw.Raw Variable)
lookupRawType target = go . unGlobalEnvironment
  where
    go = msum . map found
    found = \case
      GlobalAssum v τ   | v == target -> Just τ
      GlobalDef   v τ _ | v == target -> Just τ
      GlobalInd   i -> go (unwrapRawInductive i)
      _ -> Nothing

{-| `lookupType v ge` tries to find a construct named `v` in `ge`. It will find
either a global assumption, a global declaration, an inductive type, or a
constructor, and will return its type. |-}

-- lookupType ::
--   Variable -> GlobalEnvironment (Checked Variable) Variable ->
--   Maybe (TypeChecked.Type Variable)
-- lookupType target = go . unGlobalEnvironment
--   where
--     go = msum . map found
--     found = \case
--       GlobalAssum v τ   | v == target -> Just τ
--       GlobalDef   v τ _ | v == target -> Just τ
--       GlobalInd   i -> go (unwrapInductive i)
--       _ -> Nothing

toLocalContext ::
  GlobalEnvironment (C.Checked Variable) Variable ->
  LC.LocalContext (C.Checked Variable) Variable
toLocalContext =
  LC.LocalContext . foldr (\ g acc -> localize g ++ acc) [] . unGlobalEnvironment
  where
    localize ::
      GlobalDeclaration (C.Checked Variable) Variable ->
      [LocalDeclaration (C.Checked Variable) Variable]
    localize = \case
      GlobalAssum v τ   -> [LocalAssum (Binder (Just v)) τ]
      GlobalDef   v τ t -> [LocalDef v τ t]
      GlobalInd   i     -> concatMap localize (unwrapInductive i)

-- envInductives :: GlobalEnvironment ξ ν -> [I.Inductive ξ ν]
-- envInductives = foldr addIfInductive [] . unGlobalEnvironment
--   where
--     addIfInductive :: GlobalDeclaration ξ ν -> [I.Inductive ξ ν] -> [I.Inductive ξ ν]
--     addIfInductive (GlobalInd i) a = i : a
--     addIfInductive _             a = a

{-| 'isInductive' checks whether the type is an instance of some
inductive type in the global environment.  For instance, given '(List
T)', it should return the inductive 'Some (Inductive for List)'.
Given '(A -> List T)', it should return 'None'.
-}
isInductive ::
  Eq ν =>
  GlobalEnvironment (C.Checked ν) ν -> C.Type ν ->
  Maybe (I.Inductive (C.Checked ν) ν)
isInductive ge = go
  where
    go τ =
      case τ of
        App _ l _ -> go l
        Var _ v   ->
          case lookupDecl v ge of
            Just (GlobalInd i) -> Just i
            _ -> Nothing
        _ -> Nothing

-- | Adds the type of an inductive to the global environment
addInductiveType ::
  I.Inductive Raw.Raw Variable ->
  GlobalEnvironment Raw.Raw Variable -> GlobalEnvironment Raw.Raw Variable
addInductiveType ind =
  addGlobalAssum (Binder $ Just $ I.inductiveName ind, I.inductiveRawType ind)

addConstructor ::
  I.Constructor Raw.Raw Variable ->
  GlobalEnvironment Raw.Raw Variable -> GlobalEnvironment Raw.Raw Variable
addConstructor c = addGlobalAssum (Binder $ Just $ I.constructorName c, I.constructorRawType True c)
