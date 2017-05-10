{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Tactic where

import Control.Monad.Except
import GHC.Generics
import Prelude
import Test.QuickCheck
import Text.PrettyPrint.Annotated.WL
import Text.Printf

import DictMetaOut
import Inductive.Constructor
import Inductive.Inductive
import Precedence
import PrettyPrinting.PrettyPrintable
import PrettyPrinting.PrettyPrintableAnnotated
import Substitutable
import Term.AlphaEquivalence
import Term.AlphaRenaming
import Term.Binder
import Term.Free
import Term.Term
import Term.TypeChecked as TypeChecked
import Term.Variable
import Typing.GlobalEnvironment
import Typing.LocalDeclaration
import Typing.LocalContext

data Goal ξ ν = Goal
  { hypotheses :: [LocalDeclaration ξ ν]
  , conclusion :: TermX ξ ν
  }
  deriving (Generic)

deriving instance (ForallX Eq ξ, Eq ν) => Eq (Goal ξ ν)
deriving instance (ForallX Show ξ, Show ν) => Show (Goal ξ ν)

{-
instance (ForallX Arbitrary ξ) => Arbitrary (Goal ξ ν) where
  arbitrary = Goal <$> take 2 <$> listOf arbitrary <*> arbitrary
-}

instance PrettyPrintableAnnotated Goal where
  prettyDocA (Goal hyps concl) = do
    hypsDoc <- mapM prettyDocA hyps
    conclDoc <- prettyDocA concl
    return $ vcat
      [ vcat hypsDoc
      , text (replicate 40 '-')
      , conclDoc
      ]

data Goals ξ ν = Goals
  { focused   :: [Goal ξ ν]
  , unfocused :: [([Goal ξ ν], [Goal ξ ν])]
  }

splitList
   :: Int -> [a] -> Maybe ([a], a, [a])
splitList n xs =
  revL <$> go n xs
  where
    go 0 (h:t) = Just ([], h, t)
    go m (h:t) = prependL h <$> go (m-1) t
    go _ []    = Nothing
    prependL h (revl, x, r) = (h:revl, x, r)
    revL (l, x, r) = (reverse l, x, r)

focus :: Int -> Goals ξ ν -> Maybe (Goals ξ ν)
focus n (Goals f u) =
  case splitList n f of
  Nothing -> Nothing
  Just (l, x, r) -> Just (Goals [x] ((l, r):u))

addHyp ::
  (Eq ν, MonadError String m) =>
  LocalDeclaration ξ ν -> [LocalDeclaration ξ ν] -> m [LocalDeclaration ξ ν]
addHyp hyp hyps
  | nameOf hyp `elem` map nameOf hyps = throwError "addHyp: name conflict"
  | otherwise = return $ hyp:hyps

data Atomic ν
  = Admit
  | Destruct -- assumes goal is ∀ x. P and destructs x
  | Exact ν
  | Intro (Binder ν)
  deriving (Show)

orElse :: MonadError e m => Maybe a -> e -> m a
orElse Nothing  e = throwError e
orElse (Just a) _ = return a

isPi :: TermX ξ ν -> Maybe (TermX ξ ν)
isPi t@(Pi _ _ _) = Just t
isPi _            = Nothing

lookupVariable ::
  MonadError String m =>
  Variable ->
  LocalContext (TypeChecked Variable) Variable ->
  GlobalEnvironment (TypeChecked Variable) Variable ->
  m (TypeChecked.Term Variable)
lookupVariable v hyps ge =
  case Typing.LocalContext.lookupType v (hyps `mappend` toLocalContext ge) of
    Nothing ->
      throwError $
      printf "Could not find variable %s in global environment"
      (prettyStr v)
    Just τ -> return τ

{-
runAtomic ::
  MonadError String m =>
  GlobalEnvironment (TypeChecked ν) ν ->
  Atomic ν -> Goal (TypeChecked ν) ν -> m [Goal (TypeChecked ν) ν]
runAtomic ge a (Goal hyps concl) =
  case a of

  Admit -> return []

  Destruct -> do
    Pi _ τ1 bτ2          <- isPi concl        `orElse` "destruct expects the goal to be a forall"
    Inductive n ps is cs <- isInductive ge τ1 `orElse` "destruct expects the term to be inductive"
    let conclModifier cTerm concl =
          case unBinder b of
            -- if the Pi was not binding, no need for substitution
            Nothing -> concl
            -- if the Pi
            Just v  -> subst v cTerm concl
    forM cs $ \c -> do
      error "TODO (see bottom of Inductive/Constructor.hs)"
      {-
      let cTerm = constructorTerm c
      return . Goal hyps $ case unBinder b of
        Nothing -> concl
        Just v  -> subst v cTerm concl
      -}
    {-
    case concl of
      Pi _ v τ _ -> do
        case isInductive ge τ of
          Nothing -> throwError "Cannot destruct a non-inductive type"
          Just (Inductive n ps is cs) ->
            forM cs $ \c -> do
            l_
      _ -> throwError "destruct expects the goal to be a forall"
-}
{-
      τ <- lookupVariable v hyps ge
      case isInductive ge τ of
        Nothing -> throwError "Cannot destruct a non-inductive type"
        Just (Inductive n ps is cs) ->
          forM cs $ \ (Constructor n ps is) -> do
          subst v (constructor
-}

  Exact v ->
    do
      τ <- lookupVariable v hyps ge
      if τ == concl
        then return []
        else throwError $
             printf "The type of %s does not match the conclusion"
             (prettyStr v)

  Intro (Binder mi) ->
    pure <$> -- i.e. returns just the one goal produced
      case concl of
      Let _ t1 bt2 -> runIntro (typeOf t1) t2 (flip LocalDef t1) (mi, mv)
      Pi  _ τ1 bτ2 -> runIntro τ1          τ2 LocalAssum         (mi, mv)
      _ -> throwError "Head constructor does not allow introduction"

   where

     runIntro ::
       MonadError String m =>
       TypeChecked.Term ν -> TypeChecked.Term ν ->
       (Variable -> TypeChecked.Term ν -> LocalDeclaration (TypeChecked ν) ν) ->
       (Maybe Variable, Maybe Variable) -> m (Goal (TypeChecked ν) ν)
     runIntro introed rest h = \case
       (Nothing, Nothing) -> return $ Goal hyps rest
       (Just i, Nothing) -> do
         hyps' <- addHyp (h i introed) hyps
         return $ Goal hyps' rest
       (Nothing, Just v) ->
         if isFree v rest
         then return $ Goal hyps rest
         else throwError "Can't discard a dependent binder"
       (Just i, Just v) ->
         Goal <$> addHyp (h i introed) hyps <*> pure (αrename v i rest)
-}

data Tactic ν
  = Atomic (Atomic ν)
  | Semicolon (Tactic ν) (Tactic ν)
  deriving (Show)

decomposeTactic :: Tactic ν -> (Atomic ν, [Tactic ν])
decomposeTactic (Atomic a)      = (a, [])
decomposeTactic (Semicolon a b) =
  let (atomic, ts) = decomposeTactic a in
  (atomic, ts ++ [b])

{-
runTactic ::
  MonadError String m =>
  GlobalEnvironment (TypeChecked ν) ν ->
  Tactic ν -> Goal (TypeChecked ν) ν -> m [Goal (TypeChecked ν) ν]
runTactic ge t goal =
  case t of
    Atomic a -> runAtomic ge a goal
    Semicolon t1 t2 -> do
      gs <- runTactic ge t1 goal
      gs' <- concat <$> sequence (map (runTactic ge t2) gs)
      return gs'
-}
