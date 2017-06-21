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
--import Test.QuickCheck
import Text.PrettyPrint.Annotated.WL
import Text.Printf

--import Inductive.Constructor
import Inductive.Inductive
--import Precedence
import PrettyPrinting.PrettyPrintable
--import PrettyPrinting.PrettyPrintableUnannotated
--import Substitutable
--import Term.AlphaEquivalence
import Term.AlphaRenaming
import Term.Binder
import Term.Free
import Term.Term
import Term.TypeChecked as TypeChecked
import Term.Variable
import Typing.GlobalEnvironment
import Typing.LocalDeclaration
import Typing.LocalContext
import Utils

data Goal ξ ν = Goal
  { hypotheses :: LocalContext ξ ν
  , conclusion :: TermX ξ ν
  }
  deriving (Generic)

deriving instance (Eq ξ, Eq ν) => Eq (Goal ξ ν)
deriving instance (Show ξ, Show ν) => Show (Goal ξ ν)

{-
instance (Arbitrary ξ) => Arbitrary (Goal ξ ν) where
  arbitrary = Goal <$> take 2 <$> listOf arbitrary <*> arbitrary
-}

{-
instance PrettyPrintableAnnotated Goal where
  prettyDocA (Goal hyps concl) = do
    hypsDoc <- mapM prettyDocA hyps
    conclDoc <- prettyDocA concl
    return $ vcat
      [ vcat hypsDoc
      , text (replicate 40 '-')
      , conclDoc
      ]
-}

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
  LocalDeclaration ξ ν -> LocalContext ξ ν -> m (LocalContext ξ ν)
addHyp hyp (LocalContext hyps)
  | nameOf hyp `elem` map nameOf hyps = throwError "addHyp: name conflict"
  | otherwise = return . LocalContext $ hyp:hyps

data Atomic ν
  = Admit
  | Destruct -- assumes goal is ∀ x. P and destructs x
  | Exact ν
  | Intro (Binder ν)
  deriving (Show)

instance PrettyPrintable ν => PrettyPrintable (Atomic ν) where
  prettyDoc = \case
    Admit    -> text "admit"
    Destruct -> text "destruct"
    Exact v  -> fillCat [text "exact", prettyDoc v]
    Intro b  -> fillCat [text "intro", prettyDoc b]

lookupVariable ::
  MonadError String m =>
  Variable ->
  LocalContext (Checked Variable) Variable ->
  GlobalEnvironment (Checked Variable) Variable ->
  m (TypeChecked.Term Variable)
lookupVariable v hyps ge =
  case Typing.LocalContext.lookupType v (hyps `mappend` toLocalContext ge) of
    Nothing ->
      throwError $
      printf "Could not find variable %s in global environment"
      (prettyStr v)
    Just τ -> return τ

runAtomic ::
  MonadError String m =>
  GlobalEnvironment (Checked Variable) Variable ->
  Atomic Variable -> Goal (Checked Variable) Variable -> m [Goal (Checked Variable) Variable]
runAtomic ge a (Goal hyps concl) =
  case a of

  Admit -> return []

  Destruct -> do
    Pi _ τ1 _bτ2            <- isPi concl        `orElse` "destruct expects the goal to be a forall"
    Inductive _n _ps _is cs <- isInductive ge τ1 `orElse` "destruct expects the term to be inductive"
    {-
    let (b, τ2) = unscopeTerm bτ2
    let conclModifier cTerm concl =
          case unBinder b of
            -- if the Pi was not binding, no need for substitution
            Nothing -> concl
            -- if the Pi
            Just v  -> substitute v cTerm concl
    -}
    forM cs $ \ _c -> do
      error "TODO (see bottom of Inductive/Constructor.hs)"

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
      Let _ t1 bt2 -> do
        τ1 <- typeOf t1 `orElse` "could not figure out the type of let-bound variable"
        let (b, t2) = unscopeTerm bt2
        runIntro τ1 t2 (flip LocalDef t1) (mi, unBinder b)
      Pi  _ τ1 bτ2 -> do
        let (b, τ2) = unscopeTerm bτ2
        runIntro τ1 τ2 LocalAssum (mi, unBinder b)
      _ -> throwError "Head constructor does not allow introduction"

   where

     runIntro ::
       MonadError String m =>
       TypeChecked.Term Variable -> TypeChecked.Term Variable ->
       (Variable -> TypeChecked.Term Variable -> LocalDeclaration (Checked Variable) Variable) ->
       (Maybe Variable, Maybe Variable) -> m (Goal (Checked Variable) Variable)
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

data Tactic ν
  = Atomic (Atomic ν)
  | Semicolon (Tactic ν) (Tactic ν)
  deriving (Show)

instance PrettyPrintable ν => PrettyPrintable (Tactic ν) where
  prettyDoc = \case
    Atomic a -> prettyDoc a
    Semicolon a b ->
      fillCat
        [ prettyDoc a
        , text "; "
        , prettyDoc b
        ]

decomposeTactic :: Tactic ν -> (Atomic ν, [Tactic ν])
decomposeTactic (Atomic a)      = (a, [])
decomposeTactic (Semicolon a b) =
  let (atomic, ts) = decomposeTactic a in
  (atomic, ts ++ [b])

runTactic ::
  MonadError String m =>
  GlobalEnvironment (Checked Variable) Variable ->
  Tactic Variable -> Goal (Checked Variable) Variable -> m [Goal (Checked Variable) Variable]
runTactic ge t goal =
  case t of
    Atomic a -> runAtomic ge a goal
    Semicolon t1 t2 -> do
      gs <- runTactic ge t1 goal
      gs' <- concat <$> sequence (map (runTactic ge t2) gs)
      return gs'
