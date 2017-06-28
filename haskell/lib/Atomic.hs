{-# language FlexibleContexts #-}
{-# language LambdaCase #-}

module Atomic
  ( Atomic(..)
  , runAtomic
  )where

import           Control.Monad.Except
import           Text.PrettyPrint.Annotated.WL
import           Text.Printf

import           Goal
import           Inductive.Inductive
import           PrettyPrinting.PrettyPrintable
import           Term.AlphaRenaming
import           Term.Binder
import           Term.Free
import           Term.Term
import qualified Term.TypeChecked as C
import           Term.Variable
import qualified Typing.GlobalEnvironment as GE
import           Typing.LocalDeclaration
import qualified Typing.LocalContext as LC
import           Utils

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

-- TODO: this should go elsewhere
-- TODO: turn this into a freer-effect
lookupVariable ::
  MonadError String m =>
  Variable ->
  LC.LocalContext (C.Checked Variable) Variable ->
  GE.GlobalEnvironment (C.Checked Variable) Variable ->
  m (C.Term Variable)
lookupVariable v hyps ge =
  case LC.lookupType v (hyps `mappend` GE.toLocalContext ge) of
    Nothing ->
      throwError $
      printf "Could not find variable %s in global environment"
      (prettyStr v)
    Just τ -> return τ

runAtomic ::
  MonadError String m =>
  GE.GlobalEnvironment (C.Checked Variable) Variable ->
  Atomic Variable -> Goal (C.Checked Variable) Variable -> m [Goal (C.Checked Variable) Variable]
runAtomic ge a (Goal hyps concl) =
  case a of

  Admit -> return []

  Destruct -> do
    Pi _ τ1 _bτ2            <- isPi concl           `orElse` "destruct expects the goal to be a forall"
    Inductive _n _ps _is cs <- GE.isInductive ge τ1 `orElse` "destruct expects the term to be inductive"
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
        τ1 <- C.typeOf t1 `orElse` "could not figure out the type of let-bound variable"
        let (b, t2) = unscopeTerm bt2
        runIntro τ1 t2 (flip LocalDef t1) (mi, unBinder b)
      Pi  _ τ1 bτ2 -> do
        let (b, τ2) = unscopeTerm bτ2
        runIntro τ1 τ2 LocalAssum (mi, unBinder b)
      _ -> throwError "Head constructor does not allow introduction"

   where

     runIntro ::
       MonadError String m =>
       C.Term Variable -> C.Term Variable ->
       (Variable -> C.Term Variable -> LocalDeclaration (C.Checked Variable) Variable) ->
       (Maybe Variable, Maybe Variable) -> m (Goal (C.Checked Variable) Variable)
     runIntro introed rest h = \case
       (Nothing, Nothing) -> return $ Goal hyps rest
       (Just i, Nothing) -> do
         hyps' <- LC.addHyp (h i introed) hyps
         return $ Goal hyps' rest
       (Nothing, Just v) ->
         if isFree v rest
         then return $ Goal hyps rest
         else throwError "Can't discard a dependent binder"
       (Just i, Just v) ->
         Goal <$> LC.addHyp (h i introed) hyps <*> pure (αrename v i rest)
