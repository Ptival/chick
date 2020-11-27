{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Atomic
  ( Atomic (..),
    runAtomic,
  )
where

import Control.Monad.Except ( forM, MonadError(throwError) )
import Goal
import qualified Inductive.Inductive as Inductive
import Language (Language (Chick))
import PrettyPrinting.Chick.Binder ()
import PrettyPrinting.HasNonBindingPattern ( HasNonBindingPattern )
import PrettyPrinting.PrettyPrintable
  ( PrettyPrintable (prettyDoc, prettyStr),
  )
import qualified Prettyprinter as Doc
import Term.AlphaRenaming (αrename)
import Term.Binder (Binder (..))
import Term.Free (isFree)
import Term.Term
  ( TermX (Let, Pi),
    Variable,
    unscopeTerm,
  )
import qualified Term.TypeChecked as C
import Text.Printf (printf)
import qualified Typing.GlobalEnvironment as GE
import qualified Typing.LocalContext as LC
import Typing.LocalDeclaration
  ( LocalDeclaration (LocalAssum, LocalDef),
  )
import Utils ( isPi, orElse )

data Atomic ν
  = Admit
  | Destruct -- assumes goal is ∀ x. P and destructs x
  | Exact ν
  | Intro (Binder ν)
  deriving (Show)

instance
  ( HasNonBindingPattern l,
    PrettyPrintable l ν
  ) =>
  PrettyPrintable l (Atomic ν)
  where
  prettyDoc = \case
    Admit -> Doc.pretty "admit"
    Destruct -> Doc.pretty "destruct"
    Exact v -> Doc.fillCat [Doc.pretty "exact", prettyDoc @l v]
    Intro b -> Doc.fillCat [Doc.pretty "intro", prettyDoc @l b]

-- TODO: this should go elsewhere
-- TODO: turn this into a freer-effect
lookupVariable ::
  forall l m.
  MonadError String m =>
  PrettyPrintable l Variable =>
  Variable ->
  LC.LocalContext (C.Checked Variable) Variable ->
  GE.GlobalEnvironment (C.Checked Variable) Variable ->
  m (C.Term Variable)
lookupVariable v hyps ge =
  case LC.lookupType v (hyps `mappend` GE.toLocalContext ge) of
    Nothing ->
      throwError $
        printf
          "Could not find variable %s in global environment"
          (prettyStr @l v)
    Just τ -> return τ

runAtomic ::
  forall l m.
  MonadError String m =>
  PrettyPrintable l Variable =>
  GE.GlobalEnvironment (C.Checked Variable) Variable ->
  Atomic Variable ->
  Goal (C.Checked Variable) Variable ->
  m [Goal (C.Checked Variable) Variable]
runAtomic ge a (Goal hyps concl) =
  case a of
    Admit -> return []
    Destruct -> do
      (_, τ1, _) <- isPi concl `orElse` "destruct expects the goal to be a forall"
      Inductive.Inductive {..} <- GE.isInductive ge τ1 `orElse` "destruct expects the term to be inductive"
      {-
      let (b, τ2) = unscopeTerm bτ2
      let conclModifier cTerm concl =
            case unBinder b of
              -- if the Pi was not binding, no need for substitution
              Nothing -> concl
              -- if the Pi
              Just v  -> substitute v cTerm concl
      -}
      forM inductiveConstructors $ \_c ->
        error "TODO (see bottom of Inductive/Constructor.hs)"
    Exact v ->
      do
        τ <- lookupVariable @l v hyps ge
        if τ == concl
          then return []
          else
            throwError $
              printf
                "The type of %s does not match the conclusion"
                (prettyStr @ 'Chick v)
    Intro (Binder mi) ->
      pure
        <$> case concl of -- i.e. returns just the one goal produced
          Let _ t1 bt2 -> do
            τ1 <- C.typeOf t1 `orElse` "could not figure out the type of let-bound variable"
            let (b, t2) = unscopeTerm bt2
            runIntro τ1 t2 (\v τ -> LocalDef v τ t1) (mi, unBinder b)
          Pi _ τ1 bτ2 -> do
            let (b, τ2) = unscopeTerm bτ2
            runIntro τ1 τ2 (\v e -> LocalAssum (Binder (Just v)) e) (mi, unBinder b)
          _ -> throwError "Head constructor does not allow introduction"
      where
        runIntro ::
          MonadError String m =>
          C.Term Variable ->
          C.Term Variable ->
          (Variable -> C.Term Variable -> LocalDeclaration (C.Checked Variable) Variable) ->
          (Maybe Variable, Maybe Variable) ->
          m (Goal (C.Checked Variable) Variable)
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
