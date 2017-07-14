{-# language ConstraintKinds #-}
{-# language DeriveFunctor #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language UndecidableInstances #-}

module Work where

--import Bound.Name
--import Bound.Scope
--import Control.Monad
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Trans.Free
import Data.Bifunctor
--import Data.Default
import Prelude                                   hiding (or)
--import Text.Printf
import Text.PrettyPrint.Annotated.WL

import Precedence
import PrettyPrinting.Term ()
import PrettyPrinting.PrettyPrintableUnannotated
import Typing.LocalContext
import Term.Binder
--import PrettyPrinting.Term
import Term.Raw                                  as Raw
import Term.Term
import Term.TypeChecked                          as TypeChecked
import Term.TypeErrored                          as TypeErrored
import Term.Variable
import TypeCheckingFailure
import Utils

type TypeCheckingTerm ν = Either (TypeErrored.Term ν) (TypeChecked.Term ν)

data TypeCheckerF ν k
  = Check (LocalContext (Checked ν) ν) (Raw.Term ν) (Raw.Type ν) (TypeCheckingTerm ν -> k)
  | Synth (LocalContext (Checked ν) ν) (Raw.Term ν)              (TypeCheckingTerm ν -> k)
  | Failure (TypeErrored.Term ν)
  | Success (TypeChecked.Term ν)
  deriving (Functor)

tcTrace ::
  (TypeCheckerF ν k -> TypeCheckerF ν k) -> TypeCheckerF ν k -> [TypeCheckerF ν k]
tcTrace _    (Failure f) = [Failure f]
tcTrace _    (Success s) = [Success s]
tcTrace step w           = w : tcTrace step (step w)

tc ::
  TypeCheckerF Variable (TCMonad Variable (TypeChecked.Term Variable)) ->
  Either (TypeErrored.Term Variable) (TypeChecked.Term Variable)
tc (Failure f) = Left f
tc (Success s) = Right s
tc w           = tc (stepTypeCheckerF w)

{-
tcStep ::
  TypeCheckMonad (TypeCheckerT Identity TypeCheckingTerm) ->
  TypeCheckerF (TypeCheckerT Identity TypeCheckingTerm) ->
  TypeCheckerF (TypeCheckerT Identity TypeCheckingTerm)
tcStep magic t =
  let x = runIdentity . runFreeT . runExceptT $ magic in
  case x of
  Pure (Left  l) -> Failure l
  Pure (Right r) -> Success _
  Free f ->
    let beep = _ f in
    beep
-}

showContext :: Bool
showContext = False

prettyTypeCheckerF ::
  (MonadReader PrecedenceTable m) =>
  TypeCheckerF Variable k -> m (Doc ())
prettyTypeCheckerF = \case
  Check _γ t τ _ -> do
    tDoc <- prettyDocU t
    τDoc <- prettyDocU τ
    return $ fillSep
      [ text "Check"
      , text "γ ⊢"
      , tDoc
      , text ":"
      , τDoc
      ]
  Synth _γ t _ -> do
    tDoc <- prettyDocU t
    return $ fillSep
      [ text "Synth"
      , text "γ ⊢"
      , tDoc
      ]
  Failure f -> do
    fDoc <- prettyDocU (raw f)
    return $ fillSep
      [ text "Failure"
      , fDoc
      ]
  Success s -> do
    sDoc <- prettyDocU (raw s)
    return $ fillSep
      [ text "Success"
      , sDoc
      ]

{-
instance Show (TypeCheckerF k) where
  show (Check γ t τ _) =
    "\n  Check\n  "
    ++ if showContext then show γ else ""
    ++ printf "%v\n  %v\n" t τ
  show (Synth γ t _)   =
    "\n  Synth\n  "
    ++ if showContext then show γ else ""
    ++ printf "%v\n" t
  show (Failure t)   = printf "Failure %v" t
  show (Success t)   = printf "Success %v" t
-}

{-
instance Functor TypeCheckerF where
  fmap f (Synth t k) = Synth t (f . k)
  fmap f (Check t τ k) = Check t τ (f . k)
  --fmap f (Done r) = Done r
-}

type TypeCheckerT ν = FreeT (TypeCheckerF ν)

type MonadTypeCheck ν m =
  ( MonadError (TypeErrored.Term ν) m
  , MonadFree  (TypeCheckerF ν) m
  )

checkF ::
  MonadError e m =>
  LocalContext (Checked ν) ν -> TermX ξ ν -> TermX ψ ν -> (TypeErrored.Term ν -> e) ->
  TypeCheckerF ν (m (TypeChecked.Term ν))
checkF γ t τ h = Check γ (raw t) (raw τ) (either (throwError . h) return)

checkM ::
  MonadTypeCheck ν m =>
  LocalContext (Checked ν) ν -> TermX ξ ν -> TypeX ψ ν -> (TypeErrored.Term ν -> TypeErrored.Term ν) ->
  m (TypeChecked.Term ν)
checkM γ t τ h = wrap $ checkF γ t τ h

synthF ::
  MonadError e m =>
  LocalContext (Checked ν) ν -> TermX ξ ν -> (TypeErrored.Term ν -> e) ->
  TypeCheckerF ν (m (TypeChecked.Term ν))
synthF γ t h = Synth γ (raw t) (either (throwError . h) return)

synthM ::
  MonadTypeCheck ν m =>
  LocalContext (Checked ν) ν -> TermX ξ ν -> (TypeErrored.Term ν -> TypeErrored.Term ν) ->
  m (TypeChecked.Term ν)
synthM γ t h = wrap $ synthF γ t h

failure :: MonadTypeCheck ν m => TypeErrored.Term ν -> m (TypeChecked.Term ν)
failure t = wrap $ Failure t

success :: MonadTypeCheck ν m => TypeChecked.Term ν -> m (TypeChecked.Term ν)
success t = wrap $ Success t

{-
synth :: Monad m => TermX ξ -> TypeCheckerT m
synth t = liftF $ Synth (raw t) id

check :: Monad m => TermX ξ -> TypeX ξ -> TypeCheckerT m
check t τ = liftF $ Check (raw t) (raw τ) id

done :: Monad m => TypeChecked.Term -> TypeCheckerT m
done r = liftF $ Done r
-}

rawType :: Raw.Term ν
rawType = Type

{-
meither :: Monad m => m (Either l r) -> (l -> m o) -> (r -> m o) -> m o
meither a l r = a >>= either l r

tryEither :: Monad m => m (Either l r) -> (l -> m o) -> Cont (m o) r
tryEither m l = cont $ meither m l
-}

(~!) :: forall ξ ν. TermX ξ ν -> TypeErrored.Term ν
(~!) = first (const (Left Unchecked))

(~!!) ::
  forall ξ.
  NameScope (TermX ξ) Variable ->
  NameScope (TermX (Either (TypeError Variable) (TypeChecked.Checked Variable))) Variable
(~!!) s =
  let (b, t) = unscopeTerm s in
  abstractBinder b ((~!) t)

(!->) :: TypeChecked.Term ν -> TypeErrored.Term ν
(!->) = first Right

runFreeTypeCheckerT :: TypeCheckerT ν m a ->
                      m (FreeF (TypeCheckerF ν) a (TypeCheckerT ν m a))
runFreeTypeCheckerT = runFreeT

-- TypeCheckerT = FreeT TypeCheckerF
type TCMonad ν = TypeCheckerT ν (ExceptT (TypeErrored.Term ν) Identity)

runTypeCheck2 ::
  TCMonad ν a ->
  Either (TypeErrored.Term ν) (FreeF (TypeCheckerF ν) a (TCMonad ν a))
runTypeCheck2 = runIdentity . runExceptT . runFreeT

{-
hole :: TCMonad TypeChecked.Term -> TypeCheckingTerm
hole m =
  case runTypeCheck2 m of
  Left l -> Left l
  Right r ->
    case r of
    Pure t -> Right t
    Free f ->
      case f of
      Synth t k -> hole . k . runSynth $ t

runSynth :: Raw.Term -> TypeCheckingTerm
runSynth = \case
  App () fun arg -> hole $ (synthApp fun arg) -- :: TCMonad TypeChecked.Term)
  t -> Left (unchecked t)

interpret :: Monad m => TypeCheckerT m TypeCheckingTerm -> m TypeCheckingTerm
interpret thing = do
  foo <- runFreeT thing
  case foo of
    Pure p -> return p
    Free t ->
      case t of
      Failure f -> return (Left f)
      Success s -> return (Right s)
      Synth t k -> interpret . k $ runSynth t
-}

eqβ :: TermX ξ ν -> TermX ψ ν -> Bool
_ `eqβ` _ = True -- TODO

redβ :: TermX ξ ν -> TermX ξ ν
redβ = id

matchBinders :: Eq ν => Binder ν -> Binder ν -> Maybe (Binder ν)
matchBinders (Binder a) (Binder b) = case (a, b) of
  (Nothing, Nothing) -> Just (Binder Nothing)
  (Just  _, Nothing) -> Just (Binder a)
  (Nothing, Just  _) -> Just (Binder b)
  (Just va, Just vb) -> if va == vb then Just (Binder a) else Nothing

runSynth' ::
  (MonadTypeCheck Variable m) =>
  LocalContext (Checked Variable) Variable -> TermX ξ Variable -> m (TypeChecked.Term Variable)
runSynth' γ = \case

  App _ fun arg -> do
    -- synthesize a type for fun
    sFun <- synthM γ fun
           (\ fFun -> App (Left AppFunctionFailed) fFun ((~!) arg))
    -- check that this type is a π-type : (binder : τIn) -> τOut binder
    τFun <- typeOf sFun `orElse`
           (App (Left AppFunctionFailed) ((!->)sFun) ((~!) arg))
    Pi _ τIn bτOut <-
      isPi τFun `orElse`
      (App (Left (AppFunctionTypeFailed (raw fun))) ((!->) sFun) ((~!) arg))
    -- check that arg has the type τIn
    cArg <- checkM γ arg τIn
           (\ fArg -> App (Left AppArgumentFailed) ((!->) sFun) fArg)
    -- perform substitution if needed
    --let n = name _ --bτOut
    let (b, τOut) = unscopeTerm bτOut
    case unBinder b of
      Just v -> do
        let τOut' = substitute v cArg τOut -- cArg τOut
        return $ App (Checked τOut') sFun cArg
      Nothing ->
        return $ App (Checked τOut) sFun cArg

  Var _ name ->
    case lookupType name γ of
    Nothing -> throwError $ Var Nothing name
    Just τ -> return τ

  Pi _ τIn bτOut -> do
    let (b, τOut) = unscopeTerm bτOut
    τIn' <- checkM γ τIn Type
           (\ _τIn' -> error "TODO qwer")
    let γ' = addLocalAssum (b, τIn') γ
    τOut' <- checkM γ' τOut Type
            (\ _τOut' -> error "TODO adsf")
    -- this is weird
    return $ Pi (Checked Type) τIn' (abstractBinder b τOut')

  Type -> return $ Type

  Lam _ bt -> throwError $ Lam (Left SynthesizeLambda) ((~!!) bt)

  term -> throwError $ (~!) term

runCheck' ::
  (MonadTypeCheck Variable m) =>
  LocalContext (Checked Variable) Variable -> TermX ξ Variable -> TermX ψ Variable -> m (TypeChecked.Term Variable)
runCheck' γ t τ = case t of

  Lam _ bt -> do
    let (bLam, tLam) = unscopeTerm bt
    τ' <- checkM γ τ Type                     (\ _τ' -> annotateError NotAType t)
    Pi _ τIn bτOut <- isPi (redβ τ') `orElse` annotateError (error "TODO") t
    let (bOut, τOut) = unscopeTerm bτOut
    _ <- matchBinders bLam bOut      `orElse` annotateError (error . show $ bOut) t
    let γ' = addLocalAssum (bLam, τIn) γ
    tLam' <- checkM γ' tLam τOut              (\ _t' -> error "TODO")
    return $ Lam (Checked τ') (abstractBinder bLam tLam')

  Hole _ -> error "runCheck Hole"

  -- conversion rule
  _ -> do
    t' <- synthM γ t                  (\ t' -> t')
    τ' <- typeOf t' `orElse`  annotateError TODO t
    () <- (τ' `eqβ` τ)      `orElse'` annotateError IncompatibleTypes t
    return t'

runTypeCheckerF ::
  TypeCheckerF Variable (TypeCheckerT Variable (TCMonad Variable) (TypeChecked.Term Variable)) ->
  TCMonad Variable (TypeChecked.Term Variable)
runTypeCheckerF ff = case ff of
  Failure f     -> throwError f
  Success s     -> return s
  Synth γ t k   -> join $ runTypeCheckerT . k . Right <$> runSynth' γ t
  Check γ t τ k -> join $ runTypeCheckerT . k . Right <$> runCheck' γ t τ

runFreeF ::
  FreeF (TypeCheckerF Variable) (TypeChecked.Term Variable)
  (TypeCheckerT Variable (TCMonad Variable) (TypeChecked.Term Variable)) ->
  TCMonad Variable (TypeChecked.Term Variable)
runFreeF = \case
  Pure pp -> return pp
  Free ff -> runTypeCheckerF ff

runTypeCheckerT ::
  TypeCheckerT Variable (TCMonad Variable) (TypeChecked.Term Variable) ->
  TCMonad Variable (TypeChecked.Term Variable)
runTypeCheckerT = runFreeT >=> runFreeF

runTypeCheckerF' ::
  TypeCheckerF Variable (TCMonad Variable (TypeChecked.Term Variable)) ->
  TCMonad Variable (TypeChecked.Term Variable)
runTypeCheckerF' ff = case ff of
  Failure f     -> throwError f
  Success s     -> return s
  Synth γ t k   -> join $ k . Right <$> runSynth' γ t
  Check γ t τ k -> join $ k . Right <$> runCheck' γ t τ

runFreeF' ::
  FreeF (TypeCheckerF Variable) (TypeChecked.Term Variable)
  (TCMonad Variable (TypeChecked.Term Variable)) ->
  TCMonad Variable (TypeChecked.Term Variable)
runFreeF' = \case
  Pure pp -> return pp
  Free ff -> runTypeCheckerF' ff

stepTypeCheckerF ::
  TypeCheckerF Variable (TCMonad Variable (TypeChecked.Term Variable)) ->
  TypeCheckerF Variable (TCMonad Variable (TypeChecked.Term Variable))
stepTypeCheckerF input =
  case runTypeCheck2 . runTypeCheckerF' $ input of
  Left  l -> Failure l
  Right r ->
    case r of
    Pure p -> Success p
    Free f -> f

{-
ExceptT ... FreeT ... Identity
-}
