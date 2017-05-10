{-# language ConstraintKinds #-}
{-# language DeriveFunctor #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language UndecidableInstances #-}

module Work where

--import Control.Monad
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Trans.Free
--import Data.Default
import Prelude                                   hiding (or)
--import Text.Printf
import Text.PrettyPrint.Annotated.WL

import Precedence
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

type TypeCheckingTerm ν = Either (TypeErrored.Term ν) (TypeChecked.Term ν)

data TypeCheckerF ν k
  = Check (LocalContext (TypeChecked ν) ν) (Raw.Term ν) (Raw.Type ν) (TypeCheckingTerm ν -> k)
  | Synth (LocalContext (TypeChecked ν) ν) (Raw.Term ν)              (TypeCheckingTerm ν -> k)
  | Failure (TypeErrored.Term ν)
  | Success (TypeChecked.Term ν)
  deriving (Functor)

tcTrace ::
  (TypeCheckerF ν k -> TypeCheckerF ν k) -> TypeCheckerF ν k -> [TypeCheckerF ν k]
tcTrace _    (Failure f) = [Failure f]
tcTrace _    (Success s) = [Success s]
tcTrace step w           = w : tcTrace step (step w)

tc ::
  TypeCheckerF ν (TCMonad ν (TypeChecked.Term ν)) ->
  Either (TypeErrored.Term ν) (TypeChecked.Term ν)
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
  LocalContext (TypeChecked ν) ν -> TermX ξ ν -> TermX ψ ν -> (TypeErrored.Term ν -> e) ->
  TypeCheckerF ν (m (TypeChecked.Term ν))
checkF γ t τ h = Check γ (raw t) (raw τ) (either (throwError . h) return)

checkM ::
  MonadTypeCheck ν m =>
  LocalContext (TypeChecked ν) ν -> TermX ξ ν -> TypeX ψ ν -> (TypeErrored.Term ν -> TypeErrored.Term ν) ->
  m (TypeChecked.Term ν)
checkM γ t τ h = wrap $ checkF γ t τ h

synthF ::
  MonadError e m =>
  LocalContext (TypeChecked ν) ν -> TermX ξ ν -> (TypeErrored.Term ν -> e) ->
  TypeCheckerF ν (m (TypeChecked.Term ν))
synthF γ t h = Synth γ (raw t) (either (throwError . h) return)

synthM ::
  MonadTypeCheck ν m =>
  LocalContext (TypeChecked ν) ν -> TermX ξ ν -> (TypeErrored.Term ν -> TypeErrored.Term ν) ->
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
rawType = Type ()

{-
meither :: Monad m => m (Either l r) -> (l -> m o) -> (r -> m o) -> m o
meither a l r = a >>= either l r

tryEither :: Monad m => m (Either l r) -> (l -> m o) -> Cont (m o) r
tryEither m l = cont $ meither m l
-}

isPiOtherwise :: MonadTypeCheck ν m =>
                TermX ξ ν -> TypeErrored.Term ν -> m (TermX ξ ν)
isPiOtherwise t@(Pi _ _ _) _ = return t
isPiOtherwise _            e = throwError e

(~!) :: TermX ξ ν -> TypeErrored.Term ν
(~!) = unchecked

(!->) :: TypeChecked.Term ν -> TypeErrored.Term ν
(!->) = fromChecked

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

matchBinders :: Binder ν -> Binder ν -> Maybe (Binder ν)
matchBinders (Binder a) (Binder b) = case (a, b) of
  (Nothing, Nothing) -> Just (Binder Nothing)
  (Just  _, Nothing) -> Just (Binder a)
  (Nothing, Just  _) -> Just (Binder b)
  (Just va, Just vb) -> if va == vb then Just (Binder a) else Nothing

maybeFailWith :: MonadError e m => Maybe s -> e -> m s
(Just s) `maybeFailWith` _ = return s
Nothing  `maybeFailWith` e = throwError e

ifFalseFailWith :: MonadError e m => Bool -> e -> m ()
True  `ifFalseFailWith` _ = return ()
False `ifFalseFailWith` e = throwError e

runSynth' ::
  (MonadTypeCheck ν m) =>
  LocalContext (TypeChecked ν) ν -> TermX ξ ν -> m (TypeChecked.Term ν)
runSynth' γ = \case

  App _ fun arg -> do
    -- synthesize a type for fun
    sFun <- synthM γ fun
           (\ fFun -> App (Left AppFunctionFailed) fFun ((~!) arg))
    -- check that this type is a π-type : (binder : τIn) -> τOut binder
    Pi _ (Binder binder) τIn τOut <-
      typeOf sFun `isPiOtherwise`
      (App (Left (AppFunctionTypeFailed (raw fun))) ((!->) sFun) ((~!) arg))
    -- check that arg has the type τIn
    cArg <- checkM γ arg τIn
           (\ fArg -> App (Left AppArgumentFailed) ((!->) sFun) fArg)
    -- perform substitution if needed
    case binder of
      Just name -> do
        let τOut' = subst name cArg τOut
        return $ App τOut' sFun cArg
      Nothing ->
        return $ App τOut sFun cArg

  Var _ name ->
    case lookupType name γ of
    Nothing -> throwError $ Var (Left $ UnboundVariable name) name
    Just τ -> return $ Var τ name

  Pi _ binderPi τIn τOut -> do
    τIn' <- checkM γ τIn (Type () :: Raw.Term)
           (\ _τIn' -> error "TODO qwer")
    let γ' = addLocalAssum (binderPi, τIn') γ
    τOut' <- checkM γ' τOut (Type () :: Raw.Term)
            (\ _τOut' -> error "TODO adsf")
    return $ Pi (Type ()) binderPi τIn' τOut'

  Type _ -> return $ Type ()

  Lam _ b t -> throwError $ Lam (Left SynthesizeLambda) b ((~!) t)

  term -> throwError $ unchecked term

runCheck' ::
  (MonadTypeCheck ν m) =>
  LocalContext (TypeChecked ν) ν -> TermX ξ ν -> TermX ψ ν -> m (TypeChecked.Term ν)
runCheck' γ t τ = case t of

  Lam _ binderLam bodyLam -> do
    τ' <- checkM γ τ (Type () :: Raw.Type)
         (\ _τ' -> annotateError NotAType t)
    Pi _ binderPi τIn τOut <-
      redβ τ' `isPiOtherwise`
      annotateError (error "YOLO") t
    _ <- matchBinders binderLam binderPi
        `maybeFailWith` annotateError (error . show $ binderPi) t
    let γ' = addLocalAssum (binderLam, τIn) γ
    bodyLam' <- checkM γ' bodyLam τOut
        (\ _t' -> error "TODOCACA")
    return $ Lam τ' binderLam bodyLam'

  Hole _ -> error "runCheck Hole"

  -- conversion rule
  _ -> do
    t' <- synthM γ t (\ t' -> t')
    () <- (typeOf t' `eqβ` τ) `ifFalseFailWith`
        annotateError IncompatibleTypes t
    return t'

runTypeCheckerF ::
  TypeCheckerF ν (TypeCheckerT ν (TCMonad ν) (TypeChecked.Term ν)) ->
  TCMonad ν (TypeChecked.Term ν)
runTypeCheckerF ff = case ff of
  Failure f     -> throwError f
  Success s     -> return s
  Synth γ t k   -> join $ runTypeCheckerT . k . Right <$> runSynth' γ t
  Check γ t τ k -> join $ runTypeCheckerT . k . Right <$> runCheck' γ t τ

runFreeF ::
  FreeF (TypeCheckerF ν) (TypeChecked.Term ν)
  (TypeCheckerT ν (TCMonad ν) (TypeChecked.Term ν)) ->
  TCMonad ν (TypeChecked.Term ν)
runFreeF = \case
  Pure pp -> return pp
  Free ff -> runTypeCheckerF ff

runTypeCheckerT ::
  TypeCheckerT ν (TCMonad ν) (TypeChecked.Term ν) ->
  TCMonad ν (TypeChecked.Term ν)
runTypeCheckerT = runFreeT >=> runFreeF

runTypeCheckerF' ::
  TypeCheckerF ν (TCMonad ν (TypeChecked.Term ν)) ->
  TCMonad ν (TypeChecked.Term ν)
runTypeCheckerF' ff = case ff of
  Failure f     -> throwError f
  Success s     -> return s
  Synth γ t k   -> join $ k . Right <$> runSynth' γ t
  Check γ t τ k -> join $ k . Right <$> runCheck' γ t τ

runFreeF' ::
  FreeF (TypeCheckerF ν) (TypeChecked.Term ν)
  (TCMonad ν (TypeChecked.Term ν)) ->
  TCMonad ν (TypeChecked.Term ν)
runFreeF' = \case
  Pure pp -> return pp
  Free ff -> runTypeCheckerF' ff

stepTypeCheckerF ::
  TypeCheckerF ν (TCMonad ν (TypeChecked.Term ν)) ->
  TypeCheckerF ν (TCMonad ν (TypeChecked.Term ν))
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
