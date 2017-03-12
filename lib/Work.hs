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
--import Control.Monad.Managed
import Control.Monad.Trans.Free
--import Data.Default
import Prelude                       hiding (or)
--import Text.Printf
import Text.PrettyPrint.Annotated.WL

import Typing.LocalContext
import PrettyPrinting
import Term.Raw                      as Raw
import Term.Substitution
import Term.Term
import Term.TypeChecked              as TypeChecked
import Term.TypeErrored              as TypeErrored
import TypeCheckingFailure

type TypeCheckingTerm = Either TypeErrored.Term TypeChecked.Term

data TypeCheckerF k
  = Check (LocalContext TypeChecked) Raw.Term Raw.Type (TypeCheckingTerm -> k)
  | Synth (LocalContext TypeChecked) Raw.Term          (TypeCheckingTerm -> k)
  | Failure TypeErrored.Term
  | Success TypeChecked.Term
  deriving (Functor)

tcTrace ::
  (TypeCheckerF k -> TypeCheckerF k) -> TypeCheckerF k -> [TypeCheckerF k]
tcTrace _    (Failure f) = [Failure f]
tcTrace _    (Success s) = [Success s]
tcTrace step w           = w : tcTrace step (step w)

tc :: TypeCheckerF (TCMonad TypeChecked.Term) -> Either TypeErrored.Term TypeChecked.Term
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

prettyTypeCheckerF :: TypeCheckerF k -> Doc ()
prettyTypeCheckerF = \case
  Check _γ t τ _ -> fillSep
    [ text "Check"
    , text "γ ⊢"
    , prettyTermDoc' t
    , text ":"
    , prettyTermDoc' τ
    ]
  Synth _γ t _ -> fillSep
    [ text "Synth"
    , text "γ ⊢"
    , prettyTermDoc' t
    ]
  Failure f -> fillSep
    [ text "Failure"
    , prettyTermDoc' (raw f)
    ]
  Success s -> fillSep
    [ text "Success"
    , prettyTermDoc' (raw s)
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

type TypeCheckerT = FreeT TypeCheckerF

type MonadTypeCheck m =
  ( MonadError TypeErrored.Term m
  , MonadFree TypeCheckerF m
  )

checkF ::
  MonadError e m =>
  LocalContext TypeChecked -> TermX ξ -> TermX ψ -> (TypeErrored.Term -> e) ->
  TypeCheckerF (m TypeChecked.Term)
checkF γ t τ h = Check γ (raw t) (raw τ) (either (throwError . h) return)

checkM ::
  MonadTypeCheck m =>
  LocalContext TypeChecked -> TermX ξ -> TypeX ψ -> (TypeErrored.Term -> TypeErrored.Term) ->
  m TypeChecked.Term
checkM γ t τ h = wrap $ checkF γ t τ h

synthF ::
  MonadError e m =>
  LocalContext TypeChecked -> TermX ξ -> (TypeErrored.Term -> e) ->
  TypeCheckerF (m TypeChecked.Term)
synthF γ t h = Synth γ (raw t) (either (throwError . h) return)

synthM ::
  MonadTypeCheck m =>
  LocalContext TypeChecked -> TermX ξ -> (TypeErrored.Term -> TypeErrored.Term) ->
  m TypeChecked.Term
synthM γ t h = wrap $ synthF γ t h

failure :: MonadTypeCheck m => TypeErrored.Term -> m TypeChecked.Term
failure t = wrap $ Failure t

success :: MonadTypeCheck m => TypeChecked.Term -> m TypeChecked.Term
success t = wrap $ Success t

{-
synth :: Monad m => TermX ξ -> TypeCheckerT m
synth t = liftF $ Synth (raw t) id

check :: Monad m => TermX ξ -> TypeX ξ -> TypeCheckerT m
check t τ = liftF $ Check (raw t) (raw τ) id

done :: Monad m => TypeChecked.Term -> TypeCheckerT m
done r = liftF $ Done r
-}

rawType :: Raw.Term
rawType = Type ()

{-
meither :: Monad m => m (Either l r) -> (l -> m o) -> (r -> m o) -> m o
meither a l r = a >>= either l r

tryEither :: Monad m => m (Either l r) -> (l -> m o) -> Cont (m o) r
tryEither m l = cont $ meither m l
-}

isPiOtherwise :: MonadTypeCheck m =>
                TermX ξ -> TypeErrored.Term -> m (TermX ξ)
isPiOtherwise t@(Pi _ _ _ _) _ = return t
isPiOtherwise _              e = throwError e

(~!) :: TermX ξ -> TypeErrored.Term
(~!) = unchecked

(!->) :: TypeChecked.Term -> TypeErrored.Term
(!->) = fromChecked

runFreeTypeCheckerT :: TypeCheckerT m a ->
                      m (FreeF TypeCheckerF a (TypeCheckerT m a))
runFreeTypeCheckerT = runFreeT

-- TypeCheckerT = FreeT TypeCheckerF
type TCMonad = TypeCheckerT (ExceptT TypeErrored.Term Identity)

runTypeCheck2 ::
  TCMonad a ->
  Either TypeErrored.Term (FreeF TypeCheckerF a (TCMonad a))
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

eqβ :: TermX ξ -> TermX ψ -> Bool
_ `eqβ` _ = True -- TODO

redβ :: TermX ξ -> TermX ξ
redβ = id

matchBinders :: Binder -> Binder -> Maybe Binder
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
  (MonadTypeCheck m) =>
  LocalContext TypeChecked -> TermX ξ -> m TypeChecked.Term
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
  (MonadTypeCheck m) =>
  LocalContext TypeChecked -> TermX ξ -> TermX ψ -> m TypeChecked.Term
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
  TypeCheckerF (TypeCheckerT TCMonad TypeChecked.Term) ->
  TCMonad TypeChecked.Term
runTypeCheckerF ff = case ff of
  Failure f     -> throwError f
  Success s     -> return s
  Synth γ t k   -> join $ runTypeCheckerT . k . Right <$> runSynth' γ t
  Check γ t τ k -> join $ runTypeCheckerT . k . Right <$> runCheck' γ t τ

runFreeF ::
  FreeF TypeCheckerF TypeChecked.Term
  (TypeCheckerT TCMonad TypeChecked.Term) ->
  TCMonad TypeChecked.Term
runFreeF = \case
  Pure pp -> return pp
  Free ff -> runTypeCheckerF ff

runTypeCheckerT ::
  TypeCheckerT TCMonad TypeChecked.Term ->
  TCMonad TypeChecked.Term
runTypeCheckerT = runFreeT >=> runFreeF

runTypeCheckerF' ::
  TypeCheckerF (TCMonad TypeChecked.Term) ->
  TCMonad TypeChecked.Term
runTypeCheckerF' ff = case ff of
  Failure f     -> throwError f
  Success s     -> return s
  Synth γ t k   -> join $ k . Right <$> runSynth' γ t
  Check γ t τ k -> join $ k . Right <$> runCheck' γ t τ

runFreeF' ::
  FreeF TypeCheckerF TypeChecked.Term
  (TCMonad TypeChecked.Term) ->
  TCMonad TypeChecked.Term
runFreeF' = \case
  Pure pp -> return pp
  Free ff -> runTypeCheckerF' ff

stepTypeCheckerF ::
  TypeCheckerF (TCMonad TypeChecked.Term) ->
  TypeCheckerF (TCMonad TypeChecked.Term)
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
