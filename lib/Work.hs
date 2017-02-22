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
import Prelude                  hiding (or)
import Text.Printf

import Context
import RawTerm
import Term
import Term.TypeCheckedTerm
import TypeCheckingFailure
import Term.TypeErroredTerm

type TypeCheckingTerm = Either TypeErroredTerm TypeCheckedTerm

data TypeCheckerF k
  = Check (Context Raw) RawTerm RawType (TypeCheckingTerm -> k)
  | Synth (Context Raw) RawTerm         (TypeCheckingTerm -> k)
  | Failure TypeErroredTerm
  | Success TypeCheckedTerm
  deriving (Functor)

tcTrace ::
  (TypeCheckerF k -> TypeCheckerF k) -> TypeCheckerF k -> [TypeCheckerF k]
tcTrace _    (Failure f) = [Failure f]
tcTrace _    (Success s) = [Success s]
tcTrace step w           = w : tcTrace step (step w)

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

{-
instance Functor TypeCheckerF where
  fmap f (Synth t k) = Synth t (f . k)
  fmap f (Check t τ k) = Check t τ (f . k)
  --fmap f (Done r) = Done r
-}

type TypeCheckerT = FreeT TypeCheckerF

type MonadTypeCheck m =
  ( MonadError TypeErroredTerm m
  , MonadFree TypeCheckerF m
  )

checkF :: MonadError e m =>
         Context Raw -> TermX ξ -> TermX ψ -> (TypeErroredTerm -> e) ->
         TypeCheckerF (m TypeCheckedTerm)
checkF γ t τ h = Check γ (raw t) (raw τ) (either (throwError . h) return)

checkM :: MonadTypeCheck m =>
         Context Raw -> TermX ξ -> TypeX ψ ->
         (TypeErroredTerm -> TypeErroredTerm) ->
         m TypeCheckedTerm
checkM γ t τ h = wrap $ checkF γ t τ h

synth :: MonadTypeCheck m =>
        Context Raw -> TermX ξ -> (TypeErroredTerm -> TypeErroredTerm) ->
        m TypeCheckedTerm
synth γ t h = wrap $ Synth γ (raw t) (either (throwError . h) return)

failure :: MonadTypeCheck m => TypeErroredTerm -> m TypeCheckedTerm
failure t = wrap $ Failure t

success :: MonadTypeCheck m => TypeCheckedTerm -> m TypeCheckedTerm
success t = wrap $ Success t

{-
synth :: Monad m => TermX ξ -> TypeCheckerT m
synth t = liftF $ Synth (raw t) id

check :: Monad m => TermX ξ -> TypeX ξ -> TypeCheckerT m
check t τ = liftF $ Check (raw t) (raw τ) id

done :: Monad m => TypeCheckedTerm -> TypeCheckerT m
done r = liftF $ Done r
-}

rawType :: RawTerm
rawType = Type ()

{-
meither :: Monad m => m (Either l r) -> (l -> m o) -> (r -> m o) -> m o
meither a l r = a >>= either l r

tryEither :: Monad m => m (Either l r) -> (l -> m o) -> Cont (m o) r
tryEither m l = cont $ meither m l
-}

isPiOtherwise :: MonadTypeCheck m =>
                TermX ξ -> TypeErroredTerm -> m (TermX ξ)
isPiOtherwise t@(Pi _ _ _ _) _ = return t
isPiOtherwise _              e = throwError e

(~!) :: TermX ξ -> TypeErroredTerm
(~!) = unchecked

(!->) :: TypeCheckedTerm -> TypeErroredTerm
(!->) = fromChecked

runFreeTypeCheckerT :: TypeCheckerT m a ->
                      m (FreeF TypeCheckerF a (TypeCheckerT m a))
runFreeTypeCheckerT = runFreeT

-- TypeCheckerT = FreeT TypeCheckerF
type TCMonad = TypeCheckerT (ExceptT TypeErroredTerm Identity)

runTypeCheck2 ::
  TCMonad a ->
  Either TypeErroredTerm (FreeF TypeCheckerF a (TCMonad a))
runTypeCheck2 = runIdentity . runExceptT . runFreeT


{-
hole :: TCMonad TypeCheckedTerm -> TypeCheckingTerm
hole m =
  case runTypeCheck2 m of
  Left l -> Left l
  Right r ->
    case r of
    Pure t -> Right t
    Free f ->
      case f of
      Synth t k -> hole . k . runSynth $ t

runSynth :: RawTerm -> TypeCheckingTerm
runSynth = \case
  App () fun arg -> hole $ (synthApp fun arg) -- :: TCMonad TypeCheckedTerm)
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
  Context Raw -> TermX ξ -> m TypeCheckedTerm
runSynth' γ t = case t of

  App _ fun arg -> do
    -- synthesize a type for fun
    sFun <- synth γ fun
           (\ fFun -> App (Right AppFunctionFailed) fFun ((~!) arg))
    -- check that this type is a π-type : (binder : τIn) -> τOut binder
    Pi _ (Binder binder) τIn τOut <-
      typeOf sFun `isPiOtherwise`
      (App (Right (AppFunctionTypeFailed (raw fun))) ((!->) sFun) ((~!) arg))
    -- check that arg has the type τIn
    sArg <- checkM γ arg τIn
           (\ fArg -> App (Right AppArgumentFailed) ((!->) sFun) fArg)
    -- perform substitution if needed
    case binder of
      Just _name ->
        let τOut' = τOut in -- substitute name arg τOut
        return $ App (raw τOut') sFun sArg
      Nothing ->
        return $ App (raw τOut) sFun sArg

  Var _ name ->
    case lookup name γ of
    Nothing -> throwError $ Var (error "TODEEDOO") name
    Just τ -> return $ Var τ name

  Pi _ binderPi τIn τOut -> do
    τIn' <- checkM γ τIn (Type () :: RawTerm)
           (\ _τIn' -> error "TODO qwer")
    let γ' = (binderPi, raw τIn') +: γ
    τOut' <- checkM γ' τOut (Type () :: RawTerm)
            (\ _τOut' -> error "TODO adsf")
    return $ Pi (Type ()) binderPi τIn' τOut'

  Type _ -> return $ Type (Type ())

  _ -> error "TODO: runSynth'"

runCheck' ::
  (MonadTypeCheck m) =>
  Context Raw -> TermX ξ -> TermX ψ -> m TypeCheckedTerm
runCheck' γ t τ = case t of

  Lam _ binderLam bodyLam -> do
    τ' <- checkM γ τ (Type () :: RawType)
         (\ _τ' -> annotateError (Right NotAType) t)
    Pi _ binderPi τIn τOut <-
      redβ τ' `isPiOtherwise`
      annotateError (Right (error "YOLO")) t
    _ <- matchBinders binderLam binderPi
        `maybeFailWith` annotateError (Right (error . show $ binderPi)) t
    let γ' = (binderLam, raw τIn) +: γ
    bodyLam' <- checkM γ' bodyLam τOut
        (\ _t' -> error "TODOCACA")
    return $ Lam (raw τ') binderLam bodyLam'

  Hole _ -> error "runCheck Hole"

  -- conversion rule
  _ -> do
    t' <- synth γ t (\ t' -> t')
    () <- (typeOf t' `eqβ` τ) `ifFalseFailWith`
        annotateError (Right IncompatibleTypes) t
    return t'

runTypeCheckerF ::
  TypeCheckerF (TypeCheckerT TCMonad TypeCheckedTerm) ->
  TCMonad TypeCheckedTerm
runTypeCheckerF ff = case ff of
  Failure f     -> throwError f
  Success s     -> return s
  Synth γ t k   -> join $ runTypeCheckerT . k . Right <$> runSynth' γ t
  Check γ t τ k -> join $ runTypeCheckerT . k . Right <$> runCheck' γ t τ

runFreeF ::
  FreeF TypeCheckerF TypeCheckedTerm
  (TypeCheckerT TCMonad TypeCheckedTerm) ->
  TCMonad TypeCheckedTerm
runFreeF = \case
  Pure pp -> return pp
  Free ff -> runTypeCheckerF ff

runTypeCheckerT ::
  TypeCheckerT TCMonad TypeCheckedTerm ->
  TCMonad TypeCheckedTerm
runTypeCheckerT = runFreeT >=> runFreeF

runTypeCheckerF' ::
  TypeCheckerF (TCMonad TypeCheckedTerm) ->
  TCMonad TypeCheckedTerm
runTypeCheckerF' ff = case ff of
  Failure f     -> throwError f
  Success s     -> return s
  Synth γ t k   -> join $ k . Right <$> runSynth' γ t
  Check γ t τ k -> join $ k . Right <$> runCheck' γ t τ

runFreeF' ::
  FreeF TypeCheckerF TypeCheckedTerm
  (TCMonad TypeCheckedTerm) ->
  TCMonad TypeCheckedTerm
runFreeF' = \case
  Pure pp -> return pp
  Free ff -> runTypeCheckerF' ff

stepTypeCheckerF ::
  TypeCheckerF (TCMonad TypeCheckedTerm) ->
  TypeCheckerF (TCMonad TypeCheckedTerm)
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
