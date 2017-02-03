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

import RawTerm
import Term
import TypeCheckedTerm
import TypeCheckingFailure
import TypeErroredTerm

type TypeCheckingTerm = Either TypeErroredTerm TypeCheckedTerm

data TypeCheckerF k
  = Check RawTerm RawType (TypeCheckingTerm -> k)
  | Synth RawTerm         (TypeCheckingTerm -> k)
  | Failure TypeErroredTerm
  | Success TypeCheckedTerm
  deriving (Functor)

tcTrace :: (TypeCheckerF k -> TypeCheckerF k) -> TypeCheckerF k -> [TypeCheckerF k]
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

instance Show (TypeCheckerF k) where
  show (Check t τ _) = printf "Check %v %v" t τ
  show (Synth t _)   = printf "Synth %v" t
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

check :: MonadTypeCheck m =>
        TermX ξ -> TypeX ψ -> (TypeErroredTerm -> TypeErroredTerm) ->
        m TypeCheckedTerm
check t τ h = wrap $ Check (raw t) (raw τ) (either (throwError . h) return)

synth :: MonadTypeCheck m =>
        TermX ξ -> (TypeErroredTerm -> TypeErroredTerm) ->
        m TypeCheckedTerm
synth t h = wrap $ Synth (raw t) (either (throwError . h) return)

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

(~!) :: ForallX (ConvertTo TypeError) ξ => TermX ξ -> TypeErroredTerm
(~!) = unchecked

(!->) :: TypeCheckedTerm -> TypeErroredTerm
(!->) = fromChecked

synthApp ::
  ( MonadTypeCheck m
    -- not sure we'll ever need this modularity, but:
  , ForallX (ConvertTo TypeError) ξ
  , ForallX (ConvertTo TypeError) ψ
  ) =>
  TermX ξ -> TermX ψ -> m TypeCheckedTerm
synthApp fun arg = do
  -- synthesize a type for fun
  sFun <- synth fun
         (\ fFun -> App (Right AppFunctionFailed) fFun ((~!) arg))
  -- check that this type is a π-type : (binder : τIn) -> τOut binder
  Pi _ binder τIn τOut <-
    sFun `isPiOtherwise`
    (App (Right (AppFunctionTypeFailed (raw fun))) ((!->) sFun) ((~!) arg))
  -- check that arg has the type τIn
  sArg <- check arg τIn
        (\ fArg -> App (Right AppArgumentFailed) ((!->) sFun) fArg)
  -- perform substitution if needed
  case binder of
    Just _name ->
      let τOut' = τOut in -- substitute name arg τOut
      return $ App (raw τOut') sFun sArg
    Nothing ->
      return $ App (raw τOut) sFun sArg

{-
runSynth :: RawTerm -> TypeCheckingTerm
runSynth = \case
  App () fun arg -> _ $ (synthApp fun arg :: TypeCheckMonad TypeCheckedTerm)
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
      Synth t k ->
        interpret (k (_ t))

-}

steppy ::
  ( MonadError TypeErroredTerm m
  , MonadFree TypeCheckerF m
  ) =>
  TypeCheckerF TypeCheckedTerm -> m TypeCheckedTerm
steppy = \case
  Success s -> return s
  Failure f -> throwError f
  Synth (App _ fun arg) k -> k . Right <$> synthApp fun arg
  _ -> undefined

{-
doIt :: TypeCheckerF TypeCheckedTerm -> TypeCheckingTerm
doIt = \case
  Success s -> Right s
  Failure f -> Left f
  Synth (App _ fun arg) k ->
    let x = _ $ runExceptT $
            --(synthApp fun arg) in
            (synthApp fun arg :: ExceptT TypeErroredTerm (Free TypeCheckerF) TypeCheckedTerm) in
    let y = runFree x in
    case y of
    Pure p -> Right $ k p
    Free f ->
      _
  _ -> undefined
-}

runFreeTypeCheckerT :: TypeCheckerT m a ->
                      m (FreeF TypeCheckerF a (TypeCheckerT m a))
runFreeTypeCheckerT = runFreeT


-- runFreeT :: FreeT TypeCheckerF m a ->
--             m (FreeF TypeCheckerF a (FreeT TypeCheckerF m a))

-- runExceptT :: ExceptT e m a -> m (Either e a)

{-
runSynthApp1 ::
  ( ForallX (ConvertTo TypeError) ξ
  , ForallX (ConvertTo TypeError) ψ
  ) =>
  TermX ξ -> TermX ψ ->
  FreeF TypeCheckerF
  (Either TypeErroredTerm TypeCheckedTerm)
  (TypeCheckerT Identity (Either TypeErroredTerm TypeCheckedTerm))
runSynthApp1 a b = runIdentity . runFreeT . runExceptT $ synthApp a b
-}

{-
type TypeCheckMonad a = ExceptT TypeErroredTerm (Free TypeCheckerF) a
-- is equivalent to:
--type TypeCheckMonad a = ExceptT TypeErroredTerm (TypeCheckerT Identity) a
-- since:
-- TypeCheckerT = FreeT TypeCheckerF
-- and:
-- Free f = FreeT f Identity
runTypeCheck ::
  TypeCheckMonad a ->
  FreeF TypeCheckerF (Either TypeErroredTerm a)
  (TypeCheckerT Identity (Either TypeErroredTerm a))
runTypeCheck = runIdentity . runFreeT . runExceptT
-}



{-
typeCheck :: TypeCheckMonad TypeCheckedTerm ->
            Either TypeErroredTerm TypeCheckedTerm
typeCheck tc =
  case runTypeCheck tc of
  Pure result -> result
  Free task ->
    -- task   :: TypeCheckerF (TypeCheckerT Identity TypeCheckingTerm)
    -- steppy :: TypeCheckerF TypeCheckedTerm                          -> ...
    let _ = steppy task in
    _
-}






type TypeCheckMonad2 = TypeCheckerT (ExceptT TypeErroredTerm Identity)

runSynthApp2 ::
  ( ForallX (ConvertTo TypeError) ξ
  , ForallX (ConvertTo TypeError) ψ
  ) =>
  TermX ξ -> TermX ψ ->
  Either TypeErroredTerm (FreeF TypeCheckerF TypeCheckedTerm
                          (TypeCheckMonad2 TypeCheckedTerm))
runSynthApp2 a b = runIdentity . runExceptT . runFreeT $ synthApp a b

runTypeCheck2 ::
  TypeCheckMonad2 a ->
  Either TypeErroredTerm
  (FreeF TypeCheckerF a (TypeCheckMonad2 a))
runTypeCheck2 = runIdentity . runExceptT . runFreeT


{-
hole :: TypeCheckMonad2 TypeCheckedTerm -> TypeCheckingTerm
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
  App () fun arg -> hole $ (synthApp fun arg) -- :: TypeCheckMonad2 TypeCheckedTerm)
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

matchBinders :: Maybe Name -> Maybe Name -> Maybe (Maybe Name)
matchBinders Nothing  Nothing  = Just Nothing
matchBinders a        Nothing  = Just $ a
matchBinders Nothing  b        = Just $ b
matchBinders (Just a) (Just b) =
  if a == b then Just $ Just a else Nothing

maybeFailWith :: MonadError e m => Maybe s -> e -> m s
(Just s) `maybeFailWith` _ = return s
Nothing  `maybeFailWith` e = throwError e

runSynth' ::
  ( MonadTypeCheck m
    -- not sure we'll ever need this modularity, but:
  , ForallX (ConvertTo TypeError) ξ
  ) =>
  TermX ξ -> m TypeCheckedTerm
runSynth' t = case t of

  App _ fun arg -> do
    -- synthesize a type for fun
    sFun <- synth fun
           (\ fFun -> App (Right AppFunctionFailed) fFun ((~!) arg))
    -- check that this type is a π-type : (binder : τIn) -> τOut binder
    Pi _ binder τIn τOut <-
      sFun `isPiOtherwise`
      (App (Right (AppFunctionTypeFailed (raw fun))) ((!->) sFun) ((~!) arg))
    -- check that arg has the type τIn
    sArg <- check arg τIn
           (\ fArg -> App (Right AppArgumentFailed) ((!->) sFun) fArg)
    -- perform substitution if needed
    case binder of
      Just _name ->
        let τOut' = τOut in -- substitute name arg τOut
        return $ App (raw τOut') sFun sArg
      Nothing ->
        return $ App (raw τOut) sFun sArg

  _ -> error "TODO: runSynth'"

runCheck' ::
  ( MonadTypeCheck m
    -- not sure we'll ever need this modularity, but:
  , ForallX (ConvertTo TypeError) ξ
  , ForallX (ConvertTo TypeError) ψ
  ) =>
  TermX ξ -> TermX ψ -> m TypeCheckedTerm
runCheck' t τ = case t of

  Lam _ binderLam bodyLam -> do
    τ' <- check τ (Type () :: RawType)
         (\ τ' -> annotateError (Right NotAType) t)
    Pi _ binderPi τIn τOut <-
      redβ τ' `isPiOtherwise`
      annotateError (Right TODO) t
    mn <- matchBinders binderLam binderPi
         `maybeFailWith` annotateError (Right TODO) t
    -- let γ' = (n, σ) +: γ
    bodyLam' <- check bodyLam τ'
        (\ t' -> error "TODO")
    return $ Lam (raw τ') binderLam bodyLam'

  Hole _ -> error "runCheck Hole"

  -- conversion rule
  _ -> do
    sτ <- synth t id
    if sτ `eqβ` τ
      then error "conversion true" --undefined t sτ
      else throwError $ annotateError (Right IncompatibleTypes) t

{-
runSynth' :: RawTerm -> TypeCheckMonad2 TypeCheckedTerm
runSynth' = \case
  App () fun arg -> do
    x <- runExceptT $ synthApp fun arg
    case x of
      Left l  -> throwError l
      Right r -> return r
  t -> throwError $ unchecked t
-}

interpret' :: TypeCheckerT TypeCheckMonad2 TypeCheckedTerm ->
             TypeCheckMonad2 TypeCheckedTerm
interpret' thing = do
  runFreeT thing >>= \case
    Pure pp -> return pp
    Free ff ->
      case ff of
      Failure f   -> throwError f
      Success s   -> return s
      Synth t k   -> join $ interpret' . k . Right <$> runSynth' t
      Check t τ k -> join $ interpret' . k . Right <$> runCheck' t τ


--foo = runTypeCheck2 $ runSynth' rawType



steppy2 :: MonadTypeCheck m =>
          TypeCheckerF TypeCheckedTerm -> m TypeCheckedTerm
steppy2 = \case
  Success s -> return s
  Failure f -> throwError f
  Synth (App _ fun arg) k -> k . Right <$> synthApp fun arg
  _ -> undefined

{-
steppy :: MonadTypeCheck m =>
         TypeCheckerF TypeCheckingTerm -> m TypeCheckingTerm
steppy = \case

  Synth (App _ fun arg) k ->
    let free = runExceptT (synthApp fun arg)
          :: Free TypeCheckerF TypeCheckingTerm in
    case runFree free of
    Pure p -> return p
--    Free f -> do
      --let foo = steppy _
  --    return . k $ _
    --let foo = (runExceptT (synthApp fun arg) :: Free TypeCheckerF TypeCheckingTerm) in
    --return . k $ _
-}

    {-do
    fun' <- synth fun
    arg' <- check arg arg
    _-}

{-
stepTC
  :: Monad m =>
    TypeCheckerF (TypeCheckerT m) -> TypeCheckerT m
stepTC = \case

  Success t -> done d

  Synth (App _ fun arg) k ->
    let bad a b c = k . Left $ App a b c in
    flip runCont id $ do
      -- synthesize a type for fun
      fun' <- synth fun `or` (\ fun' ->
                               bad (Right AppFunctionFailed) fun' ((~!) arg))
      -- check that this type is a π-type : (binder : τIn) -> τOut binder
      Pi _ binder τIn τOut <-
        tryEither (assertPi fun')
        (\ () -> bad (Right $ AppFunctionTypeFailed (raw fun'))
               (fromChecked fun') (unchecked arg))
      -- check that arg has the type τIn
      arg' <- tryEither (check (raw arg) (raw τIn))
             (\arg' -> bad (Right AppArgumentFailed) (fromChecked fun') arg')
      -- return τOut [ binder <- arg ]
      return $ case binder of
        Just _name ->
          let τOut' = τOut in -- substitute name arg τOut
          k . Right $ App (raw τOut') fun' arg'
        Nothing ->
          k . Right $ App (raw τOut) fun' arg'
-}

{-
              Right arg' ->
                case binder of
                Just _name ->
                  let τOut' = τOut in -- substitute name arg τOut
                  k . Right $ App (raw τOut') fun' arg'
                Nothing ->
                  k . Right $ App (raw τOut) fun' arg'
              Left arg' ->
                k . Left $ App (Right AppArgumentFailed) (fromChecked fun') arg'
          _ -> k . Left $ App (Right $ AppFunctionTypeFailed (raw fun')) (fromChecked fun') (unchecked arg)
      Left fun' -> k . Left $ App (Right AppFunctionFailed) fun' (unchecked arg)
-}

{-
  -- synthesize a type for fun
  -- check that this type is a π-type : (binder : τIn) -> τOut binder
  -- check that arg has the type τIn
  -- return τOut [ binder <- arg ]
  Synth (App _ fun arg) k -> do
    synth fun >>= \case
      Right fun' -> do
        case fun' of
          Pi _ binder τIn τOut -> do
            check (raw arg) (raw τIn) >>= \case
              Right arg' ->
                case binder of
                Just _name ->
                  let τOut' = τOut in -- substitute name arg τOut
                  k . Right $ App (raw τOut') fun' arg'
                Nothing ->
                  k . Right $ App (raw τOut) fun' arg'
              Left arg' ->
                k . Left $ App (Right AppArgumentFailed) (fromChecked fun') arg'
          _ -> k . Left $ App (Right $ AppFunctionTypeFailed (raw fun')) (fromChecked fun') (unchecked arg)
      Left fun' -> k . Left $ App (Right AppFunctionFailed) fun' (unchecked arg)
-}
{-
stepTC
  :: Monad m =>
    TypeCheckerF (TypeCheckerT m) ->
    m (Work (TypeCheckerT m) TypeCheckingTerm)
stepTC = \case

  -- synthesize a type for fun
  -- check that this type is a π-type : (binder : τIn) -> τOut binder
  -- check that arg has the type τIn
  -- return τOut [ binder <- arg ]
  Synth (App _ fun arg) k ->
    return . Work $ do
      synth fun >>= \case
        Right fun' -> do
          case fun' of
            Pi _ binder τIn τOut -> do
              check (raw arg) (raw τIn) >>= \case
                Right arg' ->
                  case binder of
                  Just _name ->
                    let τOut' = τOut in -- substitute name arg τOut
                    k . Right $ App (raw τOut') fun' arg'
                  Nothing ->
                    k . Right $ App (raw τOut) fun' arg'
                Left arg' ->
                  k . Left $ App (Right AppArgumentFailed) (fromChecked fun') arg'
            _ -> k . Left $ App (Right $ AppFunctionTypeFailed (raw fun')) (fromChecked fun') (unchecked arg)
        Left fun' -> k . Left $ App (Right AppFunctionFailed) fun' (unchecked arg)
-}
{-
  Synth t k -> do
    --c1 <- check t t
    return . Work $ do
      k $ annotateWith (Left TODO) t

  Check t _τ k -> do
    return . Work $ do
      k $ annotateWith (Left TODO) t
-}

{-
  _ -> undefined
-}
{-
stepTC
  :: Monad m =>
    TypeCheckerF (TypeCheckerT m) ->
    m (Work (TypeCheckerT m) TypeAnnotatedTerm)
stepTC = \case

  Synth (Pi _ name τIn τOut) ->
    return . Work $ do
      aτIn <- check τIn set
      case getTypeAnnotation aτIn of
        Right (_, aτIn')

  Synth (App _ fun arg) k ->
    return . Work $ do
      aFun <- synth fun
      case getTypeAnnotation aFun of
        Right (_, τFun) -> do
          case τFun of
            Pi _ binder τIn τOut -> do
              aArg <- check arg τIn
              case getTypeAnnotation aArg of
                Right (_, _) ->
                  case binder of
                  Just _name ->
                    let τOut' = τOut in -- substitute name arg τOut
                    k $ App (Right (Nothing, τOut')) aFun aArg
                  Nothing ->
                    k $ App (Right (Nothing, τOut)) aFun aArg
                Left _ -> k $ App (Left AppArgumentFailed) aFun aArg
            _ -> k $ App (Left $ AppFunctionTypeFailed τFun) aFun (unchecked arg)
        Left _ -> k $ App (Left AppFunctionFailed) aFun (unchecked arg)

  Synth t k -> do
    --c1 <- check t t
    return . Work $ do
      k $ annotateWith (Left TODO) t

  Check t _τ k -> do
    return . Work $ do
      k $ annotateWith (Left TODO) t
-}

{-
tc
  :: Monad m =>
    TypeCheckerT m -> m (Work (TypeCheckerT m) TypeAnnotatedTerm)
tc mFreeTask = do
  runFreeT mFreeTask >>= \case
    Pure res -> return $ Done res
    Free task -> stepTC task
-}
