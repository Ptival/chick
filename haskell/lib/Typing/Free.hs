{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language TypeOperators #-}
{-# language TypeSynonymInstances #-}
{-# language RankNTypes #-}

module Typing.Free where

import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.Internal
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Trace
import           Data.Bifunctor
import           Data.List
import           Data.Maybe (maybeToList)
import           Text.Printf

import           PrettyPrinting.PrettyPrintable
import           PrettyPrinting.PrettyPrintableUnannotated
import           StandardLibrary
import           Term.Binder
--import qualified Term.Raw as Raw
import           Term.Term
import qualified Term.TypeChecked as C
import qualified Term.TypeErrored as E
import           Term.Variable
import           TypeCheckingFailure
import qualified Typing.GlobalEnvironment as GE
import qualified Typing.LocalContext as LC
import           Utils

type Checked = C.Term Variable
type Ctxt    = LC.LocalContext (C.Checked Variable) Variable
type Env     = GE.GlobalEnvironment (C.Checked Variable) Variable
type Error   = E.Term Variable
type Term α  = TermX α Variable

data WithLocalContext a where
  AddAssumption   :: Binder Variable -> Checked -> WithLocalContext ()
  Fresh           :: [Variable] ->                 WithLocalContext Variable
  GetLocalContext ::                               WithLocalContext Ctxt
  LookupType      :: Variable ->                   WithLocalContext Checked
  SetLocalContext :: Ctxt ->                       WithLocalContext ()

addAssumption :: Member WithLocalContext r => Binder Variable -> Checked -> Eff r ()
addAssumption b τ = send $ AddAssumption b τ

fresh :: Member WithLocalContext r => [Variable] -> Eff r Variable
fresh l = send $ Fresh l

freshBinder :: Member WithLocalContext r => [Binder Variable] -> Eff r (Binder Variable)
freshBinder l = Binder . Just <$> fresh (concatMap (maybeToList . unBinder) l)

getLocalContext :: Member WithLocalContext r => Eff r Ctxt
getLocalContext = send $ GetLocalContext

lookupType :: Member WithLocalContext r => Variable -> Eff r Checked
lookupType v = send $ LookupType v

setLocalContext :: Member WithLocalContext r => Ctxt -> Eff r ()
setLocalContext γ = send $ SetLocalContext γ

withAssumption ::
  -- ( Member Trace r
  ( Member WithLocalContext r
  ) => Binder Variable -> Checked -> Eff r a -> Eff r a
withAssumption b τ e = do
  γ   <- getLocalContext
  ()  <- addAssumption b τ
  -- trace $ printf "Added assumption %s : %s" (prettyStr b) (prettyStrU τ)
  res <- e
  -- trace "Removing assumption"
  ()  <- setLocalContext γ
  return res

data TypeCheck a where
  Check :: Term α -> Term β -> TypeCheck Checked
  Synth :: Term α ->           TypeCheck Checked

check :: Member TypeCheck r => Term α -> Term β -> Eff r Checked
check t τ = send $ Check t τ

synth :: Member TypeCheck r => Term α -> Eff r Checked
synth t = send $ Synth t

(|||) :: Member (Exc Error) r => Eff r a -> (Error -> Error) -> Eff r a
(|||) e f = do
  e `catchError` (throwError . f)

(^||) :: Member (Exc Error) r => Maybe a -> Error -> Eff r a
(^||) m e = maybe (throwError e) pure m

(?||) :: Member (Exc Error) r => Bool -> Error -> Eff r ()
(?||) b e = if b then return () else throwError e

-- | Stands for "not checked lambda"
(~!\) :: NameScope (TermX ν) Variable -> NameScope (TermX (E.Annotation Variable)) Variable
(~!\) s =
  let (b, t) = unscopeTerm s in
  abstractBinder b ((~!) t)

-- | Stands for "not checked"
(~!) :: TermX α ν -> E.Term ν
(~!) = first (const (Left Unchecked))

-- | Stands for "checked"
(!) :: C.Term ν -> E.Term ν
(!) = first Right

eqβ :: TermX α ν -> TermX β ν -> Bool
_t1 `eqβ` _t2 = True

handleCheck ::
  ( Member (Exc Error)      r
  --, Member Trace            r
  , Member TypeCheck        r
  , Member WithLocalContext r
  ) => Term α -> Term β -> Eff r Checked
handleCheck t τ = case t of

  Annot _ t' τ' -> do
    t'' <- check t' τ'  ||| \ _t'' -> E.annotateError TODO t
    τ'' <- C.typeOf t'' ^||           E.annotateError TODO t
    ()  <- τ'' `eqβ` τ  ?||           E.annotateError TODO t
    return $ Annot (C.Checked τ'') t'' τ''

  Lam _ bt -> do
    let (bLam, tLam) = unscopeTerm bt
    τ'             <- check τ  Type ||| \ _τ' -> E.annotateError NotAType t
    Pi _ τIn bτOut <- isPi  τ'      ^||          E.annotateError TODO t
    let (bOut, τOut) = unscopeTerm bτOut
    -- the binders at the type- and term- level may differ, reconcile them
    binder         <- freshBinder [bLam, bOut]
    tLam'          <-
      withAssumption binder τIn $ do
        check tLam τOut               ||| \ _t' -> E.annotateError TODO t
    return $ Lam (C.Checked τ') (abstractBinder bLam tLam')

  _ -> do -- conversion rule
    t' <- synth t     ||| id
    τ' <- C.typeOf t' ^|| E.annotateError TODO t'
    () <- τ' `eqβ` τ  ?|| E.annotateError IncompatibleTypes t
    return t'

handleSynth ::
  ( Member (Exc Error)      r
  -- , Member Trace            r
  , Member TypeCheck        r
  , Member WithLocalContext r
  ) => Term α -> Eff r Checked
handleSynth t = case t of

  Annot _ t' τ -> do
    t'' <- check t' τ   ||| \ _t'' -> E.annotateError TODO t
    τ'  <- C.typeOf t'' ^||           E.annotateError TODO t
    return $ Annot (C.Checked τ') t'' τ'

  App _ fun arg -> do
    sFun           <- synth    fun      ||| \ fFun -> sadAppFun     fFun       ((~!) arg)
    τFun           <- C.typeOf sFun     ^||           sadAppFun     ((!) sFun) ((~!) arg)
    Pi _ τIn bτOut <- isPi     τFun     ^||           sadAppFunType ((!) sFun) ((~!) arg)
    cArg           <- check    arg  τIn ||| \ fArg -> sadAppArg     ((!) sFun) fArg
    let (b, τOut) = unscopeTerm bτOut
    let τOut' = case unBinder b of
          Nothing -> τOut
          Just v  -> substitute v cArg τOut
    return $ App (C.Checked τOut') sFun cArg

  Lam _ bt -> throwError $ Lam (Left SynthesizeLambda) ((~!\) bt)

  Pi _ τIn bτOut -> do
    let (b, τOut) = unscopeTerm bτOut
    τIn'  <- check τIn Type       ||| \ fτIn  -> sadPiTODO fτIn       ((~!\) bτOut)
    τOut' <-
      withAssumption b τIn' $ do
      check τOut Type             ||| \ fτOut -> sadPiTODO ((!) τIn') (abstractBinder b fτOut)
    return $ Pi (C.Checked Type) τIn' (abstractBinder b τOut')

  Type -> pure $ Type

  Var _ name -> do
    γ <- getLocalContext
    --trace $ printf "Synthesizing %s in context %s" (prettyStr name) (prettyStrU γ)
    τ <- lookupType name
    return $ Var (Just (C.Checked τ)) name

  _ -> throwError $ (~!) t

-- | An infinite supply of valid identifiers
identifiers :: [String]
identifiers = [ c : s | s <- "" : identifiers, c <- ['a'..'z'] ]

interpretWithLocalContext :: forall r a.
  Member (Exc Error) r =>
  Eff (WithLocalContext ': r) a -> Eff (State Ctxt ': r) a
interpretWithLocalContext = replaceRelay return $ \case
  -- (>>=) $ foo   stands for   \ arr -> foo >>= arr
  AddAssumption   b τ -> (>>=) $ modify (LC.addLocalAssum (b, τ))
  Fresh           s   -> (>>=) $ interpretFresh s
  GetLocalContext     -> (>>=) $ get
  LookupType      v   -> (>>=) $ interpretLookup v
  SetLocalContext γ   -> (>>=) $ put γ
  where
    interpretFresh suggestions = do
      γ :: Ctxt <- get
      let candidates = suggestions ++ (Variable <$> identifiers)
      return $ head $ candidates \\ LC.boundNames γ
    interpretLookup v = do
      γ :: Ctxt <- get
      case LC.lookupType v γ of
        Nothing -> (throwError :: Error -> Eff (State Ctxt ': r) x) $ Var Nothing v
        Just τ  -> return τ

type Dummy a b = a ': b

{-
traceTypeCheck ::
  ( Member Trace r
  , Member TypeCheck r
  , Member WithLocalContext r
  ) => Eff r a -> Eff r a
traceTypeCheck = interpose pure $ \case
  Check t τ -> \ arr -> do
    γ <- getLocalContext
    trace $ printf "Checking (%s : %s) in context %s" (prettyStrU t) (prettyStrU τ) (prettyStrU γ)
    check t τ >>= traceTypeCheck . arr
  Synth t   -> \ arr -> do
    γ <- getLocalContext
    trace $ printf "Synthesizing %s in context %s" (prettyStrU t) (prettyStrU γ)
    synth t >>= traceTypeCheck . arr
-}

runTraceTypeCheck ::
  ( Member (Exc Error) r
  , Member Trace r
  , Member WithLocalContext r
  ) =>
  Eff (TypeCheck ': r) a -> Eff r a
runTraceTypeCheck = handleRelay pure $ \case
  Check t τ -> \ arr -> do
    γ <- getLocalContext
    trace $ printf "Checking (%s : %s) in context:\n%s" (prettyStrU t) (prettyStrU τ) (prettyStrU γ)
    runTraceTypeCheck (handleCheck t τ) >>= arr
  Synth t   -> \ arr -> do
    γ <- getLocalContext
    trace $ printf "Synthesizing %s in context:\n%s" (prettyStrU t) (prettyStrU γ)
    runTraceTypeCheck (handleSynth t) >>= arr

runTypeCheck ::
  ( Member (Exc Error) r
  , Member Trace r
  , Member WithLocalContext r
  ) =>
  Eff (TypeCheck ': r) a -> Eff r a
runTypeCheck = handleRelay pure $ \case
  Check t τ -> (>>=) $ runTypeCheck $ handleCheck t τ
  Synth t   -> (>>=) $ runTypeCheck $ handleSynth t

runCheck :: Term α -> Term β -> Eff '[Trace] (Either Error (Checked, Ctxt))
runCheck t τ =
  runError
  . flip runState (LC.LocalContext [])
  . interpretWithLocalContext
  . runTypeCheck
  . runTraceTypeCheck
  $ check t τ

foo :: IO (Either Error (Checked, Ctxt))
foo = runTrace $ runCheck tId τId
