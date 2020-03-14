{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Typing.TypeCheckOps
  ( TypeCheckOps(..)
  , check
  , runTraceTypeCheckOps
  , runTypeCheckOps
  , synth
  ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.Trace
import           Text.Printf

import           Language
import           PrettyPrinting.PrettyPrintableUnannotated
import           PrettyPrinting.Term                       ()
import           Term.Binder
import           Term.Term
import qualified Term.TypeChecked                          as C
import qualified Term.TypeErrored                          as E
import qualified Term.Universe                             as U
import           TypeCheckingFailure
import           Typing.LocalContextOps
import           Typing.Utils
import           Utils

type Checked = C.Term Variable
type Error   = E.Term Variable
type Term α  = TermX α Variable
type Type α  = Term α

data TypeCheckOps a where
  Check :: Term α -> Type β -> TypeCheckOps Checked
  Synth :: Term α ->           TypeCheckOps Checked

check :: Member TypeCheckOps r => Term α -> Type β -> Eff r Checked
check t τ = send $ Check t τ

synth :: Member TypeCheckOps r => Term α -> Eff r Checked
synth t = send $ Synth t

-- | TODO: move this where it belongs and implement it
eqβ :: TermX α ν -> TermX β ν -> Bool
_t1 `eqβ` _t2 = True

handleCheck :: ∀ α β r.
  ( Member (Exc Error) r
  --, Member Trace r
  , Member TypeCheckOps r
  , Member (LocalContextOps (C.Checked Variable)) r
  ) => Term α -> Term β -> Eff r Checked
handleCheck t τ = case t of

  Annot _ t' τ' -> do
    t'' <- check t' τ'  ||| \ _t'' -> E.annotateError TODO t
    τ'' <- C.typeOf t'' ^||           E.annotateError TODO t
    ()  <- τ'' `eqβ` τ  ?||           E.annotateError TODO t
    return $ Annot (C.Checked τ'') t'' τ''

  Lam _ bt -> do
    let (bLam, tLam) = unscopeTerm bt
    τ'             <- check τ  (Type U.Type) ||| \ _τ' -> E.annotateError NotAType t
    (_,τIn, bτOut) <- isPi  τ'               ^||          E.annotateError TODO t
    let (bOut, τOut) = unscopeTerm bτOut
    -- the binders at the type- and term- level may differ, reconcile them
    binder         <- freshBinder @(C.Checked Variable) [bLam, bOut]
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
  ( Member (Exc Error) r
  -- , Member Trace r
  , Member TypeCheckOps r
  , Member (LocalContextOps (C.Checked Variable)) r
  ) => Term α -> Eff r Checked
handleSynth t = case t of

  Annot _ t' τ -> do
    t'' <- check t' τ   ||| \ _t'' -> E.annotateError TODO t
    τ'  <- C.typeOf t'' ^||           E.annotateError TODO t
    return $ Annot (C.Checked τ') t'' τ'

  App _ fun arg -> do
    sFun            <- synth    fun      ||| \ fFun -> sadAppFun     fFun       ((~!) arg)
    τFun            <- C.typeOf sFun     ^||           sadAppFun     ((!) sFun) ((~!) arg)
    (_, τIn, bτOut) <- isPi     τFun     ^||           sadAppFunType ((!) sFun) ((~!) arg)
    cArg            <- check    arg  τIn ||| \ fArg -> sadAppArg     ((!) sFun) fArg
    let (b, τOut) = unscopeTerm bτOut
    let τOut' = case unBinder b of
          Nothing -> τOut
          Just v  -> substitute v cArg τOut
    return $ App (C.Checked τOut') sFun cArg

  Lam _ bt -> throwError $ Lam (Left SynthesizeLambda) ((~!\) bt)

  Pi _ τIn bτOut -> do
    let (b, τOut) = unscopeTerm bτOut
    τIn'  <- check τIn (Type U.Type) ||| \ fτIn  -> sadPiTODO fτIn       ((~!\) bτOut)
    τOut' <-
      withAssumption b τIn' $ do
      check τOut (Type U.Type)       ||| \ fτOut -> sadPiTODO ((!) τIn') (abstractBinder b fτOut)
    return $ Pi (C.Checked (Type U.Type)) τIn' (abstractBinder b τOut')

  Type _ -> pure $ Type U.Type

  Var _ name -> do
    -- γ <- getLocalContext
    -- trace $ printf "Synthesizing %s in context %s" (prettyStr name) (prettyStrU γ)
    τ <- lookupType name
    return $ Var (Just (C.Checked τ)) name

  _ -> throwError $ (~!) t

runTraceTypeCheckOps ::
  ( Member (Exc Error) r
  , Member Trace r
  , Member (LocalContextOps (C.Checked Variable)) r
  ) =>
  Eff (TypeCheckOps ': r) a -> Eff r a
runTraceTypeCheckOps = handleRelay pure $ \case
  Check t τ -> \ arr -> do
    _γ <- getLocalContext @(C.Checked Variable)
    trace $ printf "Checking (%s : %s) in context:\n%s" (prettyStrU @'Chick t) (prettyStrU @'Chick τ)
      "TODO: Pretty-printing local context"
      -- (prettyStrU @Chick γ)
    runTraceTypeCheckOps (handleCheck t τ) >>= arr
  Synth t   -> \ arr -> do
    _γ <- getLocalContext @(C.Checked Variable)
    trace $ printf "Synthesizing %s in context:\n%s" (prettyStrU @'Chick t)
      "TODO: Pretty-printing local context"
      -- (prettyStrU @Chick γ)
    runTraceTypeCheckOps (handleSynth t) >>= arr

runTypeCheckOps ::
  ( Member (Exc Error) r
  , Member Trace r
  , Member (LocalContextOps (C.Checked Variable)) r
  ) =>
  Eff (TypeCheckOps ': r) a -> Eff r a
runTypeCheckOps = handleRelay pure $ \case
  Check t τ -> (>>=) $ runTypeCheckOps $ handleCheck t τ
  Synth t   -> (>>=) $ runTypeCheckOps $ handleSynth t
