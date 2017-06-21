{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language TemplateHaskell #-}
{-# language TypeSynonymInstances #-}
{-# language RankNTypes #-}

module Typing.Free where

import           Control.Applicative
-- import Control.Eff
-- import Control.Eff.Exception
--import Control.Monad.Free.TH
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Data.Bifunctor

import           Term.Binder
import qualified Term.Raw as Raw
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

{-
data TypeCheck a where
  Check :: Ctxt -> Term α -> Term β -> TypeCheck (Either Error Checked)
  Synth :: Ctxt -> Term α ->           TypeCheck (Either Error Checked)

data TypeCheckFail a where
  ThrowError :: Error -> TypeCheckFail a

check :: (Member TypeCheck r, Member TypeCheckFail r) => Ctxt -> Term α -> Term β -> Eff r Checked
check γ t τ = (send $ Check γ t τ) >>= either (send . ThrowError) pure

synth :: (Member TypeCheck r, Member TypeCheckFail r) => Ctxt -> Term α -> Eff r Checked
synth γ t = (send $ Synth γ t) >>= either (send . ThrowError) pure

throwError :: Member TypeCheckFail r => Error -> Eff r a
throwError e = send $ ThrowError e
-}

data WithLocalContext a where
  AddAssumption :: Binder Variable -> Checked -> WithLocalContext ()
  LookupType :: Variable -> WithLocalContext Checked

addAssumption :: Member WithLocalContext r => Binder Variable -> Checked -> Eff r ()
addAssumption b τ = send $ AddAssumption b τ

lookupType :: Member WithLocalContext r => Variable -> Eff r Checked
lookupType v = send $ LookupType v

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
_ `eqβ` _ = error "TODO: eqβ"

handleCheck ::
  ( Member WithLocalContext r
  , Member TypeCheck r
  , Member (Exc Error) r
  ) => Term α -> Term β -> Eff r Checked
handleCheck t τ = case t of

  Lam _ bt -> do
    let (bLam, tLam) = unscopeTerm bt
    τ'             <- check τ  Type          ||| \ _τ' -> E.annotateError NotAType t
    Pi _ τIn bτOut <- isPi  τ'               ^||          E.annotateError TODO t
    let (bOut, τOut) = unscopeTerm bτOut
    ()             <- addAssumption bLam τIn
    tLam'          <- check tLam τOut        ||| \ _t' -> E.annotateError TODO t
    return $ Lam (C.Checked τ') (abstractBinder bLam tLam')

  _ -> do -- conversion rule
    t' <- synth t     ||| id
    τ' <- C.typeOf t' ^|| E.annotateError TODO t'
    () <- τ' `eqβ` τ  ?|| E.annotateError IncompatibleTypes t
    return t'

handleSynth ::
  ( Member WithLocalContext r
  , Member TypeCheck r
  , Member (Exc Error) r
  ) => Term α -> Eff r Checked
handleSynth t = case t of

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
    τIn'  <- check τIn Type       ||| (const $ error TODO)
    ()    <- addAssumption b τIn'
    τOut' <- check τOut Type      ||| (const $ error TODO)
    return $ Pi (C.Checked Type) τIn' (abstractBinder b τOut')

  Type -> pure $ Type

  Var name -> lookupType name

  _ -> throwError $ (~!) t
