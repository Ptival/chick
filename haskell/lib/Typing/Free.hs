{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language TypeOperators #-}
{-# language TypeSynonymInstances #-}
{-# language RankNTypes #-}

module Typing.Free
  ( traceCheck
  , runCheck
  ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.Internal
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Trace

import           Term.Term
import qualified Term.TypeChecked as C
import qualified Term.TypeErrored as E
import           Term.Variable
import qualified Typing.LocalContext as LC
import           Typing.LocalContextOps
import           Typing.TypeCheckOps

type Checked = C.Term Variable
type Ctxt    = LC.LocalContext (C.Checked Variable) Variable
type Error   = E.Term Variable
type Term α  = TermX α Variable

skipTrace :: Eff '[Trace] a -> a
skipTrace (Val x) = x
skipTrace (E u q) =
  case extract u of
    Trace _ -> skipTrace (qApp q ())

runCheck' :: Term α -> Term β -> Eff '[Trace] (Either Error (Checked, Ctxt))
runCheck' t τ =
  runError
  . flip runState (LC.LocalContext [])
  . interpretLocalContextOps
  -- . runTypeCheckOps
  . runTraceTypeCheckOps
  $ check t τ

runCheck :: Term α -> Term β -> Either Error (Checked, Ctxt)
runCheck t τ = skipTrace $ runCheck' t τ

-- runCheck :: Term α -> Term β -> Eff '[Trace] (Either Error (Checked, Ctxt))
traceCheck :: Term α -> Term β -> IO (Either Error (Checked, Ctxt))
traceCheck t τ = runTrace $ runCheck' t τ
