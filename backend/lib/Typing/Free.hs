{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Typing.Free (
  traceCheck,
  runCheck,
  ) where

import           Polysemy
import           Polysemy.Error
import           Polysemy.State
import           Polysemy.Trace

import           Term.Term
import qualified Term.TypeChecked as C
import qualified Term.TypeErrored as E
import qualified Typing.LocalContext as LC
import           Typing.LocalContextOps
import           Typing.TypeCheckOps
import           Utils

import Examples

type Checked = C.Term Variable
type Ctxt    = LC.LocalContext (C.Checked Variable) Variable
type Error   = E.Term Variable
type Term α  = TermX α Variable

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

traceCheck :: Term α -> Term β -> IO (Either Error (Checked, Ctxt))
traceCheck t τ = runTrace $ runCheck' t τ

test = traceCheck tFlip τFlip
