{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeOperators #-}

module Typing.LocalContextOps
  ( LocalContextOps(..)
  , addAssumption
  , freshBinder
  , getLocalContext
  , interpretLocalContextOps
  , lookupType
  , withAssumption
  ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.Internal
import           Control.Monad.Freer.State
import           Data.List
import           Data.Maybe (maybeToList)

import           Term.Binder
import           Term.Term
import qualified Term.TypeChecked as C
import qualified Term.TypeErrored as E
import           Term.Variable
import qualified Typing.LocalContext as LC

type Checked = C.Term Variable
type Ctxt    = LC.LocalContext (C.Checked Variable) Variable
type Error   = E.Term Variable

data LocalContextOps a where
  AddAssumption   :: Binder Variable -> Checked -> LocalContextOps ()
  Fresh           :: [Variable] ->                 LocalContextOps Variable
  GetLocalContext ::                               LocalContextOps Ctxt
  LookupType      :: Variable ->                   LocalContextOps Checked
  SetLocalContext :: Ctxt ->                       LocalContextOps ()

addAssumption :: Member LocalContextOps r => Binder Variable -> Checked -> Eff r ()
addAssumption b τ = send $ AddAssumption b τ

fresh :: Member LocalContextOps r => [Variable] -> Eff r Variable
fresh l = send $ Fresh l

freshBinder :: Member LocalContextOps r => [Binder Variable] -> Eff r (Binder Variable)
freshBinder l = Binder . Just <$> fresh (concatMap (maybeToList . unBinder) l)

getLocalContext :: Member LocalContextOps r => Eff r Ctxt
getLocalContext = send $ GetLocalContext

lookupType :: Member LocalContextOps r => Variable -> Eff r Checked
lookupType v = send $ LookupType v

setLocalContext :: Member LocalContextOps r => Ctxt -> Eff r ()
setLocalContext γ = send $ SetLocalContext γ

withAssumption ::
  -- ( Member Trace r
  ( Member LocalContextOps r
  ) => Binder Variable -> Checked -> Eff r a -> Eff r a
withAssumption b τ e = do
  γ   <- getLocalContext
  ()  <- addAssumption b τ
  -- trace $ printf "Added assumption %s : %s" (prettyStr b) (prettyStrU τ)
  res <- e
  -- trace "Removing assumption"
  ()  <- setLocalContext γ
  return res

-- | An infinite supply of valid identifiers
identifiers :: [String]
identifiers = [ c : s | s <- "" : identifiers, c <- ['a'..'z'] ]

interpretLocalContextOps :: forall r a.
  Member (Exc Error) r =>
  Eff (LocalContextOps ': r) a -> Eff (State Ctxt ': r) a
interpretLocalContextOps = replaceRelay return $ \case
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
