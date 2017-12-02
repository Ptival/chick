{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

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
import qualified Term.TypeErrored as E
import qualified Typing.LocalContext as LC

type Term α  = TermX α Variable
--type Checked = C.Term Variable
type Ctxt α  = LC.LocalContext α Variable
type Error   = E.Term Variable

data LocalContextOps α a where
  AddAssumption   :: Binder Variable -> Term α -> LocalContextOps α ()
  Fresh           :: [Variable] ->                LocalContextOps α Variable
  GetLocalContext ::                              LocalContextOps α (Ctxt α)
  LookupType      :: Variable ->                  LocalContextOps α (Term α)
  SetLocalContext :: Ctxt α ->                    LocalContextOps α ()

addAssumption :: Member (LocalContextOps α) r => Binder Variable -> Term α -> Eff r ()
addAssumption b τ = send $ AddAssumption b τ

fresh :: ∀ α r. Member (LocalContextOps α) r => [Variable] -> Eff r Variable
fresh l = send $ Fresh @α l

freshBinder :: ∀ α r. Member (LocalContextOps α) r => [Binder Variable] -> Eff r (Binder Variable)
freshBinder l = Binder . Just <$> fresh @α (concatMap (maybeToList . unBinder) l)

getLocalContext :: Member (LocalContextOps α) r => Eff r (Ctxt α)
getLocalContext = send $ GetLocalContext

lookupType :: ∀ α r. Member (LocalContextOps α) r => Variable -> Eff r (Term α)
lookupType v = send $ LookupType @α v

setLocalContext :: Member (LocalContextOps α) r => Ctxt α -> Eff r ()
setLocalContext γ = send $ SetLocalContext γ

withAssumption :: ∀ α r a.
  -- ( Member Trace r
  ( Member (LocalContextOps α) r
  ) => Binder Variable -> Term α -> Eff r a -> Eff r a
withAssumption b τ e = do
  γ   <- getLocalContext @α
  ()  <- addAssumption @α b τ
  -- trace $ printf "Added assumption %s : %s" (prettyStr b) (prettyStrU τ)
  res <- e
  -- trace "Removing assumption"
  ()  <- setLocalContext γ
  return res

-- | An infinite supply of valid identifiers
identifiers :: [String]
identifiers = [ c : s | s <- "" : identifiers, c <- ['a'..'z'] ]

interpretLocalContextOps :: ∀ α r a.
  Member (Exc Error) r =>
  Eff (LocalContextOps α ': r) a -> Eff (State (Ctxt α) ': r) a
interpretLocalContextOps = replaceRelay return $ \case
  -- (>>=) $ foo   stands for   \ arr -> foo >>= arr
  AddAssumption   b τ -> (>>=) $ modify (LC.addLocalAssum (b, τ))
  Fresh           s   -> (>>=) $ interpretFresh s
  GetLocalContext     -> (>>=) $ get
  LookupType      v   -> (>>=) $ interpretLookup v
  SetLocalContext γ   -> (>>=) $ put γ
  where
    interpretFresh :: [Variable] -> Eff (State (Ctxt α) ': r) Variable
    interpretFresh suggestions = do
      γ :: Ctxt α <- get
      let candidates = suggestions ++ (Variable <$> identifiers)
      return $ head $ candidates \\ LC.boundNames γ
    interpretLookup :: Variable -> Eff (State (Ctxt α) ': r) (Term α)
    interpretLookup v = do
      γ :: Ctxt α <- get
      case LC.lookupType v γ of
        Nothing -> (throwError :: Error -> Eff (State (Ctxt α) ': r) x) $ Var Nothing v
        Just τ  -> return τ
