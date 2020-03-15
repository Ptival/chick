{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Typing.LocalContextOps (
  LocalContextOps(..),
  addAssumption,
  freshBinder,
  getLocalContext,
  interpretLocalContextOps,
  lookupType,
  withAssumption,
  ) where

import           Polysemy
import           Polysemy.Error
import           Polysemy.Internal
import           Polysemy.State
import           Polysemy.Trace
import           Data.List
import           Data.Maybe ( mapMaybe, maybeToList )

import           Term.Binder
import           Term.Term
import qualified Term.TypeErrored as E
import qualified Typing.LocalContext as LC

type Term α    = TermX α Variable
--type Checked = C.Term Variable
type Ctxt α    = LC.LocalContext α Variable
type ErrorTerm = E.Term Variable

data LocalContextOps (α :: Type) m a where
  AddAssumption   :: Binder Variable -> Term α -> LocalContextOps α m ()
  Fresh           :: [Variable] ->                LocalContextOps α m Variable
  GetLocalContext ::                              LocalContextOps α m (Ctxt α)
  LookupType      :: Variable ->                  LocalContextOps α m (Term α)
  SetLocalContext :: Ctxt α ->                    LocalContextOps α m ()

makeSem ''LocalContextOps

freshBinder :: ∀ α r. Member (LocalContextOps α) r => [Binder Variable] -> Sem r (Binder Variable)
freshBinder l = Binder . Just <$> fresh @α (mapMaybe unBinder l)

withAssumption :: ∀ α (r :: EffectRow) a.
  Member Trace r =>
  Member (LocalContextOps α a) r =>
  Binder Variable -> Term α -> Sem r a -> Sem r a
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
  Member (Error ErrorTerm) r =>
  Sem (LocalContextOps α ': r) a -> Sem (State (Ctxt α) ': r) a
interpretLocalContextOps = replaceRelay return $ \case
  -- (>>=) $ foo   stands for   \ arr -> foo >>= arr
  AddAssumption   b τ -> (>>=) $ modify (LC.addLocalAssum (b, τ))
  Fresh           s   -> (>>=) $ interpretFresh s
  GetLocalContext     -> (>>=) get
  LookupType      v   -> (>>=) $ interpretLookup v
  SetLocalContext γ   -> (>>=) $ put γ
  where
    interpretFresh :: [Variable] -> Sem (State (Ctxt α) ': r) Variable
    interpretFresh suggestions = do
      γ :: Ctxt α <- get
      let candidates = suggestions ++ (Variable <$> identifiers)
      return $ head $ candidates \\ LC.boundNames γ
    interpretLookup :: Variable -> Sem (State (Ctxt α) ': r) (Term α)
    interpretLookup v = do
      γ :: Ctxt α <- get
      case LC.lookupType v γ of
        Nothing -> (throwError :: Error -> Sem (State (Ctxt α) ': r) x) $ Var Nothing v
        Just τ  -> return τ
