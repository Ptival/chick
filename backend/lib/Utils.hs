module Utils (
  extractApps,
  extractLams,
  extractPi,
  extractPis,
  extractSomeApps,
  extractSomeLams,
  extractSomePis,
  foldlWith,
  foldrWith,
  isPi,
  mapWithIndex,
  mkApps,
  mkLams,
  mkPis,
  orElse,
  orElse',
  splitList,
  unzipMaybe,
  withState,
  ) where

import           Control.Lens
import qualified Control.Monad.Error.Class as ME
import           Polysemy
import           Polysemy.Error
import           Polysemy.State
import           Text.Printf

import           Term.Term

foldlWith :: Foldable t => (b -> a -> b) -> t a -> b -> b
foldlWith f l a = foldl f a l

foldrWith :: Foldable t => (a -> b -> b) -> t a -> b -> b
foldrWith f l a = foldr f a l

isPi :: TermX ξ ν -> Maybe (ξ, TypeX ξ ν, ScopedTerm (TypeX ξ) ν)
isPi (Pi a b c) = Just (a, b, c)
isPi _          = Nothing

mapWithIndex :: (a -> Int -> b) -> [a] -> [b]
mapWithIndex f l = zipWith f l [0..]

orElse ::
  ME.MonadError e m =>
  Maybe a -> e -> m a
orElse Nothing  e = ME.throwError e
orElse (Just a) _ = return a

orElse' ::
  ME.MonadError e m =>
  Bool -> e -> m ()
orElse' False e = ME.throwError e
orElse' True  _ = return ()

-- dumb, but has the same signature as `runTrace`, so easier to interchange
-- runSkipTrace :: Sem '[Trace] a -> IO a
-- runSkipTrace = return <$> skipTrace
--
-- skipTrace :: Sem '[Trace] a -> a
-- skipTrace (Val x) = x
-- skipTrace (E u q) =
--   case extract u of
--     Trace _ -> skipTrace (qApp q ())

splitList
   :: Int -> [a] -> Maybe ([a], a, [a])
splitList n xs =
  revL <$> go n xs
  where
    go 0 (h:t) = Just ([], h, t)
    go m (h:t) = prependL h <$> go (m-1) t
    go _ []    = Nothing
    prependL h (revl, x, r) = (h:revl, x, r)
    revL (l, x, r) = (reverse l, x, r)

unzipMaybe :: Maybe (a, b) -> (Maybe a, Maybe b)
unzipMaybe Nothing       = (Nothing, Nothing)
unzipMaybe (Just (a, b)) = (Just a,  Just b)

-- | `withState` localizes a modification of the state to a given effectful computation
withState ::
  Member (State s) r =>
  (s -> s) -> Sem r a -> Sem r a
withState f e = do
  s <- get
  put (f s)
  r <- e
  put s
  return r

-- | `f a b c` becomes `(f, [a, b, c])`
extractApps ::
  TermX α Variable -> Sem r (TermX α Variable, [(α, TermX α Variable)])
extractApps t = over _2 reverse <$> go t
  where
    go (App a t1 t2) = do
      (f, args) <- go t1
      return (f, (a, t2) : args)
    go t1              = return (t1, [])

extractSomeApps ::
  ( Member (Error String) r
  , Show α
  ) =>
  Int -> TermX α Variable -> Sem r (TermX α Variable, [(α, TermX α Variable)])
extractSomeApps 0 t = return (t, [])
extractSomeApps n (App a t1 t2) = do
  (f, args) <- extractSomeApps (n - 1) t1
  return (f, args ++ [(a, t2)]) -- TODO: make this not quadratic, I'm being lazy
extractSomeApps _ t =
  let e :: String = printf "extractLams: not a Lam: %s" (show t) in
  throw e

extractSomeLams ::
  ( Member (Error String) r
  , Show α
  )=>
  Int -> TermX α Variable -> Sem r ([(α, Binder Variable)], TermX α Variable)
extractSomeLams 0 t = return ([], t)
extractSomeLams n (Lam a bt) = do
  let (b, t) = unscopeTerm bt
  (l, rest) <- extractSomeLams (n - 1) t
  return ((a, b) : l, rest)
extractSomeLams _ t =
  let e :: String = printf "extractLams: not a Lam: %s" (show t) in
  throw e

extractLams ::
  TermX α Variable -> Sem r ([(α, Binder Variable)], TermX α Variable)
extractLams = \case
  Lam a bt -> do
    let (b, t) = unscopeTerm bt
    (l, rest) <- extractLams t
    return ((a, b) : l, rest)
  t -> return ([], t)

extractSomePis ::
  Member (Error String) r =>
  Show α =>
  Int -> TermX α Variable -> Sem r ([(α, Binder Variable, TermX α Variable)], TermX α Variable)
extractSomePis 0 t = return ([], t)
extractSomePis n (Pi a τ1 bτ2) = do
  let (b, τ2) = unscopeTerm bτ2
  (l, rest) <- extractSomePis (n - 1) τ2
  return ((a, b, τ1) : l, rest)
extractSomePis _ t =
  let e :: String = printf "extractPis: not a Pi: %s" (show t) in
  throw e

extractPis ::
  TermX α Variable ->
  Sem r ([(α, Binder Variable, TermX α Variable)], TermX α Variable)
extractPis = \case
  Pi a τ1 bτ2 -> do
    let (b, τ2) = unscopeTerm bτ2
    (l, rest) <- extractPis τ2
    return ((a, b, τ1) : l, rest)
  t -> return ([], t)

extractPi ::
  Member (Error String) r =>
  Show α =>
  TermX α Variable ->
  Sem r (α, TermX α Variable, Binder Variable, TermX α Variable)
extractPi = \case
  Pi a τ1 bτ2 -> do
    let (b, τ2) = unscopeTerm bτ2
    return (a, τ1, b, τ2)
  t ->
    let e :: String = printf "extractPi: not a Pi: %s" (show t) in
    throw e

mkApps :: TermX α Variable -> [(α, TermX α Variable)] -> TermX α Variable
mkApps f []           = f
mkApps f ((a, e) : t) = mkApps (App a f e) t

mkLams :: [(α, Binder Variable)] -> TermX α Variable -> TermX α Variable
mkLams []          rest = rest
mkLams ((a, b) : t) rest = Lam a (abstractBinder b (mkLams t rest))

mkPis :: [(α, Binder Variable, TermX α Variable)] -> TermX α Variable -> TermX α Variable
mkPis []               rest = rest
mkPis ((a, b, τ1) : t) rest = Pi a τ1 (abstractBinder b (mkPis t rest))
