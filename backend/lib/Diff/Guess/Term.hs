{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}

module Diff.Guess.Term
  ( Match(..)
  , algorithm
  , guess
  , guessδ
  , makeNode
  , matchPairs
  , recommended
  , traceGuessδ
  , withNodeMapping
  ) where

import           Control.Lens hiding (children, preview)
import           Control.Monad
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Trace
import           Data.Function
import qualified Data.List as List
import           Data.List.HT
import           Data.Maybe
import           Prelude hiding (product)
import           Text.Printf

import qualified Diff.Atom as ΔA
import           Diff.Guess.BottomUp
import           Diff.Guess.Mapping
import           Diff.Guess.TopDown
import           Diff.Guess.Node
import qualified Diff.Term as ΔT
import           Diff.Utils
import           PrettyPrinting.Term ()
import           PrettyPrinting.PrettyPrintable
import           Term.Term
import qualified Term.Raw as Raw
import           Utils

branchChild :: Branch α Variable -> TermX α Variable
branchChild = view _3 . unpackBranch

-- Turns out we need to help the GumTree algorithm when we don't have any unique
-- matches

-- termChildren :: TermX α Variable -> [TermX α Variable]
-- termChildren = go
--   where
--     go input = case input of
--       Annot _ t τ -> [t, τ]
--       App _ t1 t2 -> [t1, t2]
--       Hole _ -> []
--       Lam _ bt -> let (_, t) = unscopeTerm bt in [t]
--       Let _ t1 bt2 -> [t1, t2] where (_, t2) = unscopeTerm bt2
--       Match _ t bs -> t : map branchChild bs
--       Pi _ τ1 bτ2 -> let (_, τ2) = unscopeTerm bτ2 in [τ1, τ2]
--       Type _ -> []
--       Var _ _ -> []

termChildren :: TermX α Variable -> [TermX α Variable]
termChildren = go
  where
    go input = case input of
      Annot _ t τ -> [t, τ]
      App _ _ _ ->
        case run $ runError (ΔT.extractApps input) of
        Left (_ :: String) -> error "This should not happen"
        Right (c, cs) -> c : map (view _2) cs
      Hole _ -> []
      Lam _ _ ->
        case run $ runError (ΔT.extractLams input) of
        Left (_ :: String) -> error "This should not happen"
        Right (cs, c) ->
          map (variableFromBinder . view _2) cs ++ [c]
      Let _ t1 bt2 -> [t1, t2] where (_, t2) = unscopeTerm bt2
      Match _ t bs -> t : map branchChild bs
      Pi _ _ _ ->
        case run $ runError (ΔT.extractPis input) of
        Left (_ :: String) -> error "This should not happen"
        Right (cs, c) -> map (view _3) cs ++ [c]
      Type _ -> []
      Var _ _ -> []
    variableFromBinder (Binder b) = case b of
      Nothing -> Var Nothing (mkVariable "__FIXME_Diff.Guess.Term__")
      Just v  -> Var Nothing v

termHeight :: TermX α Variable -> Int
termHeight t = case termChildren t of
  [] -> 1 -- huh... why would they choose this...?
  c  -> 1 + maximum (map termHeight c)

fresh :: (Member (State Int) r) => Eff r Int
fresh = get <* modify ((+) (1 :: Int))

makeNode :: (Member (State Int) r) => Raw.Term Variable -> Eff r Node
makeNode t = ($ Nothing) <$> go t
  where
    go t = do
      i <- fresh
      childrenGen <- mapM go (termChildren t)
      return $ \ p -> fix $ \ n -> Node
        { children   = map ($ (Just n)) childrenGen
        , height     = termHeight t
        , identifier = i
        , node       = t
        , parent     = p
        }

algorithm ::
  ( Member Trace r
  ) =>
  Int -> Double -> Int -> Node -> Node -> Eff r Mapping
algorithm minHeight minDice _maxSize n1 n2 = do
  s1 <- runTopDown  n1 n2 minHeight
  let m1 = view topDownStateM s1
  s2 <- runBottomUp n1 n2 m1 minDice {- maxSize -}
  let m2 = view bottomUpStateM s2
  return m2

recommended ::
  ( Member Trace r
  ) =>
  Node -> Node -> Eff r Mapping
recommended = algorithm 0 0.0 100

guess ::
  ( Member Trace r
  ) =>
  Raw.Term Variable -> Raw.Term Variable -> Eff r (ΔT.Diff Raw.Raw)
guess = withNodeMapping mkGuessδ

{- For many functions we will want to take as input two terms, and compute
something based on the nodes of those terms and the mapping between those.
-}
withNodeMapping ::
  (Node -> Node -> Mapping -> a) ->
  Raw.Term Variable -> Raw.Term Variable -> a
withNodeMapping k t1 t2 =
  let (n1, n2) = fst . run $ runState s (0 :: Int) in
  let m = skipTrace $ recommended n1 n2 in
  k n1 n2 m
  where
    s = do
      n1 <- makeNode t1
      n2 <- makeNode t2
      return (n1, n2)

guessδ :: Raw.Term Variable -> Raw.Term Variable -> ΔT.Diff Raw.Raw
guessδ t1 t2 = skipTrace (guess t1 t2)

traceGuessδ ::
  Raw.Term Variable -> Raw.Term Variable -> IO (ΔT.Diff Raw.Raw)
traceGuessδ = withNodeMapping $ \ n1 n2 m -> do
  putStrLn $ printf "MAPPING:\n%s\n" (show m)
  δ <- runTrace $ mkGuessδ n1 n2 m
  return δ

unsafeSplitWhen :: (t -> Bool) -> [t] -> ([t], t, [t])
unsafeSplitWhen _ [] = error "unsafeSplitWhen"
unsafeSplitWhen p (h : t) | p h       = ([], h, t)
                          | otherwise = over _1 ((:) h) $ unsafeSplitWhen p t

{- expandSpanning equals l1 l2 splits two lists, respectively, into (pre1,
post1) and (pre2, post2) s.t. no element of pre1 has an equal element in post2,
and no element of pre2 has an equal element in post1.  It does so while minimizing
the length of pre1 and pre2.

expandSpanning (=) [a, b, c, d, e, f] [c, a, e, g] =
(([a, b, c, d, e], [f]), ([c, a, e], [g]))
-}

expandSpanning :: (a -> b -> Bool) -> [a] -> [b] -> (([a], [a]), ([b], [b]))
expandSpanning _ [] [] = (([], []), ([] ,[]))
expandSpanning _ [] l2 = (([], []), ([], l2))
expandSpanning _ l1 [] = (([], l1), ([], []))
expandSpanning equals (h1 : t1) (h2 : t2) = go ([h1], t1) ([h2], t2)
  where
    go s1@(pre1, post1) s2@(pre2, post2) =
      case List.find (isIn2 post2) pre1 of
      Nothing ->
        case List.find (isIn1 post1) pre2 of
        Nothing -> (s1, s2)
        Just e2 ->
          let (l1, e1, r1) = unsafeSplitWhen (`equals` e2) post1 in
          go (pre1 ++ l1 ++ [e1], r1) (pre2, post2)
      Just e1 ->
        let (l2, e2, r2) = unsafeSplitWhen (equals e1) post2 in
        go (pre1, post1) (pre2 ++ l2 ++ [e2], r2)
    isIn1 l e = List.any (`equals` e) l
    isIn2 l e = List.any (equals e) l

{- generatePermutation takes two lists with matching elements, and generate a permutation
such that:

- matching elements are permuted to appear in the order of the second list
- non matching elements are left at the position they were in the first list

i.e.

l1 = A X Y D Z F G H
l2 = D G U A V H F

permutation should yield:

r =  D X Y G Z A H F

-}
generatePermutation :: (a -> t -> Bool) -> [a] -> [t] -> [Int]
generatePermutation equals l1 l2 =
  let replacements =
        catMaybes
        $ map (\ e2 -> List.findIndex (\ e1 -> equals e1 e2) l1) l2
  in go (zip l1 [0..]) replacements
  where
    go [] _ = []
    go ((e1, _) : l1) (r : rs) | List.any (\ e2 -> equals e1 e2) l2 = r : go l1 rs
    go ((_, p1) : l1) rs = p1 : go l1 rs

data Match
  = Matched        Node Node
  | LeftUnmatched  Node
  | RightUnmatched Node
  | Permuted       [Int]
  deriving (Show)

{- `matchPairs m l1 l2` takes a mapping between nodes, and two lists of nodes,
and computes a match between the two lists, which is a list of indications on
how to relate elements from `l1` and `l2`, in order.
For instance, given input lists:
l1 = [a, b, c, d, e]
l2 = [x, d, b, y]
It should output:
                      [a, b, c, d, e]   [x, d, b, y]
LeftUnmatched a          [b, c, d, e]   [x, d, b, y]
RightUnmatched x         [b, c, d, e]      [d, b, y]
Permuted [1, 2, 0]       [c, d, b, e]      [d, b, y]
LeftUnmatched c             [d, b, e]      [d, b, y]
Matched d                      [b, e]         [b, y]
Matched b                         [e]            [y]
LeftUnmatched e                    []            [y]
RightUnmatched y                   []             []

-}
matchPairs :: Mapping -> [Node] -> [Node] -> [Match]
matchPairs m l1 l2 = go l1 l2
  where
    equals e1 e2 = (e1, e2) `elem` m
    go [] [] = []
    go (n1 : t1) [] = LeftUnmatched  n1 : go [] t1
    go [] (n2 : t2) = RightUnmatched n2 : go [] t2
    go l1@(n1 : t1) l2@(n2 : t2)
      | (n1, n2) `elem` m         = Matched n1 n2 : go t1 t2
      | List.any (matchesL n1) t2 && List.any (matchesR n2) t1
      = let ((pre1, post1), (pre2, post2)) = expandSpanning equals l1 l2 in
        let p = generatePermutation equals pre1 pre2 in
        Permuted p : go (permute p pre1 ++ post1) (pre2 ++ post2)
      | List.any (matchesL n1) t2 = RightUnmatched n2 : go l1 t2
      | List.any (matchesR n2) t1 = LeftUnmatched n1 : go t1 l2
      | otherwise                 = LeftUnmatched n1 : RightUnmatched n2 : go t1 t2
    matchesL nL nR = (nL, nR) `elem` m
    matchesR nR nL = (nL, nR) `elem` m

data FoldAppsStatus
  = NoPermutation
  | DelayedPermutation [Int] Int -- need to insert a permutation later
  deriving (Show)

mkGuessδ ::
  ( Member Trace r
  ) =>
  Node -> Node -> Mapping -> Eff r (ΔT.Diff Raw.Raw)
mkGuessδ n1 n2 m = go n1 n2

  where

    go n1 n2 = do

      trace $ printf "Guessing δ for (%s, %s)" (preview (node n1)) (preview (node n2))

      if (n1, n2) `elem` m
        then do
        if isomorphic n1 n2
          then do
          trace "Matched and isomorphic!"
          return $ ΔT.Same
          else do
          trace "Matched, but not isomorphic!"
          case (node n1, node n2) of

            (App _ _ _, App _ _ _) -> do
              let pairs = matchPairs m (children n1) (children n2)
              trace $ printf "About to foldApps (%s, %s)" (show $ children n1) (show $ children n2)
              let pairsRest = case pairs of
                    Matched _ _ : pairsRest -> pairsRest
                    _ -> error $ printf "Couldn't extract pairsRest from: %s" (show pairs)
              (δ, _) <- foldM foldApps (id, NoPermutation) pairsRest
              return $ δ ΔT.Same

            (Lam _ _, Lam _ _) -> do
              let pairs = matchPairs m (children n1) (children n2)
              (δ, _) <- foldM foldLams (id, (node n1, node n2)) pairs
              return $ δ ΔT.Same --(ΔT.Replace (Var Nothing (Variable "HERE")))

            -- when we have two non-isomorphic Pis to match together,
            -- we attempt to find a matching among the Pi-telescopes
            (Pi _ _ _, Pi _ _ _) -> do
              -- trace $ show $ node n1
              -- trace $ show $ node n2
              let pairs = matchPairs m (children n1) (children n2)
              -- trace $ printf "PAIRS:\n%s" (show pairs)
              (δ, _) <- foldM foldPis (id, (node n1, node n2)) pairs
              return $ δ ΔT.Same --(ΔT.Replace (Var Nothing (Variable "HERE")))

            (_, _) -> error $ printf "TODO not isomorphic: (%s, %s)" (preview (node n1)) (preview (node n2))
        else do
        trace "Unmatched"
        case (node n1, children n1, node n2, children n2) of

          (Pi _ _ _bτ2, _τs, Pi _α' _ _bτ2', _τs') -> do
            trace $ show $ matchPairs m (children n1) (children n2)
            error "FINISH ME"
            -- if containMatches m τ τ'
            --   then do
            --   δ1 <- go τ τ'
            --   let (_, τ2)  = unscopeTerm bτ2
            --   let (_, τ2') = unscopeTerm bτ2'
            --   δ2 <- go _
            --   return $ ΔT.CpyPi <$> δ1 <*> Just ΔA.Same <*> δ2
            --   else do
            --   δ1 <- go n1 τ1
            --   δ2 <- go n1 τ2
            --   return $ ΔT.InsPi <$> Just α' <*> δ1 <*> Just (Binder Nothing) <*> δ2

          (_, _, Pi _ _ _, τs') -> do

            δs' <- forM τs' (go n1)
            case viewR δs' of
              Nothing -> error "This should not happen"
              Just (δs', δ') -> do
                -- Problem: τs' does not contain the name of the binders for each child
                -- Solution: extract them from (node n2)
                (πs, _) <- extractPis (node n2)
                if length πs /= length δs'
                  then do
                  trace $ printf "%s" (show $ map prettyStr πs)
                  trace $ printf "%s" (show δs')
                  error "This is possibly odd, check if it happens"
                  else return ()
                return $ foldr (\ (δ, b) δs -> ΔT.InsPi () δ b δs) δ' (zip δs' (map (view _2) πs))

          (Pi _ _ _, _ : τ2 : _, Var _ _, _) -> do
            δ <- go τ2 n2
            return $ ΔT.RemovePi δ

          (App _ _ _, _, Var _ _, _) -> return $ ΔT.Replace (node n2)
          (Var _ _, _, App _ _ _, f' : args') -> do
            -- if the Var on the left has a match on the right, InsApp, else Replace
            if node n1 == node f'
              then do
              let δf = ΔT.Same
              δargs <- forM args' (go n1)
              return $ foldr (\ δ δs -> ΔT.InsApp () δs δ) δf δargs
              else return $ ΔT.Replace (node n2)

          -- no choice here
          (Type _,  _, Var _ _, _) -> return $ ΔT.Replace (node n2)
          (Var _ _, _, Type _,  _) -> return $ ΔT.Replace (node n2)
          (Var _ _, _, Var _ _, _) -> return $ ΔT.Replace (node n2)

          _ -> do
            error $ printf "TODO: (%s, %s) (%s, %s)" (preview $ node n1) (preview $ node n2) (show $ node n1) (show $ node n2)

-- δs' <- forM τs' (go n1)
-- case viewR δs' of
--   Nothing -> error "This should not happen"
--   Just (δs', δ') -> do
--     trace $ printf "### InsPi ###"
--     return $ foldr (\ δ δs -> ΔT.InsPi () δ b δs) δ' δs'

{-
Oh boy, this is annoyingly convoluted.
This one is not super intuitive.  A nested application sequence:

(((((f b) y) c ) d) e)   --->   (((((f a) d) b) x) c)

produces a matching:

Matched f f, RightUnmatched a, Permute [3, 1, 0, 2], LeftUnmatched y,
Matched d d, Matched b b, RightUnmatched x, Matched c c, LeftUnmatched e

which should yield diff:

RemoveApp (e), PermuteApps [3, 1, 0, 2], CpyApp (c), InsApp x, CpyApp (b),
CpyApp (d), RemoveApp (y), InsApp a, Same (f)

Process:
Matched f f             ->   Same          (probably done at setup time)
RightUnmatched a        ->   InsApp a
Permuted [3, 1, 0, 2]   ->                 <delayed [3, 1, 0, 2] length: 4>
Matched d d             ->   CpyApp (d)    <delayed [3, 1, 0, 2] length: 2>
LeftUnmatched y         ->   RemoveApp (y) <delayed [3, 1, 0, 2] length: 3>
Matched b b             ->   CpyApp (b)    <delayed [3, 1, 0, 2] length: 1>
RightUnmatched x        ->   InsApp x      <delayed [3, 1, 0, 2] length: 1>
Matched c c             ->   CpyApp (c)    <delayed [3, 1, 0, 2] length: 0>
                             PermuteApps [3, 1, 0, 2]
LeftUnmatched e         ->   RemoveApp (e)

-}

-- TODO: this handling of (t, t') does not make sense in this context anymore
-- figure out whether we need them at all (for InsApp?)
    foldApps (δ, s) m = do

      trace $ printf "foldApps δ %s %s" (show m) (show s)

      (δ, s) <- case s of
        -- inject delayed permutation if needed
        NoPermutation ->
          case m of
          Permuted p -> return (δ, DelayedPermutation p (length p))
          _ -> return (δ, NoPermutation)
        DelayedPermutation p 0 -> return (ΔT.PermutApps p . δ, NoPermutation)
        DelayedPermutation p d -> do
          let d' = case m of
                RightUnmatched _ -> d     -- does not change count in original
                LeftUnmatched _  -> d - 1
                Matched _ _      -> d - 1
                Permuted _ -> error "Permuception!"
          return (δ, DelayedPermutation p d')

      δ <- case m of
        RightUnmatched t -> do
          return $ flip (ΔT.InsApp ()) (ΔT.Replace (node t)) . δ
        LeftUnmatched _ -> do
          return $ ΔT.RemoveApp . δ
        Matched t t' -> do
          δ2 <- go t t'
          return $ flip ΔT.CpyApp δ2 . δ
        Permuted _ -> return δ -- already injected a delayed permutation

      return (δ, s)

    foldLams (δ, (t, t')) = \case
      RightUnmatched _ -> do
        let (Lam α' bt') = t'
        let (_, t') = unscopeTerm bt'
        let b = view originalBinder bt'
        return $ (δ . ΔT.InsLam α' b, (t, t'))
      LeftUnmatched _ -> do
        let (Lam _ bt) = t
        let (_, t) = unscopeTerm bt
        return $ (δ . ΔT.RemoveLam, (t, t'))
      Matched t1 t1' -> do
        case (t, t') of
          (Lam _ bt, Lam _ bt') -> do
            let (_, t2) = unscopeTerm bt
            let (_, t2') = unscopeTerm bt'
            δ1 <- go t1 t1'
            return $ (δ . ΔT.CpyPi δ1 ΔA.Same, (t2, t2'))
          (_, _) -> do
            -- nothing to do here?
            return $ (δ, (t, t'))
      Permuted p -> do
        case ΔT.patchMaybe t (ΔT.PermutLams p ΔT.Same) of
          Nothing -> error "computed permutation was incorrect"
          Just tPatched -> do
            return $ (δ . ΔT.PermutLams p, (tPatched, t'))

    foldPis (δ, (t, t')) = \case
      RightUnmatched _ -> do
        let (Pi α' τ1' bτ2') = t'
        let b = view originalBinder bτ2'
        let (_, τ2') = unscopeTerm bτ2'
        trace $ printf "IMMA INSERT SOME %s" (show b)
        return $ (δ . ΔT.InsPi α' (ΔT.Replace τ1') b, (t, τ2'))
      LeftUnmatched _ -> do
        let (Pi _ _ bτ2) = t
        let (_, τ2) = unscopeTerm bτ2
        return $ (δ . ΔT.RemovePi, (τ2, t'))
      Matched τ1 τ1' -> do
        case (t, t') of
          (Pi _ _ bτ2, Pi _ _ bτ2') -> do
            δ1 <- go τ1 τ1'
            let (_, τ2) = unscopeTerm bτ2
            let (_, τ2') = unscopeTerm bτ2'
            return $ (δ . ΔT.CpyPi δ1 ΔA.Same, (τ2, τ2'))
          (_, _) -> do
            -- nothing to do here?
            return $ (δ, (t, t'))
      Permuted p -> do
        case ΔT.patchMaybe t (ΔT.PermutPis p ΔT.Same) of
          Nothing -> error "computed permutation was incorrect"
          Just tPatched -> do
            return $ (δ . ΔT.PermutPis p, (tPatched, t'))
