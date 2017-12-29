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
  ( algorithm
  , guess
  , guessδ
  , makeNode
  , recommended
  , traceGuessδ
  ) where

import           Control.Lens hiding (children, preview)
import           Control.Monad
import           Control.Monad.Extra (whenM)
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.Reader
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Trace
import           Control.Monad.Loops
import           Data.Function
import qualified Data.List as List
import           Data.Maybe
import           Data.Ord
import           Prelude hiding (product)
import           Text.Printf
import           Util ((<&&>), count)

import qualified Diff.Atom as ΔA
import qualified Diff.Term as ΔT
import           Diff.Utils
import           PrettyPrinting.Term ()
import           PrettyPrinting.PrettyPrintable
import           Term.Term
import qualified Term.Raw as Raw
import           Utils

branchChild :: Branch α Variable -> TermX α Variable
branchChild = view _3 . unpackBranch

label :: Node -> String
label n = case node n of
  Annot _ _ _ -> "Annot"
  App   _ _ _ -> "App"
  Hole  _     -> "Hole"
  Lam   _ _   -> "Lam"
  Let   _ _ _ -> "Let"
  Match _ _ _ -> "Match"
  Pi    _ _ _ -> "Pi"
  Type  _     -> "Type"
  Var   _ v   -> printf "Var(%s)" (show v)

-- Turns out we need to help the GumTree algorithm when we don't have any
-- unique matches

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
      Nothing -> Var Nothing (Variable "_")
      Just v  -> Var Nothing v

nodeDescendants :: Node -> [Node]
nodeDescendants t = c ++ concatMap nodeDescendants c
  where c = children t

nodeAndDescendants :: Node -> [Node]
nodeAndDescendants n = n : nodeDescendants n

nodeAndDescendantsPostOrder :: Node -> [Node]
nodeAndDescendantsPostOrder n =
  concatMap nodeAndDescendantsPostOrder (children n) ++ [n]

termHeight :: TermX α Variable -> Int
termHeight t = case termChildren t of
  [] -> 1 -- huh... why would they choose this...?
  c  -> 1 + maximum (map termHeight c)

-- HI stands for height-indexed (invariant: decreasing heights)
newtype HIList = HIList { unHIList :: [Node] }

instance Show HIList where
  show = show . unHIList

peekMax :: HIList -> Int
peekMax (HIList [])      = 0
peekMax (HIList (h : _)) = height h

insert :: Node -> HIList -> HIList
insert n = HIList . List.insertBy (comparing $ Down . height) n . unHIList

product :: [Node] -> [Node] -> [(Node, Node)]
product l1 l2 = [ (a, b) | a <- l1, b <- l2 ]

{- dice computes a ratio of subnodes that are mapped together -}
dice :: [(Node, Node)] -> Node -> Node -> Double
dice m t1 t2 = (2 * fromIntegral c) / (lengthOf s1 + lengthOf s2)
  where
    lengthOf = fromIntegral . List.length
    c = count (`elem` m) (product s1 s2)
    s1 = nodeDescendants t1
    s2 = nodeDescendants t2

isomorphic :: Node -> Node -> Bool
isomorphic n1 n2 =
  height n1 == height n2 -- maybe this helps performance?
  && node n1 == node n2

-- NOTE: Taking care of annotations ends up being more work than I could care for
-- because we must keep track of parent annotations for all children so that we
-- know how to annotated when reconstructing.  It's simpler to just work on raw
-- terms for now.
data Node = Node
  { children   :: [Node]
  , height     :: Int
  , identifier :: Int
  , node       :: Raw.Term Variable
  , parent     :: Maybe Node
  }

type Mapping = [(Node, Node)]

instance Show Node where
  show n = printf "(%s : %s)" (show . identifier $ n) (preview . node $ n)

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

instance Eq Node where
  (==) n1 n2 = identifier n1 == identifier n2

data TopDownReader = TopDownReader
  { _readerMinHeight :: Int
  , _readerRoot1 :: Node
  , _readerRoot2 :: Node
  }
  deriving (Show)
makeLenses ''TopDownReader

data TopDownState = TopDownState
  { _stateL1 :: HIList
  , _stateL2 :: HIList
  , _stateA  :: Mapping
  , _stateM  :: Mapping
  }
  deriving (Show)
makeLenses ''TopDownState

open ::
  ( Member (State TopDownState) r
  ) => Node -> Lens' TopDownState HIList -> Eff r ()
open t lens = forM_ (reverse $ children t) $ \ c -> do
  modify $ over lens $ insert c

pop ::
  ( Member (State TopDownState) r
  ) => Lens' TopDownState HIList -> Eff r [Node]
pop lens = do
  l <- view lens <$> get
  let (popped, l') = List.partition ((==) (peekMax l) . height) (unHIList l)
  modify $ set lens (HIList l')
  return popped

popHead ::
  ( Member (State TopDownState) r
  ) => Lens' TopDownState [a] -> Eff r a
popHead lens = do
  l <- view lens <$> get
  case l of
    [] -> error "popHead on empty list"
    h : t -> do
      modify $ set lens t
      return h

push ::
  ( Member (State TopDownState) r
  ) => Node -> Lens' TopDownState HIList -> Eff r ()
push node lens = do
  modify $ over lens $ insert node

runTopDown :: ∀ r.
  ( Member Trace r
  ) =>
  Node -> Node -> Int -> Eff r TopDownState
runTopDown n1 n2 minHeight =
  snd <$> runState (runReader topDown r) s0
  where
    r = TopDownReader
      { _readerMinHeight = minHeight
      , _readerRoot1     = n1
      , _readerRoot2     = n2
      }
    s0 = TopDownState
      { _stateL1 = HIList []
      , _stateL2 = HIList []
      , _stateA  = []
      , _stateM  = []
      }

topDown :: ∀ r.
  ( Member (Reader TopDownReader) r
  , Member (State  TopDownState) r
  , Member Trace r
  ) =>
  Eff r ()
topDown = do

  root1 <- view readerRoot1 <$> ask
  root2 <- view readerRoot2 <$> ask
  push root1 stateL1
  push root2 stateL2

  whileM_ condition1 $ do

    trace "topDown loop"

    l1 <- view stateL1 <$> get
    l2 <- view stateL2 <$> get

    if peekMax l1 /= peekMax l2
      then do
      if peekMax l1 > peekMax l2
        then do
        -- trace "pop l1"
        ts <- pop stateL1
        forM_ ts $ \t -> open t stateL1
        else do
        -- trace "pop l2"
        ts <- pop stateL2
        forM_ ts $ \t -> open t stateL2
      else do
      -- trace "pop both"
      h1 <- pop stateL1
      h2 <- pop stateL2
      -- trace $ printf "h1: %s" (show h1)
      -- trace $ printf "h2: %s" (show h2)
      forM_ (product h1 h2) $ \ (t1, t2) -> do
        let set1 = nodeAndDescendants root1
        let set2 = nodeAndDescendants root2
        when (isomorphic t1 t2) $ do
          -- trace "ISOMORPHIC!"
          if (  List.any (\ tx -> isomorphic t1 tx && tx /= t2) set2
             || List.any (\ tx -> isomorphic tx t2 && tx /= t1) set1
             )
            then do
            -- trace $ printf "Adding candidate mapping (%s, %s)" (show t1) (show t2)
            modify $ over stateA $ (:) (t1, t2)
            else do
            addIsomorphicDescendants t1 t2
      a <- view stateA <$> get
      m <- view stateM <$> get
      let aum = List.union a m
      forM_ [ t1 | t1 <- h1
                 , not (List.any ((==) t1 . fst) aum) ] $ \ t1 -> do
        open t1 stateL1
      forM_ [ t2 | t2 <- h2
                 , not (List.any ((==) t2 . snd) aum) ] $ \ t2 -> do
        open t2 stateL2

  -- trace "Now sorting candidate mappings by dice"
  m <- view stateM <$> get
  modify $ over stateA $ List.sortBy (comparing (parentDice m))

  -- a <- view (stateA @α) <$> get
  -- forM_ a $ \ (t1, t2) -> do
  --   trace $ printf "A: (%s, %s)" (show $ t1) (show $ t2)
  --   trace $ printf "Parents : %s, %s"
  --     (show $ preview . node <$> parent t1)
  --     (show $ preview . node <$> parent t2)
  --   trace $ printf "Parent dice: %s" (show . parentDice m $ (t1, t2))
  -- trace $ printf "About to go through:\n%s" (show a)
  whileM_ condition2 $ do
    (t1, t2) <- popHead stateA
    -- trace $ printf "Popped %s" (show (t1, t2))
    addIsomorphicDescendants t1 t2
    modify $ over stateA $ filter ((/=) t1 . fst) . filter ((/=) t2 . snd)

  -- trace "END OF TOP-DOWN PHASE:"
  -- m <- view (stateM @α) <$> get
  -- trace $ show m

  where

    addIsomorphicDescendants t1 t2 = do
      let st1 = nodeAndDescendants t1
      let st2 = nodeAndDescendants t2
      -- trace $ printf "Node and descendants for %s" (show (t1, t2))
      -- trace $ printf "t1: %s" (preview . node $ t1)
      -- trace $ printf "t2: %s" (preview . node $ t2)
      -- trace $ printf "are %s" (show (st1, st2))
      forM_ (product st1 st2) $ \ (n1, n2) -> do
        -- trace $ printf "Pondering: %s" (show (n1, n2))
        when (isomorphic n1 n2) $ do
          -- trace "Adding isomorphic descendants"
          modify $ over stateM $ (:) (n1, n2)

    condition1 = do
      r :: TopDownReader <- ask
      s :: TopDownState  <- get
      let minHeight = view readerMinHeight r
      let l1 = view stateL1 s
      let l2 = view stateL2 s
      -- trace $ printf "l1: %s" (show l1)
      -- trace $ printf "l2: %s" (show l2)
      return $ min (peekMax l1) (peekMax l2) > minHeight

    parentDice :: [(Node, Node)] -> (Node, Node) -> Double
    parentDice m (t1, t2) =
      case (parent t1, parent t2) of
      (Just p1, Just p2) -> dice m p1 p2
      _ -> 0

    condition2 = do
      s :: TopDownState <- get
      let a = view stateA s
      return $ length a > 0

data BottomUpReader = BottomUpReader
  { _bottomUpReaderRoot1 :: Node
  , _bottomUpReaderRoot2 :: Node
  , _bottomUpReaderMinDice :: Double
  }
  deriving (Show)
makeLenses ''BottomUpReader

data BottomUpState = BottomUpState
  { _bottomUpStateM  :: Mapping
  }
  deriving (Show)
makeLenses ''BottomUpState

getM :: ∀ r.
  ( Member (State BottomUpState) r
  ) => Eff r Mapping
getM = view bottomUpStateM <$> get

isMatched1 ::
  ( Member (State BottomUpState) r
  ) =>
  Node -> Eff r Bool
isMatched1 n = List.any ((==) n . fst) <$> getM

isMatched2 ::
  ( Member (State BottomUpState) r
  ) =>
  Node -> Eff r Bool
isMatched2 n = List.any ((==) n . snd) <$> getM

isUnmatched1 ::
  ( Member (State BottomUpState) r
  ) =>
  Node -> Eff r Bool
isUnmatched1 n = not <$> isMatched1 n

isUnmatched2 ::
  ( Member (State BottomUpState) r
  ) =>
  Node -> Eff r Bool
isUnmatched2 n = not <$> isMatched2 n

hasMatchedChildren1 ::
  ( Member (State BottomUpState) r
  ) =>
  Node -> Eff r Bool
hasMatchedChildren1 n = anyM isMatched1 (children n)

candidate :: ∀ r.
  ( Member (Reader BottomUpReader) r
  , Member (State  BottomUpState) r
  , Member Trace r
  ) => Node -> Eff r (Maybe Node)
candidate t1 = do
  root2 <- view bottomUpReaderRoot2 <$> ask
  candidates root2 t1 >>= \case
    [] -> return Nothing
    cs -> do
      trace $ printf "Candidates for %s" (preview . node $ t1)
      trace $ printf "%s" (show cs)
      m <- getM
      return $ Just (List.maximumBy (comparing (dice m t1)) cs)
  where
    candidates :: Node -> Node -> Eff r [Node]
    candidates root2 t1 = filterM (isCandidateFor t1) (nodeAndDescendants root2)
    isCandidateFor t1 t2 = do
      unmatched <- isUnmatched2 t2
      matchingDescendants <- anyM areMatchingDescendants
                             (product (nodeDescendants t1) (nodeDescendants t2))
      return $ label t1 == label t2 && unmatched && matchingDescendants
    areMatchingDescendants (t1, t2) = List.elem (t1, t2) <$> getM

runBottomUp :: ∀ r.
  ( Member Trace r
  ) =>
  Node -> Node -> Mapping -> Double -> Eff r BottomUpState
runBottomUp n1 n2 m minDice =
  snd <$> runState (runReader bottomUp r) s0
  where
    r = BottomUpReader
      { _bottomUpReaderMinDice = minDice
      , _bottomUpReaderRoot1   = n1
      , _bottomUpReaderRoot2   = n2
      }
    s0 = BottomUpState
      { _bottomUpStateM  = m
      }

bottomUp :: ∀ r.
  ( Member (Reader BottomUpReader) r
  , Member (State  BottomUpState) r
  , Member Trace r
  ) => Eff r ()
bottomUp = do

  -- trace "STARTING BOTTOM-UP PHASE"

  root1   <- view bottomUpReaderRoot1   <$> ask
  -- root2   <- view (bottomUpReaderRoot2   @α) <$> ask
  minDice <- view bottomUpReaderMinDice <$> ask

  forM_ [ t1 | t1 <- nodeAndDescendantsPostOrder root1 ] $ \ t1 -> do
    -- trace $ printf "BU: %s" (preview . node $ t1)
    -- whenM (isUnmatched1 t1)        $ trace "is unmatched"
    -- forM_ (children t1) $ \ c -> trace $ printf "child: %s" (show c)
    -- whenM (hasMatchedChildren1 t1) $ trace "has matched children"
    whenM (isUnmatched1 t1 <&&> hasMatchedChildren1 t1) $ do
      -- trace $ "let's find a candidate"
      candidate t1 >>= \case
        Nothing -> return ()
        Just t2 -> do
          -- trace $ printf "Found a candidate: %s" (preview . node $ t2)
          m <- getM
          -- trace $ printf "Candidate dice: %s" (show $ dice m t1 t2)
          when (dice m t1 t2 > minDice) $ do
            -- trace "Adding bottom-up mapping"
            modify $ over bottomUpStateM $ List.union [(t1, t2)]
            -- TODO: opt

algorithm ::
  ( Member Trace r
  ) =>
  Int -> Double -> Int -> Node -> Node -> Eff r Mapping
algorithm minHeight minDice _maxSize n1 n2 = do
  s1 <- runTopDown  n1 n2 minHeight
  let m1 = view stateM s1
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
              let (Matched _ _ : pairsRest) = pairs -- if this fails, think about it
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

          (_, _, Pi α' _ _, [τ1', τ2']) -> do
            δ1 <- go n1 τ1'
            δ2 <- go n1 τ2'
            return $ ΔT.InsPi α' δ1 (Binder Nothing) δ2

          (Var _ _, _, Var _ _, _) -> do
            return $ ΔT.Replace (node n2)

          _ -> do
            trace $ printf "TODO: (%s, %s)" (show $ node n1) (show $ node n2)
            error "TODO"

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
        let (b, t') = unscopeTerm bt'
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
        let (b, τ2') = unscopeTerm bτ2'
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

-- main :: Node -> Node -> IO ()
-- main n1 n2 = do
--   m <- recommended n1 n2
--   runTrace (mkGuessδ n1 n2 m) >>= \case
--     Nothing -> printf "Could not guess a diff"
--     Just δ  -> do
--       (runTrace $ runError $ ΔT.patch (node n1) δ) >>= \case
--         Left  e -> printf e
--         Right p ->
--           if node n2 == p
--           then printf "Guessed a correct diff!"
--           else printf "Guessed a bad diff..."
--   return ()
