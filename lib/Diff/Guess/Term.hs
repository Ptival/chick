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
import           Data.Ord
import           Prelude hiding (product)
import           Text.Printf
import           Util ((<&&>), count)

import qualified Diff.Atom as ΔA
import qualified Diff.Term as ΔT
import           PrettyPrinting.Term ()
import           PrettyPrinting.PrettyPrintable
import           Term.Term
import           Utils

branchChild :: Branch α Variable -> TermX α Variable
branchChild = view _3 . unpackBranch

label :: Node α -> String
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
        Right (cs, c) -> map (variableFromBinder . view _2) cs ++ [c]
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

nodeDescendants :: Node α -> [Node α]
nodeDescendants t = c ++ concatMap nodeDescendants c
  where c = children t

nodeAndDescendants :: Node α -> [Node α]
nodeAndDescendants n = n : nodeDescendants n

nodeAndDescendantsPostOrder :: Node α -> [Node α]
nodeAndDescendantsPostOrder n =
  concatMap nodeAndDescendantsPostOrder (children n) ++ [n]

termHeight :: TermX α Variable -> Int
termHeight t = case termChildren t of
  [] -> 1 -- huh... why would they choose this...?
  c  -> 1 + maximum (map termHeight c)

newtype HIList α = HIList { unHIList :: [Node α] }

instance Show α => Show (HIList α) where
  show = show . unHIList

peekMax :: HIList α -> Int
peekMax (HIList [])      = 0
peekMax (HIList (h : _)) = height h

insert :: Node α -> HIList α -> HIList α
insert n = HIList . List.insertBy (comparing $ Down . height) n . unHIList

product :: [Node α] -> [Node α] -> [(Node α, Node α)]
product l1 l2 = [ (a, b) | a <- l1, b <- l2 ]

{- dice computes a ratio of subnodes that are isomorphic -}
dice :: [(Node α, Node α)] -> Node α -> Node α -> Double
dice m t1 t2 = (2 * fromIntegral c) / (lengthOf s1 + lengthOf s2)
  where
    lengthOf = fromIntegral . List.length
    c = count (`elem` m) (product s1 s2)
    s1 = nodeDescendants t1
    s2 = nodeDescendants t2

isomorphic :: Node α -> Node α -> Bool
isomorphic n1 n2 =
  height n1 == height n2 -- maybe this helps performance?
  && node n1 == node n2

data Node α = Node
  { children   :: [Node α]
  , height     :: Int
  , identifier :: Int
  , node       :: TermX α Variable
  , parent     :: Maybe (Node α)
  }

type Mapping α = [(Node α, Node α)]

instance Show α => Show (Node α) where
  show n = printf "(%s : %s)" (show . identifier $ n) (preview . node $ n)

fresh :: (Member (State Int) r) => Eff r Int
fresh = get <* modify ((+) (1 :: Int))

makeNode :: (Member (State Int) r) => TermX α Variable -> Eff r (Node α)
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

instance Eq (Node α) where
  (==) n1 n2 = identifier n1 == identifier n2

data TopDownReader α = TopDownReader
  { _readerMinHeight :: Int
  , _readerRoot1 :: Node α
  , _readerRoot2 :: Node α
  }
  deriving (Show)
makeLenses ''TopDownReader

data TopDownState α = TopDownState
  { _stateL1 :: HIList α
  , _stateL2 :: HIList α
  , _stateA  :: Mapping α
  , _stateM  :: Mapping α
  }
  deriving (Show)
makeLenses ''TopDownState

open ::
  ( Member (State (TopDownState α)) r
  ) => Node α -> Lens' (TopDownState α) (HIList α) -> Eff r ()
open t lens = forM_ (reverse $ children t) $ \ c -> do
  modify $ over lens $ insert c

pop ::
  ( Member (State (TopDownState α)) r
  ) => Lens' (TopDownState α) (HIList  α) -> Eff r [Node α]
pop lens = do
  l <- view lens <$> get
  let (popped, l') = List.partition ((==) (peekMax l) . height) (unHIList l)
  modify $ over lens $ const (HIList l')
  return popped

popHead ::
  ( Member (State (TopDownState α)) r
  ) => Lens' (TopDownState α) [a] -> Eff r a
popHead lens = do
  l <- view lens <$> get
  case l of
    [] -> error "popHead on empty list"
    h : t -> do
      modify $ over lens $ const t
      return h

push ::
  ( Member (State (TopDownState α)) r
  ) => Node α -> Lens' (TopDownState α) (HIList α) -> Eff r ()
push node lens = do
  modify $ over lens $ insert node

runTopDown :: ∀ α r.
  ( Member Trace r
  , Show α
  ) =>
  Node α -> Node α -> Int -> Eff r (TopDownState α)
runTopDown n1 n2 minHeight =
  snd <$> runState (runReader (topDown @α) r) s0
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

topDown :: ∀ α r.
  ( Member (Reader (TopDownReader α)) r
  , Member (State  (TopDownState  α)) r
  , Member Trace r
  , Show α
  ) =>
  Eff r ()
topDown = do

  root1 <- view (readerRoot1 @α) <$> ask
  root2 <- view (readerRoot2 @α) <$> ask
  push root1 stateL1
  push root2 stateL2

  whileM_ condition1 $ do

    trace "topDown loop"

    l1 <- view (stateL1 @α) <$> get
    l2 <- view (stateL2 @α) <$> get

    if peekMax l1 /= peekMax l2
      then do
      if peekMax l1 > peekMax l2
        then do
        trace "pop l1"
        ts <- pop @α stateL1
        forM_ ts $ \t -> open t stateL1
        else do
        trace "pop l2"
        ts <- pop @α stateL2
        forM_ ts $ \t -> open t stateL2
      else do
      trace "pop both"
      h1 <- pop @α stateL1
      h2 <- pop @α stateL2
      trace $ printf "h1: %s" (show h1)
      trace $ printf "h2: %s" (show h2)
      forM_ (product h1 h2) $ \ (t1, t2) -> do
        let set1 = nodeAndDescendants root1
        let set2 = nodeAndDescendants root2
        when (isomorphic t1 t2) $ do
          trace "ISOMORPHIC!"
          if (  List.any (\ tx -> isomorphic t1 tx && tx /= t2) set2
             || List.any (\ tx -> isomorphic tx t2 && tx /= t1) set1
             )
            then do
            trace $ printf "Adding candidate mapping (%s, %s)" (show t1) (show t2)
            modify $ over stateA $ (:) (t1, t2)
            else do
            addIsomorphicDescendants t1 t2
      a <- view (stateA @α) <$> get
      m <- view (stateM @α) <$> get
      let aum = List.union a m
      forM_ [ t1 | t1 <- h1
                 , not (List.any ((==) t1 . fst) aum) ] $ \ t1 -> do
        open t1 stateL1
      forM_ [ t2 | t2 <- h2
                 , not (List.any ((==) t2 . snd) aum) ] $ \ t2 -> do
        open t2 stateL2

  trace "Now sorting candidate mappings by dice"
  m <- view stateM <$> get
  modify $ over stateA $ List.sortBy (comparing (parentDice m))

  a <- view (stateA @α) <$> get
  forM_ a $ \ (t1, t2) -> do
    trace $ printf "A: (%s, %s)" (show $ t1) (show $ t2)
    trace $ printf "Parents : %s, %s"
      (show $ preview . node <$> parent t1)
      (show $ preview . node <$> parent t2)
    trace $ printf "Parent dice: %s" (show . parentDice m $ (t1, t2))
  trace $ printf "About to go through:\n%s" (show a)
  whileM_ condition2 $ do
    (t1, t2) <- popHead @α stateA
    trace $ printf "Popped %s" (show (t1, t2))
    addIsomorphicDescendants t1 t2
    modify $ over stateA $ filter ((/=) t1 . fst) . filter ((/=) t2 . snd)

  trace "END OF TOP-DOWN PHASE:"
  m <- view (stateM @α) <$> get
  trace $ show m

  where

    addIsomorphicDescendants t1 t2 = do
      let st1 = nodeAndDescendants t1
      let st2 = nodeAndDescendants t2
      trace $ printf "Node and descendants for %s" (show (t1, t2))
      trace $ printf "t1: %s" (preview . node $ t1)
      trace $ printf "t2: %s" (preview . node $ t2)
      trace $ printf "are %s" (show (st1, st2))
      forM_ (product st1 st2) $ \ (n1, n2) -> do
        trace $ printf "Pondering: %s" (show (n1, n2))
        when (isomorphic n1 n2) $ do
          trace "Adding isomorphic descendants"
          modify $ over stateM $ (:) (n1, n2)

    condition1 = do
      r :: TopDownReader α <- ask
      s :: TopDownState  α <- get
      let minHeight = view readerMinHeight r
      let l1 = view stateL1 s
      let l2 = view stateL2 s
      trace $ printf "l1: %s" (show l1)
      trace $ printf "l2: %s" (show l2)
      return $ min (peekMax l1) (peekMax l2) > minHeight

    parentDice :: [(Node α, Node α)] -> (Node α, Node α) -> Double
    parentDice m (t1, t2) =
      case (parent t1, parent t2) of
      (Just p1, Just p2) -> dice m p1 p2
      _ -> 0

    condition2 = do
      s :: TopDownState  α <- get
      let a = view stateA s
      return $ length a > 0

data BottomUpReader α = BottomUpReader
  { _bottomUpReaderRoot1 :: Node α
  , _bottomUpReaderRoot2 :: Node α
  , _bottomUpReaderMinDice :: Double
  }
  deriving (Show)
makeLenses ''BottomUpReader

data BottomUpState α = BottomUpState
  { _bottomUpStateM  :: Mapping α
  }
  deriving (Show)
makeLenses ''BottomUpState

getM :: ∀ α r.
  ( Member (State  (BottomUpState  α)) r
  ) => Eff r (Mapping α)
getM = view (bottomUpStateM @α) <$> get

isMatched1 ::
  ( Member (State (BottomUpState α)) r
  ) =>
  Node α -> Eff r Bool
isMatched1 n = List.any ((==) n . fst) <$> getM

isMatched2 ::
  ( Member (State (BottomUpState α)) r
  ) =>
  Node α -> Eff r Bool
isMatched2 n = List.any ((==) n . snd) <$> getM

isUnmatched1 ::
  ( Member (State (BottomUpState α)) r
  ) =>
  Node α -> Eff r Bool
isUnmatched1 n = not <$> isMatched1 n

isUnmatched2 ::
  ( Member (State (BottomUpState α)) r
  ) =>
  Node α -> Eff r Bool
isUnmatched2 n = not <$> isMatched2 n

hasMatchedChildren1 ::
  ( Member (State (BottomUpState α)) r
  ) =>
  Node α -> Eff r Bool
hasMatchedChildren1 n = anyM isMatched1 (children n)

candidate :: ∀ α r.
  ( Member (Reader (BottomUpReader α)) r
  , Member (State  (BottomUpState  α)) r
  , Member Trace r
  , Show α
  ) => Node α -> Eff r (Maybe (Node α))
candidate t1 = do
  root2 <- view (bottomUpReaderRoot2 @α) <$> ask
  candidates root2 t1 >>= \case
    [] -> return Nothing
    cs -> do
      trace $ printf "Candidates for %s" (preview . node $ t1)
      trace $ printf "%s" (show cs)
      m <- getM
      return $ Just (List.maximumBy (comparing (dice m t1)) cs)
  where
    candidates :: Node α -> Node α -> Eff r [Node α]
    candidates root2 t1 = filterM (isCandidateFor t1) (nodeAndDescendants root2)
    isCandidateFor t1 t2 = do
      unmatched <- isUnmatched2 t2
      matchingDescendants <- anyM areMatchingDescendants
                             (product (nodeDescendants t1) (nodeDescendants t2))
      return $ label t1 == label t2 && unmatched && matchingDescendants
    areMatchingDescendants (t1, t2) = List.elem (t1, t2) <$> getM

runBottomUp :: ∀ α r.
  ( Member Trace r
  , Show α
  ) =>
  Node α -> Node α -> Mapping α -> Double -> Eff r (BottomUpState α)
runBottomUp n1 n2 m minDice =
  snd <$> runState (runReader (bottomUp @α) r) s0
  where
    r = BottomUpReader
      { _bottomUpReaderMinDice = minDice
      , _bottomUpReaderRoot1   = n1
      , _bottomUpReaderRoot2   = n2
      }
    s0 = BottomUpState
      { _bottomUpStateM  = m
      }

bottomUp :: ∀ α r.
  ( Member (Reader (BottomUpReader α)) r
  , Member (State  (BottomUpState  α)) r
  , Member Trace r
  , Show α
  ) => Eff r ()
bottomUp = do

  trace "STARTING BOTTOM-UP PHASE"

  root1   <- view (bottomUpReaderRoot1   @α) <$> ask
  -- root2   <- view (bottomUpReaderRoot2   @α) <$> ask
  minDice <- view (bottomUpReaderMinDice @α) <$> ask

  forM_ [ t1 | t1 <- nodeAndDescendantsPostOrder root1 ] $ \ t1 -> do
    trace $ printf "BU: %s" (preview . node $ t1)
    whenM (isUnmatched1 t1)        $ trace "is unmatched"
    forM_ (children t1) $ \ c -> trace $ printf "child: %s" (show c)
    whenM (hasMatchedChildren1 t1) $ trace "has matched children"
    whenM (isUnmatched1 t1 <&&> hasMatchedChildren1 t1) $ do
      trace $ "let's find a candidate"
      candidate t1 >>= \case
        Nothing -> return ()
        Just t2 -> do
          trace $ printf "Found a candidate: %s" (preview . node $ t2)
          m <- getM
          trace $ printf "Candidate dice: %s" (show $ dice m t1 t2)
          when (dice m t1 t2 > minDice) $ do
            trace "Adding bottom-up mapping"
            modify $ over bottomUpStateM $ List.union [(t1, t2)]
            -- TODO: opt

algorithm ::
  ( Member Trace r
  , Show α
  ) =>
  Int -> Double -> Int -> Node α -> Node α -> Eff r (Mapping α)
algorithm minHeight minDice _maxSize n1 n2 = do
  s1 <- runTopDown  n1 n2 minHeight
  let m1 = view stateM s1
  s2 <- runBottomUp n1 n2 m1 minDice {- maxSize -}
  let m2 = view bottomUpStateM s2
  return m2

recommended ::
  ( Member Trace r
  , Show α
  ) =>
  Node α -> Node α -> Eff r (Mapping α)
recommended = algorithm 0 0.0 100

guessδ :: Show α => TermX α Variable -> TermX α Variable -> Maybe (ΔT.Diff α)
guessδ t1 t2 =
  let (n1, n2) = fst . run $ runState s (0 :: Int) in
  let m = skipTrace $ recommended n1 n2 in
  let δ = skipTrace $ mkGuessδ n1 n2 m in
  δ
  where
    s = do
      n1 <- makeNode t1
      n2 <- makeNode t2
      return (n1, n2)

traceGuessδ ::
  Show α =>
  TermX α Variable -> TermX α Variable -> IO (Maybe (ΔT.Diff α))
traceGuessδ t1 t2 = do
  let (n1, n2) = fst . run $ runState s (0 :: Int)
  m <- runTrace $ recommended n1 n2
  putStrLn $ printf "MAPPING:\n%s\n" (show m)
  δ <- runTrace $ mkGuessδ n1 n2 m
  return δ
  where
    s = do
      n1 <- makeNode t1
      n2 <- makeNode t2
      return (n1, n2)

matchPairs m l1 l2 = go l1 l2
  where
    go [] [] = []
    go l1@(n1 : t1) l2@(n2 : t2)
      | (n1, n2) `elem` m         = (Just n1, Just n2) : go t1 t2
      | List.any (matchesL n1) t2 = (Nothing, Just n2) : go l1 t2
      | List.any (matchesR n2) t1 = (Just n1, Nothing) : go t1 l2
      | otherwise                 = (Just n1, Nothing) : (Nothing, Just n2) : go t1 t2
    matchesL nL nR = (nL, nR) `elem` m
    matchesR nR nL = (nL, nR) `elem` m

mkGuessδ ::
  ( Member Trace r
  , Show α
  ) =>
  Node α -> Node α -> Mapping α -> Eff r (Maybe (ΔT.Diff α))
mkGuessδ n1 n2 m = go n1 n2

  where

    go n1 n2 = do

      trace $ printf "Guessing δ for (%s, %s)" (preview (node n1)) (preview (node n2))

      if (n1, n2) `elem` m
        then do
        if isomorphic n1 n2
          then do
          trace "Isomorphic!"
          return $ Just ΔT.Same
          else do
          trace "Not isomorphic!"
          case (node n1, node n2) of
            (Pi _ _ _, Pi _ _ _) -> do
              trace $ show $ node n1
              trace $ show $ node n2
              let pairs = matchPairs m (children n1) (children n2)
              (δ, _) <- foldM foldPis (id, (node n1, node n2)) pairs
              return $ Just $ δ ΔT.Same --(ΔT.Replace (Var Nothing (Variable "HERE")))
        else do

        case (node n1, children n1, node n2, children n2) of

          (Pi _ _ bτ2, τs, Pi α' _ bτ2', τs') -> do
            trace "YOLO"
            trace $ show $ matchPairs m (children n1) (children n2)
            return Nothing
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

          (Var _ _, _, Pi α' _ _, [τ1', τ2']) -> do
            δ1 <- go n1 τ1'
            δ2 <- go n1 τ2'
            return $ ΔT.InsPi <$> Just α' <*> δ1 <*> Just (Binder Nothing) <*> δ2

          _isomoprhic -> do
            trace  "D"
            return Nothing

    containMatches m n1 n2 = True

    foldPis (δ, (t, t')) = \case
      (Nothing, Nothing) -> error "This should not happen"
      (Nothing, Just _) -> do
        trace "Branch 1"
        let (Pi α' τ1' bτ2') = t'
        let (b, τ2') = unscopeTerm bτ2'
        return $ (δ . ΔT.InsPi α' (ΔT.Replace τ1') b, (t, τ2'))
      (Just _,  Nothing) -> do
        trace "Branch 2"
        let (Pi _ τ1 bτ2) = t
        let (b, τ2) = unscopeTerm bτ2
        return $ (δ . ΔT.RemovePi, (τ2, t'))
      (Just τ1, Just τ1') -> do
        case (t, t') of
          (Pi _ _ bτ2, Pi _ _ bτ2') -> do
            trace $ printf "TO THE LEFT: (%s, %s)" (show τ1) (show τ1')
            go τ1 τ1' >>= \case
              Nothing -> error "I think this should not happen"
              Just δ1 -> do
                trace $ printf "Diff for left branch:\n%s" (show δ1)
                let (_, τ2) = unscopeTerm bτ2
                let (_, τ2') = unscopeTerm bτ2'
                return $ (δ . ΔT.CpyPi δ1 ΔA.Same, (τ2, τ2'))
          (_, _) -> do
            -- nothing to do here?
            return $ (δ, (t, t'))

-- main :: Node α -> Node α -> IO ()
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
