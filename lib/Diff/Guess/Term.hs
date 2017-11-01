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
  ) where

import           Control.Lens hiding (children, preview)
import           Control.Monad
import           Control.Monad.Extra (whenM)
import           Control.Monad.Freer
import           Control.Monad.Freer.Reader
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Trace
import           Control.Monad.Loops
import           Data.Function
import qualified Data.List as List
import           Data.Ord
import           Prelude hiding (product)
import           Text.Printf
import           Util ((<&&>))

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

termChildren :: TermX α Variable -> [TermX α Variable]
termChildren = go
  where
    go = \case
      Annot _ t τ    -> [t, τ]
      App   _ a b    -> [a, b]
      Hole  _        -> []
      Lam   _ bt     -> [t]      where (_, t) = unscopeTerm bt
      Let   _ t1 bt2 -> [t1, t2] where (_, t2) = unscopeTerm bt2
      Match _ t  bs  -> t : map branchChild bs
      Pi    _ τ1 bτ2 -> [τ1, τ2] where (_, τ2) = unscopeTerm bτ2
      Type  _        -> []
      Var   _ _      -> []

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

peekMax :: HIList α -> Int
peekMax (HIList [])      = 0
peekMax (HIList (h : _)) = height h

newtype HIList α = HIList { unHIList :: [Node α] }
  deriving (Show)

insert :: Node α -> HIList α -> HIList α
insert n = HIList . List.insertBy (comparing $ Down . height) n . unHIList

product :: [Node α] -> [Node α] -> [(Node α, Node α)]
product l1 l2 = [ (a, b) | a <- l1, b <- l2 ]

{- dice computes a ratio of subnodes that are isomorphic -}
dice :: [(Node α, Node α)] -> Node α -> Node α -> Double
dice m t1 t2 = (2 * lengthOf t1s) / (lengthOf s1 + lengthOf s2)
  where
    lengthOf = fromIntegral . List.length
    t1s = filter (\ t1' -> (t1', t2) `elem` m) s1
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
  show n = printf "(%s : %s)" (show . identifier $ n) (label n)

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
open t lens = forM_ (children t) $ \ c -> do
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
  ) => Eff r ()
topDown = do

  root1 <- view (readerRoot1 @α) <$> ask
  root2 <- view (readerRoot2 @α) <$> ask
  push root1 stateL1
  push root2 stateL2

  whileM_ condition1 $ do
    l1 <- view (stateL1 @α) <$> get
    l2 <- view (stateL2 @α) <$> get

    if peekMax l1 /= peekMax l2
      then do
      if peekMax l1 > peekMax l2
        then do
        ts <- pop @α stateL1
        forM_ ts $ \t -> open t stateL1
        else do
        ts <- pop @α stateL2
        forM_ ts $ \t -> open t stateL2
      else do
      h1 <- pop @α stateL1
      h2 <- pop @α stateL2
      forM_ (product h1 h2) $ \ (t1, t2) -> do
        let set1 = nodeAndDescendants root1
        let set2 = nodeAndDescendants root2
        when (isomorphic t1 t2) $ do
          if (  List.any (\ tx -> isomorphic t1 tx && tx /= t2) set2
             || List.any (\ tx -> isomorphic tx t2 && tx /= t1) set1
             )
            then do
            modify $ over stateA $ (:) (t1, t2)
            else do
            addIsomorphicDescendants t1 t2

    m <- view stateM <$> get
    modify $ over stateA $ List.sortBy (comparing (parentDice m))

    whileM_ condition2 $ do
      (t1, t2) <- popHead @α stateA
      addIsomorphicDescendants t1 t2
      modify $ over stateA $ filter ((==) t1 . fst) . filter ((==) t2 . snd)

  where

    addIsomorphicDescendants t1 t2 = do
      let st1 = nodeAndDescendants t1
      let st2 = nodeAndDescendants t2
      forM_ (product st1 st2) $ \ (n1, n2) -> do
        when (isomorphic n1 n2) $ do
          modify $ over stateM $ (:) (n1, n2)

    condition1 = do
      r :: TopDownReader α <- ask
      s :: TopDownState  α <- get
      let minHeight = view readerMinHeight r
      let l1 = view stateL1 s
      let l2 = view stateL2 s
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
  ) => Node α -> Eff r (Maybe (Node α))
candidate t1 = do
  root2 <- view (bottomUpReaderRoot2 @α) <$> ask
  candidates root2 t1 >>= \case
    [] -> return Nothing
    cs -> do
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
  ) => Eff r ()
bottomUp = do

  root1   <- view (bottomUpReaderRoot1   @α) <$> ask
  -- root2   <- view (bottomUpReaderRoot2   @α) <$> ask
  minDice <- view (bottomUpReaderMinDice @α) <$> ask

  forM_ [ t1 | t1 <- nodeAndDescendantsPostOrder root1 ] $ \ t1 -> do
    whenM (isUnmatched1 t1 <&&> hasMatchedChildren1 t1) $ do
      candidate t1 >>= \case
        Nothing -> return ()
        Just t2 -> do
          m <- getM
          when (dice m t1 t2 > minDice) $ do
            modify $ over bottomUpStateM $ List.union [(t1, t2)]
            -- TODO: opt

algorithm ::
  ( Member Trace r
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
  ) =>
  Node α -> Node α -> Eff r (Mapping α)
recommended = algorithm 0 0.5 100

guessδ :: TermX α Variable -> TermX α Variable -> Maybe (ΔT.Diff α)
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

mkGuessδ ::
  ( Member Trace r
  ) =>
  Node α -> Node α -> Mapping α -> Eff r (Maybe (ΔT.Diff α))
mkGuessδ n1 n2 m = go n1 n2

  where

    go n1 n2 = do

      trace $ printf "Guessing δ for (%s, %s)" (preview (node n1)) (preview (node n2))

      case () of
        () | (n1, n2) `elem` m -> do
               if isomorphic n1 n2
                 then return $ Just ΔT.Same
                 else do
                 trace "A"
                 return Nothing
           | height n1 > height n2 -> do
               return $ Just $ ΔT.Replace (node n2)
           | height n2 > height n1 -> do
               case (node n2, children n2) of
                 (Pi α _ _, [τ1, τ2]) -> do
                   δ1 <- go n1 τ1
                   δ2 <- go n1 τ2
                   return $ ΔT.InsPi <$> Just α <*> δ1 <*> Just (Binder Nothing) <*> δ2
                 _ -> do
                   trace  "D"
                   return Nothing
           | otherwise -> do
               return $ Just $ ΔT.Replace (node n2)

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
