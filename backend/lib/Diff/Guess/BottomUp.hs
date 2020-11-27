{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Diff.Guess.BottomUp
  ( bottomUpStateM,
    runBottomUp,
  )
where

import Control.Lens (makeLenses, over, view)
import Control.Monad (filterM, forM_, when)
import Control.Monad.Extra (whenM)
import Control.Monad.Loops (anyM)
import Data.List (maximumBy, union)
import Data.Ord (comparing)
import Diff.Guess.Mapping (Mapping)
import Diff.Guess.Node
  ( Node (children, node),
    dice,
    label,
    nodeAndDescendants,
    nodeAndDescendantsPostOrder,
    nodeDescendants,
    product,
  )
import Language (Language (Chick))
import Polysemy (Member, Sem)
import Polysemy.Reader (Reader, ask, runReader)
import Polysemy.State (State, get, modify, runState)
import Polysemy.Trace (Trace, trace)
import PrettyPrinting.PrettyPrintable (PrettyPrintable (preview))
import Text.Printf (printf)
import Util ((<&&>))
import Prelude hiding (product)

data BottomUpReader = BottomUpReader
  { _bottomUpReaderRoot1 :: Node,
    _bottomUpReaderRoot2 :: Node,
    _bottomUpReaderMinDice :: Double
  }
  deriving (Show)

makeLenses ''BottomUpReader

newtype BottomUpState = BottomUpState
  { _bottomUpStateM :: Mapping
  }
  deriving (Show)

makeLenses ''BottomUpState

getM ::
  forall r.
  Member (State BottomUpState) r =>
  Sem r Mapping
getM = view bottomUpStateM <$> get

isMatched1 ::
  ( Member (State BottomUpState) r
  ) =>
  Node ->
  Sem r Bool
isMatched1 n = any ((==) n . fst) <$> getM

isMatched2 ::
  ( Member (State BottomUpState) r
  ) =>
  Node ->
  Sem r Bool
isMatched2 n = any ((==) n . snd) <$> getM

isUnmatched1 ::
  ( Member (State BottomUpState) r
  ) =>
  Node ->
  Sem r Bool
isUnmatched1 n = not <$> isMatched1 n

isUnmatched2 ::
  ( Member (State BottomUpState) r
  ) =>
  Node ->
  Sem r Bool
isUnmatched2 n = not <$> isMatched2 n

hasMatchedChildren1 ::
  ( Member (State BottomUpState) r
  ) =>
  Node ->
  Sem r Bool
hasMatchedChildren1 n = anyM isMatched1 (children n)

candidate ::
  forall r.
  ( Member (Reader BottomUpReader) r,
    Member (State BottomUpState) r,
    Member Trace r
  ) =>
  Node ->
  Sem r (Maybe Node)
candidate t1 = do
  root2 <- view bottomUpReaderRoot2 <$> ask
  candidates root2 t1 >>= \case
    [] -> return Nothing
    cs -> do
      trace $ printf "Candidates for %s" (preview @ 'Chick . node $ t1)
      trace $ printf "%s" (show cs)
      m <- getM
      return $ Just (maximumBy (comparing (dice m t1)) cs)
  where
    candidates :: Node -> Node -> Sem r [Node]
    candidates root2 t1 = filterM (isCandidateFor t1) (nodeAndDescendants root2)
    isCandidateFor t1 t2 = do
      unmatched <- isUnmatched2 t2
      matchingDescendants <-
        anyM
          areMatchingDescendants
          (product (nodeDescendants t1) (nodeDescendants t2))
      return $ label t1 == label t2 && unmatched && matchingDescendants
    areMatchingDescendants (t1, t2) = elem (t1, t2) <$> getM

runBottomUp ::
  forall r.
  Member Trace r =>
  Node ->
  Node ->
  Mapping ->
  Double ->
  Sem r BottomUpState
runBottomUp n1 n2 m minDice =
  fst <$> runState s0 (runReader r bottomUp)
  where
    r =
      BottomUpReader
        { _bottomUpReaderMinDice = minDice,
          _bottomUpReaderRoot1 = n1,
          _bottomUpReaderRoot2 = n2
        }
    s0 =
      BottomUpState
        { _bottomUpStateM = m
        }

bottomUp ::
  forall r.
  Member (Reader BottomUpReader) r =>
  Member (State BottomUpState) r =>
  Member Trace r =>
  Sem r ()
bottomUp = do
  -- trace "STARTING BOTTOM-UP PHASE"

  root1 <- view bottomUpReaderRoot1 <$> ask
  -- root2   <- view (bottomUpReaderRoot2   @α) <$> ask
  minDice <- view bottomUpReaderMinDice <$> ask

  forM_ (nodeAndDescendantsPostOrder root1) $ \t1 ->
    -- trace $ printf "BU: %s" (preview . node $ t1)
    -- whenM (isUnmatched1 t1)        $ trace "is unmatched"
    -- forM_ (children t1) $ \ c -> trace $ printf "child: %s" (show c)
    -- whenM (hasMatchedChildren1 t1) $ trace "has matched children"
    whenM (isUnmatched1 t1 <&&> hasMatchedChildren1 t1) $
      -- trace $ "let's find a candidate"
      candidate t1 >>= \case
        Nothing -> return ()
        Just t2 -> do
          -- trace $ printf "Found a candidate: %s" (preview . node $ t2)
          m <- getM
          -- trace $ printf "Candidate dice: %s" (show $ dice m t1 t2)
          when (dice m t1 t2 > minDice) $
            -- trace "Adding bottom-up mapping"
            modify $ over bottomUpStateM $ union [(t1, t2)]

-- TODO: opt
