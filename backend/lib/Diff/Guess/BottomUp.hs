{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module Diff.Guess.BottomUp
  ( bottomUpStateM
  , runBottomUp
  ) where

import Control.Lens (makeLenses, over, view)
import Control.Monad
import Control.Monad.Extra (whenM)
import Control.Monad.Loops (anyM)
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.Freer.State
import Control.Monad.Freer.Trace
import Data.List (maximumBy, union)
import Data.Ord (comparing)
import Prelude hiding (product)
import Text.Printf
import Util ((<&&>))

import Diff.Guess.Mapping
import Diff.Guess.Node
import PrettyPrinting.PrettyPrintable

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
isMatched1 n = any ((==) n . fst) <$> getM

isMatched2 ::
  ( Member (State BottomUpState) r
  ) =>
  Node -> Eff r Bool
isMatched2 n = any ((==) n . snd) <$> getM

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
      return $ Just (maximumBy (comparing (dice m t1)) cs)
  where
    candidates :: Node -> Node -> Eff r [Node]
    candidates root2 t1 = filterM (isCandidateFor t1) (nodeAndDescendants root2)
    isCandidateFor t1 t2 = do
      unmatched <- isUnmatched2 t2
      matchingDescendants <- anyM areMatchingDescendants
                             (product (nodeDescendants t1) (nodeDescendants t2))
      return $ label t1 == label t2 && unmatched && matchingDescendants
    areMatchingDescendants (t1, t2) = elem (t1, t2) <$> getM

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
            modify $ over bottomUpStateM $ union [(t1, t2)]
            -- TODO: opt