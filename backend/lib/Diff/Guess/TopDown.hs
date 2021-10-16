{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Diff.Guess.TopDown
  ( runTopDown,
    topDownStateM,
  )
where

import Control.Lens (Lens', makeLenses, over, set, view)
import Control.Monad (forM_, when)
import Control.Monad.Loops (whileM_)
import Data.List (partition, sortOn, union)
import Diff.Guess.HIList (HIList (..), insert, peekMax)
import Diff.Guess.Mapping (Mapping)
import Diff.Guess.Node
  ( Node (children, height, parent),
    dice,
    isomorphic,
    nodeAndDescendants,
    product,
  )
import Polysemy (Member, Sem)
import Polysemy.Reader (Reader, ask, runReader)
import Polysemy.State (State, get, modify, runState)
import Polysemy.Trace (Trace, trace)
import Prelude hiding (product)

data TopDownReader = TopDownReader
  { _topDownReaderMinHeight :: Int,
    _topDownReaderRoot1 :: Node,
    _topDownReaderRoot2 :: Node
  }
  deriving (Show)

makeLenses ''TopDownReader

data TopDownState = TopDownState
  { _topDownStateL1 :: HIList,
    _topDownStateL2 :: HIList,
    _topDownStateA :: Mapping,
    _topDownStateM :: Mapping
  }
  deriving (Show)

makeLenses ''TopDownState

open ::
  Member (State TopDownState) r =>
  Node ->
  Lens' TopDownState HIList ->
  Sem r ()
open t lens = forM_ (reverse $ children t) $ \c ->
  modify $ over lens $ insert c

pop ::
  Member (State TopDownState) r =>
  Lens' TopDownState HIList ->
  Sem r [Node]
pop lens = do
  l <- view lens <$> get
  let (popped, l') = partition ((==) (peekMax l) . height) (unHIList l)
  modify $ set lens (HIList l')
  return popped

popHead ::
  Member (State TopDownState) r =>
  Lens' TopDownState [a] ->
  Sem r a
popHead lens = do
  l <- view lens <$> get
  case l of
    [] -> error "popHead on empty list"
    h : t -> do
      modify $ set lens t
      return h

push ::
  Member (State TopDownState) r =>
  Node ->
  Lens' TopDownState HIList ->
  Sem r ()
push n lens =
  modify $ over lens $ insert n

runTopDown ::
  forall r.
  Member Trace r =>
  Node ->
  Node ->
  Int ->
  Sem r TopDownState
runTopDown n1 n2 minHeight =
  fst <$> runState s0 (runReader r topDown)
  where
    r =
      TopDownReader
        { _topDownReaderMinHeight = minHeight,
          _topDownReaderRoot1 = n1,
          _topDownReaderRoot2 = n2
        }
    s0 =
      TopDownState
        { _topDownStateL1 = HIList [],
          _topDownStateL2 = HIList [],
          _topDownStateA = [],
          _topDownStateM = []
        }

topDown ::
  forall r.
  Member (Reader TopDownReader) r =>
  Member (State TopDownState) r =>
  Member Trace r =>
  Sem r ()
topDown = do
  root1 <- view topDownReaderRoot1 <$> ask
  root2 <- view topDownReaderRoot2 <$> ask
  push root1 topDownStateL1
  push root2 topDownStateL2

  whileM_ condition1 $ do
    trace "topDown loop"

    l1 <- view topDownStateL1 <$> get
    l2 <- view topDownStateL2 <$> get

    if peekMax l1 /= peekMax l2
      then
        if peekMax l1 > peekMax l2
          then do
            -- trace "pop l1"
            ts <- pop topDownStateL1
            forM_ ts $ \t -> open t topDownStateL1
          else do
            -- trace "pop l2"
            ts <- pop topDownStateL2
            forM_ ts $ \t -> open t topDownStateL2
      else do
        -- trace "pop both"
        h1 <- pop topDownStateL1
        h2 <- pop topDownStateL2
        -- trace $ printf "h1: %s" (show h1)
        -- trace $ printf "h2: %s" (show h2)
        forM_ (product h1 h2) $ \(t1, t2) -> do
          let set1 = nodeAndDescendants root1
          let set2 = nodeAndDescendants root2
          when (isomorphic t1 t2) $
            -- trace "ISOMORPHIC!"
            if any (\tx -> isomorphic t1 tx && tx /= t2) set2
              || any (\tx -> isomorphic tx t2 && tx /= t1) set1
              then -- trace $ printf "Adding candidate mapping (%s, %s)" (show t1) (show t2)
                modify $ over topDownStateA $ (:) (t1, t2)
              else addIsomorphicDescendants t1 t2
        a <- view topDownStateA <$> get
        m <- view topDownStateM <$> get
        let aum = a `union` m
        forM_
          [ t1 | t1 <- h1, not (any ((==) t1 . fst) aum)
          ]
          $ \t1 ->
            open t1 topDownStateL1
        forM_
          [ t2 | t2 <- h2, not (any ((==) t2 . snd) aum)
          ]
          $ \t2 ->
            open t2 topDownStateL2

  -- trace "Now sorting candidate mappings by dice"
  m <- view topDownStateM <$> get
  modify $ over topDownStateA $ sortOn (parentDice m)

  -- a <- view (topDownStateA @α) <$> get
  -- forM_ a $ \ (t1, t2) -> do
  --   trace $ printf "A: (%s, %s)" (show $ t1) (show $ t2)
  --   trace $ printf "Parents : %s, %s"
  --     (show $ preview . node <$> parent t1)
  --     (show $ preview . node <$> parent t2)
  --   trace $ printf "Parent dice: %s" (show . parentDice m $ (t1, t2))
  -- trace $ printf "About to go through:\n%s" (show a)
  whileM_ condition2 $ do
    (t1, t2) <- popHead topDownStateA
    -- trace $ printf "Popped %s" (show (t1, t2))
    addIsomorphicDescendants t1 t2
    modify $ over topDownStateA $ filter ((/=) t1 . fst) . filter ((/=) t2 . snd)
  where
    -- trace "END OF TOP-DOWN PHASE:"
    -- m <- view (topDownStateM @α) <$> get
    -- trace $ show m

    addIsomorphicDescendants t1 t2 = do
      let st1 = nodeAndDescendants t1
      let st2 = nodeAndDescendants t2
      -- trace $ printf "Node and descendants for %s" (show (t1, t2))
      -- trace $ printf "t1: %s" (preview . node $ t1)
      -- trace $ printf "t2: %s" (preview . node $ t2)
      -- trace $ printf "are %s" (show (st1, st2))
      forM_ (product st1 st2) $ \(n1, n2) ->
        -- trace $ printf "Pondering: %s" (show (n1, n2))
        when (isomorphic n1 n2) $
          -- trace "Adding isomorphic descendants"
          modify $ over topDownStateM $ (:) (n1, n2)

    condition1 = do
      r :: TopDownReader <- ask
      s :: TopDownState <- get
      let minHeight = view topDownReaderMinHeight r
      let l1 = view topDownStateL1 s
      let l2 = view topDownStateL2 s
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
      let a = view topDownStateA s
      return $ not (null a)
