module CodeMirror.Position where

import Prelude
import Data.Array as Array
import Data.String as String
import Data.String.Utils as String.Utils
import Control.Apply (applySecond, lift2)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.State.Class (class MonadState, get, gets, modify, put)
import Data.Generic (class Generic, gEq, gShow)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)

type Position =
  { line :: Int
  , ch   :: Int
  }

-- wrapper to use when you want to `(==)` or `show`
newtype Position' = Position' Position

derive instance genericPosition' :: Generic Position'

instance eqPosition' :: Eq Position' where
  eq = gEq

instance showPosition' :: Show Position' where
  show = gShow

data Strictness
  = Strictly
  | NotStrictly

-- | `isBefore s p1 p2` if p1 before p2
isBefore :: Strictness -> Position -> Position -> Boolean
isBefore strictness { line : l1, ch : c1 } { line : l2, ch : c2 } =
  l1 < l2 || (l1 == l2 && before strictness c1 c2)
  where
    before Strictly    a b = a <  b
    before NotStrictly a b = a <= b

-- | `isAfter s p1 p2` if p1 is after p2
isAfter :: Strictness -> Position -> Position -> Boolean
isAfter s a b = isBefore s b a

-- | `isWithinRange sf from st to x` if `x` is within the range from `from` to `to`
isWithinRange :: Strictness -> Position -> Strictness -> Position -> Position -> Boolean
isWithinRange sf from st to x = isAfter sf x from && isBefore st x to

initialPosition :: Position
initialPosition =
  { line : 0
  , ch : 0
  }

bumpLine :: Position -> Position
bumpLine { line, ch } = { line : line + 1, ch : 0 }

bumpCh :: Position -> Position
bumpCh { line, ch } = { line : line, ch : ch + 1 }

type PositionState =
  { docAfter  :: String
  , docBefore :: String
  , position  :: Position
  }

-- | Moves the position state by one character, returns the character that was after and is now before
forward :: ∀ m. MonadState PositionState m => m (Maybe Char)
forward = do
  { docAfter, docBefore, position } <- get
  if String.length docAfter == 0
    then pure Nothing
    else do
      String.uncons docAfter # traverse \ { head, tail } -> do
        modify (\ s -> s { docAfter  = tail
                         , docBefore = docBefore <> String.singleton head
                         , position  = if head == '\n' then bumpLine position else bumpCh position
                         })
        pure head

-- | Returns the position at which the document suffix satisfies a condition
findFirstPositionWhere ::
  ∀ m.
  MonadRec m =>
  MonadState PositionState m =>
  (String -> Boolean) -> m (Maybe Position)
findFirstPositionWhere cond = tailRecM go unit
  where
    go unit = do
      s <- get
      if cond s.docAfter
        then pure $ Done $ Just s.position
        else forward `applySecond` (pure $ Loop $ unit)

stringAtPosition :: Position -> String -> Maybe String
stringAtPosition { line, ch } s =
  let ls = String.Utils.lines s in
  let remainingLines = Array.drop line ls in
  do
    ({ head, tail }) <- Array.uncons remainingLines
    if ch > String.length head
      then Nothing
      else Just (String.joinWith "\n" ((String.drop ch head) Array.: tail))

makePositionState :: String -> PositionState
makePositionState s =
  { docAfter  : s
  , docBefore : ""
  , position  : initialPosition
  }

moveToPosition ::
  ∀ m.
  MonadRec m =>
  MonadState PositionState m =>
  Position -> m Unit
moveToPosition target = tailRecM go unit
  where
    go unit = do
      p <- gets _.position
      if Position' p == Position' target
        then pure $ Done unit
        else forward *> (pure $ Loop unit)

-- | `peek n` lets you peek `n` characters into the document, not modifying the state
peek :: ∀ m. MonadState PositionState m => Int -> m String
peek n0 = do
  s <- get
  r <- go n0
  put s
  pure r
  where
    go n = if n == 0
           then pure ""
           else lift2 squash forward (peek (n - 1))
    squash Nothing  s = s
    squash (Just c) s = String.singleton c <> s

reachedDocEnd :: ∀ m. MonadState PositionState m => m Boolean
reachedDocEnd = do
  s <- gets _.docAfter
  pure (String.length s == 0)

-- | `addPosition p1 p2` starts from position `p1` and adds `p2`, relatively
addPosition :: Position -> Position -> Position
addPosition { line : l1, ch : c1 } { line : l2, ch : c2 } =
  { line : l1 + l2, ch : if l2 == 0 then c1 + c2 else c2 }
