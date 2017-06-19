-- | Based on https://hackage.haskell.org/package/wl-pprint-annotated

-- | Portions Copyright 2013, Google, Inc.
-- | Copyright 2011, Edward Kmett
-- | Copyright 2000, Daan Leijen
-- |
-- | All rights reserved.
-- |
-- | Redistribution and use in source and binary forms, with or without
-- | modification, are permitted provided that the following conditions are
-- | met:
-- |
-- |   * Redistributions of source code must retain the above copyright
-- |     notice, this list of conditions and the following disclaimer.
-- |
-- |   * Redistributions in binary form must reproduce the above copyright
-- |     notice, this list of conditions and the following disclaimer in
-- |     the documentation and/or other materials provided with the
-- |     distribution.
-- |
-- | This software is provided by the copyright holders "as is" and any
-- | express or implied warranties, including, but not limited to, the
-- | implied warranties of merchantability and fitness for a particular
-- | purpose are disclaimed. In no event shall the copyright holders be
-- | liable for any direct, indirect, incidental, special, exemplary, or
-- | consequential damages (including, but not limited to, procurement of
-- | substitute goods or services; loss of use, data, or profits; or
-- | business interruption) however caused and on any theory of liability,
-- | whether in contract, strict liability, or tort (including negligence
-- | or otherwise) arising in any way out of the use of this software, even
-- | if advised of the possibility of such damage.

module PrettyPrint.PrettyPrint where

import Prelude
import Data.List.Lazy as LazyList
import Data.String as S
import Control.Alternative ((<|>))
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array (fromFoldable, replicate, uncons, (:))
import Data.Foldable (class Foldable, foldl, foldr)
import Data.Int (round, toNumber)
import Data.List.Lazy (repeat)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Tuple (Tuple(..))

data PageWidth
  = AvailablePerLine Int Number
  | Unbounded

data Doc ann
  = Fail
  | Empty
  | Char Char
  | Text Int String
  | Line
  | FlatAlt (Doc ann) (Doc ann)
  | Cat (Doc ann) (Doc ann)
  | Nest Int (Doc ann)
  | Union (Doc ann) (Doc ann)
  | Column (Int -> Doc ann)
  | WithPageWidth (PageWidth -> Doc ann)
  | Nesting (Int -> Doc ann)
  | Annotated ann (Doc ann)

instance semigroupDoc :: Semigroup (Doc ann) where
    append = Cat
    -- sconcat (x :| xs) = hcat (x:xs)

instance monoidDoc :: Monoid (Doc ann) where
  mempty = emptyDoc
  --mconcat = hcat

data SimpleDocStream ann
  = SFail
  | SEmpty
  | SChar Char (SimpleDocStream ann)
  | SText Int String (SimpleDocStream ann)
  | SLine Int (SimpleDocStream ann)
  | SAnnPush ann (SimpleDocStream ann)
  | SAnnPop (SimpleDocStream ann)

newtype LayoutOptions = LayoutOptions { layoutPageWidth :: PageWidth }

newtype FittingPredicate ann
  = FittingPredicate (PageWidth -> Int -> Maybe Int -> SimpleDocStream ann -> Boolean)

data LayoutPipeline ann
  = Nil
  | Cons Int (Doc ann) (LayoutPipeline ann)
  | UndoAnn (LayoutPipeline ann)

layoutPretty ::
  ∀ ann.
  LayoutOptions ->
  Doc ann ->
  SimpleDocStream ann
layoutPretty = layoutWadlerLeijen
    (FittingPredicate (\_pWidth _minNestingLevel maxWidth sdoc -> case maxWidth of
        Nothing -> true
        Just w -> fits w sdoc ))
  where
    fits :: Int -- ^ Width in which to fit the first line
         -> SimpleDocStream ann
         -> Boolean
    fits w _ | w < 0      = false
    fits _ SFail          = false
    fits _ SEmpty         = true
    fits w (SChar _ x)    = fits (w - 1) x
    fits w (SText l _t x) = fits (w - l) x
    fits _ (SLine _ _)    = true
    fits w (SAnnPush _ x) = fits w x
    fits w (SAnnPop x)    = fits w x

layoutSmart
    :: ∀ ann. LayoutOptions
    -> Doc ann
    -> SimpleDocStream ann
layoutSmart = layoutWadlerLeijen
    (FittingPredicate (\pWidth minNestingLevel maxWidth sdoc -> case maxWidth of
        Nothing -> false
        Just w -> fits pWidth minNestingLevel w sdoc ))
  where
    -- Search with more lookahead: assuming that nesting roughly corresponds to
    -- syntactic depth, @fits@ checks that not only the current line fits, but
    -- the entire syntactic structure being formatted at this level of
    -- indentation fits. If we were to remove the second case for @SLine@, we
    -- would check that not only the current structure fits, but also the rest
    -- of the document, which would be slightly more intelligent but would have
    -- exponential runtime (and is prohibitively expensive in practice).
    fits :: PageWidth
         -> Int -- ^ Minimum nesting level to fit in
         -> Int -- ^ Width in which to fit the first line
         -> SimpleDocStream ann
         -> Boolean
    fits _ _ w _ | w < 0                    = false
    fits _ _ _ SFail                        = false
    fits _ _ _ SEmpty                       = true
    fits pw m w (SChar _ x)                 = fits pw m (w - 1) x
    fits pw m w (SText l _t x)              = fits pw m (w - l) x
    fits pw m _ (SLine i x)
      | m < i, AvailablePerLine cpl _ <- pw = fits pw m (cpl - i) x
      | otherwise                           = true
    fits pw m w (SAnnPush _ x)              = fits pw m w x
    fits pw m w (SAnnPop x)                 = fits pw m w x

layoutWadlerLeijen
    :: forall ann. FittingPredicate ann
    -> LayoutOptions
    -> Doc ann
    -> SimpleDocStream ann
layoutWadlerLeijen
    fittingPredicate
    (LayoutOptions { layoutPageWidth : pWidth })
    doc
  = best 0 0 (Cons 0 doc Nil)
  where

    -- * current column >= current nesting level
    -- * current column - current indentaion = number of chars inserted in line
    best
        :: Int -- Current nesting level
        -> Int -- Current column, i.e. "where the cursor is"
        -> LayoutPipeline ann -- Documents remaining to be handled (in order)
        -> SimpleDocStream ann
    best _ _ Nil           = SEmpty
    best nl cc (UndoAnn ds)  = SAnnPop (best nl cc ds)
    best nl cc (Cons i d ds) = case d of
        Fail            -> SFail
        Empty           -> best nl cc ds
        Char c          -> let cc' = cc+1 in SChar c (best nl cc' ds)
        Text l t        -> let cc' = cc+l in SText l t (best nl cc' ds)
        Line            -> SLine i (best i i ds)
        FlatAlt x _     -> best nl cc (Cons i x ds)
        Cat x y         -> best nl cc (Cons i x (Cons i y ds))
        Nest j x        -> let ij = i+j in best nl cc (Cons ij x ds)
        Union x y       -> let x' = best nl cc (Cons i x ds)
                               y' = best nl cc (Cons i y ds)
                           in selectNicer fittingPredicate nl cc x' y'
        Column f        -> best nl cc (Cons i (f cc) ds)
        WithPageWidth f -> best nl cc (Cons i (f pWidth) ds)
        Nesting f       -> best nl cc (Cons i (f i) ds)
        Annotated ann x -> SAnnPush ann (best nl cc (Cons i x (UndoAnn ds)))

    selectNicer
        :: FittingPredicate ann
        -> Int           -- ^ Current nesting level
        -> Int           -- ^ Current column
        -> SimpleDocStream ann -- ^ Choice A. Invariant: first lines should not be longer than B's.
        -> SimpleDocStream ann -- ^ Choice B.
        -> SimpleDocStream ann -- ^ Choice A if it fits, otherwise B.
    selectNicer (FittingPredicate fits) lineIndent currentColumn x y =
      if fits pWidth minNestingLevel availableWidth x then x else y
      where
        minNestingLevel = min lineIndent currentColumn
        ribbonWidth = case pWidth of
            AvailablePerLine lineLength ribbonFraction ->
                (Just <<< max 0 <<< min lineLength <<< round)
                    (toNumber lineLength * ribbonFraction)
            Unbounded -> Nothing
        availableWidth = do
            columnsLeftInLine <- case pWidth of
                AvailablePerLine cpl _ribbonFrac -> Just (cpl - currentColumn)
                Unbounded -> Nothing
            columnsLeftInRibbon <- do
                li <- Just lineIndent
                rw <- ribbonWidth
                cc <- Just currentColumn
                Just (li + rw - cc)
            Just (min columnsLeftInLine columnsLeftInRibbon)

layoutCompact :: ∀ ann. Doc ann -> SimpleDocStream ann
layoutCompact doc = scan 0 [doc]
  where
    scan col l0 = case uncons l0 of
      Nothing -> SEmpty
      Just { head : d, tail : ds } ->
        case d of
          Fail            -> SFail
          Empty           -> scan col ds
          Char c          -> SChar c (scan (col+1) ds)
          Text l t        -> let col' = col+l in SText l t (scan col' ds)
          FlatAlt x _     -> scan col (x:ds)
          Line            -> SLine 0 (scan 0 ds)
          Cat x y         -> scan col (x:y:ds)
          Nest _ x        -> scan col (x:ds)
          Union _ y       -> scan col (y:ds)
          Column f        -> scan col (f col:ds)
          WithPageWidth f -> scan col (f Unbounded : ds)
          Nesting f       -> scan col (f 0 : ds)
          Annotated _ x   -> scan col (x:ds)

class Pretty a where
    pretty :: ∀ ann. a -> Doc ann
    prettyList :: ∀ ann. Array a -> Doc ann

prettyListDefault :: ∀ a ann. Pretty a => Array a -> Doc ann
prettyListDefault = list <<< map pretty

instance prettyChar :: Pretty Char where
    pretty '\n' = line
    pretty c = Char c
    prettyList = pretty <<< S.fromCharArray

instance prettyString :: Pretty String where
  pretty = vsep <<< map unsafeTextWithoutNewlines <<< S.split (S.Pattern "\n")
  prettyList l = prettyListDefault l

-- | Smart constructors

align :: ∀ ann. Doc ann -> Doc ann
align d = column (\k -> nesting (\i -> nest (k - i) d)) -- nesting might be negative!

cat :: ∀ ann. Array (Doc ann) -> Doc ann
cat = group <<< vcat

char :: ∀ ann. Char -> Doc ann
char c = pretty c

changesUponFlattening :: ∀ ann. Doc ann -> Maybe (Doc ann)
changesUponFlattening = case _ of
    FlatAlt _ y     -> Just (flatten y)
    Line            -> Just Fail
    Union x _       -> changesUponFlattening x <|> Just x
    Nest i x        -> map (Nest i) (changesUponFlattening x)
    Annotated ann x -> map (Annotated ann) (changesUponFlattening x)

    Column f        -> Just (Column (flatten <<< f))
    Nesting f       -> Just (Nesting (flatten <<< f))
    WithPageWidth f -> Just (WithPageWidth (flatten <<< f))

    Cat x y -> case Tuple (changesUponFlattening x) (changesUponFlattening y) of
        Tuple Nothing Nothing -> Nothing
        Tuple (Just x') Nothing -> Just (Cat x' y )
        Tuple Nothing (Just y') -> Just (Cat x  y')
        Tuple (Just x') (Just y') -> Just (Cat x' y')

    Empty  -> Nothing
    Char _ -> Nothing
    Text _ _ -> Nothing
    Fail   -> Nothing
  where
    -- Flatten, but don’t report whether anything changes.
    flatten :: Doc ann -> Doc ann
    flatten = case _ of
        FlatAlt _ y     -> flatten y
        Cat x y         -> Cat (flatten x) (flatten y)
        Nest i x        -> Nest i (flatten x)
        Line            -> Fail
        Union x _       -> flatten x
        Column f        -> Column (flatten <<< f)
        WithPageWidth f -> WithPageWidth (flatten <<< f)
        Nesting f       -> Nesting (flatten <<< f)
        Annotated ann x -> Annotated ann (flatten x)

        x@Fail   -> x
        x@Empty  -> x
        x@(Char _) -> x
        x@(Text _ _) -> x

column :: ∀ ann. (Int -> Doc ann) -> Doc ann
column = Column

concatWith :: ∀ ann t. Foldable t => (Doc ann -> Doc ann -> Doc ann) -> t (Doc ann) -> Doc ann
concatWith f ds = case uncons $ fromFoldable ds of
  Nothing -> mempty
  Just { head, tail } -> foldr f head tail

emptyDoc :: ∀ ann. Doc ann
emptyDoc = Empty

enclose
    :: ∀ ann. Doc ann -- ^ L
    -> Doc ann -- ^ R
    -> Doc ann -- ^ x
    -> Doc ann -- ^ LxR
enclose l r x = l <> x <> r

encloseSep
    :: ∀ ann. Doc ann   -- ^ left delimiter
    -> Doc ann   -- ^ right delimiter
    -> Doc ann   -- ^ separator
    -> Array (Doc ann) -- ^ input documents
    -> Doc ann
encloseSep l r s ds = case ds of
    []  -> l <> r
    [d] -> l <> d <> r
    _   -> align (cat (fromFoldable (LazyList.zipWith (<>) (l LazyList.: repeat s) (LazyList.fromFoldable ds))) <> r)

fillSep :: ∀ ann. Array (Doc ann) -> Doc ann
fillSep = concatWith (\x y -> x <> softline <> y)

flatAlt
    :: ∀ ann. Doc ann -- ^ Default
    -> Doc ann -- ^ Fallback when 'group'ed
    -> Doc ann
flatAlt = FlatAlt

group :: ∀ ann. Doc ann -> Doc ann
-- See note [Group: special flattening]
group x = case changesUponFlattening x of
    Nothing -> x
    Just x' -> Union x' x

hcat :: ∀ ann. Array (Doc ann) -> Doc ann
hcat = concatWith (<>)

line :: ∀ ann. Doc ann
line = FlatAlt Line space

line' :: ∀ ann. Doc ann
line' = FlatAlt Line mempty

list :: ∀ ann. Array (Doc ann) -> Doc ann
list = group <<< encloseSep (flatAlt (pretty "[ ") (pretty "[")) (flatAlt (pretty " ]") (pretty "]")) (pretty ", ")

lparen :: ∀ ann. Doc ann
lparen = pretty "("

nest
    :: ∀ ann. Int -- ^ Change of nesting level
    -> Doc ann
    -> Doc ann
nest 0 x = x -- Optimization
nest i x = Nest i x

nesting :: ∀ ann. (Int -> Doc ann) -> Doc ann
nesting = Nesting

parens :: ∀ ann. Doc ann -> Doc ann
parens = enclose lparen rparen

rparen :: ∀ ann. Doc ann
rparen = pretty ")"

softline :: ∀ ann. Doc ann
softline = group line

space :: ∀ ann. Doc ann
space = pretty " "

text :: ∀ ann. String -> Doc ann
text t = pretty t

unsafeTextWithoutNewlines :: ∀ ann. String -> Doc ann
unsafeTextWithoutNewlines text0 = case S.uncons text0 of
    Nothing -> Empty
    Just { head : t, tail : ext } -> if S.null ext then Char t else Text (S.length text0) text0

vcat :: ∀ ann. Array (Doc ann) -> Doc ann
vcat = concatWith (\x y -> x <> line' <> y)

vsep :: ∀ ann. Array (Doc ann) -> Doc ann
vsep = concatWith (\x y -> x <> line <> y)

showChar :: Char -> String -> String
showChar c s = S.singleton c <> s

showString :: String -> String -> String
showString s1 s2 = s1 <> s2

renderShowS :: ∀ ann. SimpleDocStream ann -> String -> String
renderShowS = case _ of
  SFail        -> const "PANIC: Uncaught Fail"
  SEmpty       -> id
  SChar c x    -> showChar c <<< renderShowS x
  SText _l t x -> showString t <<< renderShowS x
  SLine i x    -> showString (S.fromCharArray ('\n' : replicate i ' ')) <<< renderShowS x
  SAnnPush _ x -> renderShowS x
  SAnnPop x    -> renderShowS x

renderCompact :: ∀ ann. Doc ann -> SimpleDocStream ann
renderCompact = layoutCompact

renderPretty :: ∀ ann. Number -> Int -> Doc ann -> SimpleDocStream ann
renderPretty x s = layoutPretty (LayoutOptions { layoutPageWidth : AvailablePerLine s x })

display :: ∀ ann. SimpleDocStream ann -> String
display s = renderShowS s ""
