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

module PrettyPrint.PrettyPrintWL where

import Prelude
import Data.List as List
import Control.Apply (lift2)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array (replicate, uncons, (:))
import Data.Foldable (class Foldable, foldl)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.String (fromCharArray, length, singleton)

data Doc a
  = Empty
  | Char Char
  | Text Int String
  | Line
  | FlatAlt (Doc a) (Doc a)
  | Cat (Doc a) (Doc a)
  | Nest Int (Doc a)
  | Union (Doc a) (Doc a)
  | Annotate a (Doc a)
  | Column  (Int -> Doc a)
  | Nesting (Int -> Doc a)
  | Columns (Maybe Int -> Doc a)
  | Ribbon  (Maybe Int -> Doc a)

derive instance functorDoc :: Functor Doc

instance semigroupDoc :: Semigroup (Doc a) where
  append = Cat

instance monoidDoc :: Monoid (Doc a) where
  mempty = Empty

data Docs a e
  = Nil
  | Cons Int (Doc a) (Docs a e)

data SimpleDoc a
  = SEmpty
  | SChar Char (SimpleDoc a)
  | SText Int String (SimpleDoc a)
  | SLine Int (SimpleDoc a)
  | SPushAnn a (SimpleDoc a)
  | SPopAnn  a (SimpleDoc a)

char :: ∀ a. Char -> Doc a
char '\n' = line
char c = Char c

displayDecoratedA ::
  ∀ a f o.
  Applicative f => Monoid o =>
  (a -> f o) -> (a -> f o) -> (String -> f o) -> SimpleDoc a -> f o
displayDecoratedA push pop str = go
 where
  go SEmpty         = pure mempty
  go (SChar c x)    = lift2 append (str (singleton c)) (go x)
  go (SText _ s x)  = lift2 append (str s) (go x)
  go (SLine i x)    = lift2 append (str (singleton '\n' <> spaces i)) (go x)
  go (SPushAnn a x) = lift2 append (push a) (go x)
  go (SPopAnn  a x) = lift2 append (pop a) (go x)

displayS :: ∀ a. SimpleDoc a -> String -> String
displayS = displayDecoratedA ci ci (<>)
  where ci = const id

display :: ∀ a. SimpleDoc a -> String
display = flip displayS ""

enclose :: ∀ a. Doc a -> Doc a -> Doc a -> Doc a
enclose l r x = l <> x <> r

fillSep :: ∀ f a. Foldable f => f (Doc a) -> Doc a
fillSep = mfoldl1 (</>)
  where
    mfoldl1 :: (Doc a -> Doc a -> Doc a) -> f (Doc a) -> Doc a
    mfoldl1 f xs =
      -- not efficient but heh
      case List.fromFoldable xs of
        List.Nil   -> mempty
        h List.: t -> foldl f h t

flatten :: ∀ a. Doc a -> Doc a
flatten (FlatAlt _ y)  = y
flatten (Cat x y)      = Cat (flatten x) (flatten y)
flatten (Nest i x)     = Nest i (flatten x)
flatten (Union x _)    = flatten x
flatten (Annotate a x) = Annotate a (flatten x)
flatten (Column f)     = Column  (flatten <<< f)
flatten (Nesting f)    = Nesting (flatten <<< f)
flatten (Columns f)    = Columns (flatten <<< f)
flatten (Ribbon f)     = Ribbon  (flatten <<< f)
flatten a@Empty        = a
flatten a@(Char _)     = a
flatten a@(Text _ _)   = a
flatten a@Line         = a

group :: ∀ a. Doc a -> Doc a
group x = Union (flatten x) x

line :: ∀ a. Doc a
line = FlatAlt Line space
  where
    char' :: Char -> Doc a
    char' '\n' = line
    char' c = Char c
    space :: Doc a
    space = char' ' '

lparen :: ∀ a. Doc a
lparen = char '('

nest :: ∀ a. Int -> Doc a -> Doc a
nest = Nest

nicest1 :: ∀ a. Int -> Int -> Int -> Int -> SimpleDoc a -> SimpleDoc a -> SimpleDoc a
nicest1 n k p r x' y = if fits (min n k) wid x' then x' else y
  where
    wid = min (p - k) (r - k + n)
    fits _ w _        | w < 0 = false
    fits _ _ SEmpty           = true
    fits m w (SChar _ x)      = fits m (w - 1) x
    fits m w (SText l _ x)    = fits m (w - l) x
    fits _ _ (SLine _ _)      = true
    fits m w (SPushAnn _ x)   = fits m w x
    fits m w (SPopAnn  _ x)   = fits m w x

parens :: ∀ a. Doc a -> Doc a
parens = enclose lparen rparen

renderCompact :: ∀ a. Doc a -> SimpleDoc a
renderCompact x
    = scan SEmpty 0 [x]
    where
      scan z k l0 = case uncons l0 of
        Nothing -> z
        Just { head : d, tail : ds } ->
          case d of
            Empty         -> scan z k ds
            Char c        -> let k' = k+1 in SChar c (scan z k' ds)
            Text l s      -> let k' = k+l in SText l s (scan z k' ds)
            Annotate a d' -> SPushAnn a (scan (SPopAnn a $ scan z k ds) k [d'])
            Line          -> SLine 0 (scan z 0 ds)
            FlatAlt y _   -> scan z k (y:ds)
            Cat y z'      -> scan z k (y:z':ds)
            Nest _ y      -> scan z k (y:ds)
            Union _ y     -> scan z k (y:ds)
            Column f      -> scan z k (f k:ds)
            Nesting f     -> scan z k (f 0:ds)
            Columns f     -> scan z k (f Nothing:ds)
            Ribbon  f     -> scan z k (f Nothing:ds)

renderFits ::
  ∀ a.
  (Int -> Int -> Int -> Int -> SimpleDoc a -> SimpleDoc a -> SimpleDoc a) ->
  Number -> Int -> Doc a -> SimpleDoc a
renderFits nicest rfrac w x
    = tailRec best { n : 0, k : 0, z : (\_ _ -> SEmpty), d : (Cons 0 x Nil), finally : id }
    where
      -- r :: the ribbon width in characters
      r  = max 0 (min w (round (toNumber w * rfrac)))

      -- best :: n = indentation of current line
      --         k = current column
      --        (ie. (k >= n) && (k - n == count of inserted characters)
      best { n, k, z, d : Nil, finally } = Done $ finally $ z n k
      best { n, k, z, d : Cons i d ds, finally } =
        case d of
          Empty         -> Loop { n, k, z, d : ds, finally }
          Char c        -> Loop { n, k : k + 1, z, d : ds, finally : finally <<< SChar c }
          Text l s      -> Loop { n, k : k + l, z, d : ds, finally : finally <<< SText l s }
          Line          -> Loop { n : i, k : i, z, d : ds, finally : finally <<< SLine i }
          FlatAlt l _   -> Loop { n, k, z, d : Cons i l ds, finally }
          Cat x' y      -> Loop { n, k, z, d : Cons i x' (Cons i y ds), finally }
          Nest j x'     -> Loop { n, k, z, d : Cons (i + j) x' ds, finally }
          Annotate a d' -> let z' n' k' = tailRec best { n : n', k : k', z, d : ds, finally : SPopAnn a }
                           in Loop { n, k, z : z', d : Cons i d' Nil, finally : finally <<< SPushAnn a }
          Union p q     -> Done $ finally $ nicest n k w r
                           (tailRec best { n, k, z, d : Cons i p ds, finally : id })
                           (tailRec best { n, k, z, d : Cons i q ds, finally : id })
          Column f      -> Loop { n, k, z, d : Cons i (f k) ds, finally }
          Nesting f     -> Loop { n, k, z, d : Cons i (f i) ds, finally }
          Columns f     -> Loop { n, k, z, d : Cons i (f $ Just w) ds, finally }
          Ribbon f      -> Loop { n, k, z, d : Cons i (f $ Just r) ds, finally }

renderPretty :: ∀ a. Number -> Int -> Doc a -> SimpleDoc a
renderPretty = renderFits nicest1

rparen :: ∀ a. Doc a
rparen = char ')'

softconcat :: ∀ a. Doc a -> Doc a -> Doc a
softconcat x y = x <> softline <> y
infixr 5 softconcat as </>

softline :: ∀ a. Doc a
softline = group line

spaces :: Int -> String
spaces n | n <= 0    = ""
         | otherwise = fromCharArray $ replicate n ' '

text :: ∀ a. String -> Doc a
text "" = Empty
text s  = Text (length s) s
