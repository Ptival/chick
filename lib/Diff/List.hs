{-# language DeriveFoldable #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}

module Diff.List
  ( Diff(..)
  , patch
  ) where

import Control.Monad.Freer
import Control.Monad.Freer.Exception
import Control.Monad.Freer.Trace
import Data.Bifunctor
import Text.Printf
import Text.PrettyPrint.Annotated.WL

import Diff.Utils
import PrettyPrinting.PrettyPrintable

data Diff t δt
  = Insert   t    (Diff t δt)
  | Keep          (Diff t δt)
  | Modify  δt    (Diff t δt)
  | Permute [Int] (Diff t δt)
  | Remove        (Diff t δt)
  | Replace [t]
  | Same

instance (Show t, Show δt) => Show (Diff t δt) where
  show = \case
    Insert t  δ -> printf "Insert %s\n%s"  (show t)  (show δ)
    Keep      δ -> printf "Keep\n%s"                 (show δ)
    Modify δt δ -> printf "Modify %s\n%s"  (show δt) (show δ)
    Permute p δ -> printf "Permute %s\n%s" (show p)  (show δ)
    Remove    δ -> printf "Remove\n%s"               (show δ)
    Replace l   -> printf "Replace %s"     (show l)
    Same        -> "Same"

instance (PrettyPrintable t, PrettyPrintable δt) => PrettyPrintable (Diff t δt) where
  prettyDoc = \case
    Insert t  δ -> fillSep [ text "Insert",  go t,          go δ ]
    Keep      δ -> fillSep [ text "Keep",                   go δ ]
    Modify δt δ -> fillSep [ text "Modify",  go δt,         go δ ]
    Permute p δ -> fillSep [ text "Permute", text (show p), go δ ]
    Remove    δ -> fillSep [ text "Remove",                 go δ ]
    Replace l   -> fillSep [ text "Replace", encloseSep lbracket rbracket comma (map prettyDoc l) ]
    Same        -> text "Same"
    where
      go x = fillSep [ lparen, prettyDoc x, rparen ]

instance Bifunctor Diff where
  bimap fa fb = \case
    Same         -> Same
    Insert  a  δ -> Insert  (fa a)      (bimap fa fb δ)
    Modify  b  δ -> Modify  (fb b)      (bimap fa fb δ)
    Permute p  δ -> Permute p           (bimap fa fb δ)
    Keep       δ -> Keep                (bimap fa fb δ)
    Remove     δ -> Remove              (bimap fa fb δ)
    Replace la   -> Replace (map fa la)

patch ::
  ( Member (Exc String) r
  , Member Trace r
  , PrettyPrintable a
  , PrettyPrintable δa
  ) =>
  (a -> δa -> Eff r a) -> [a] -> Diff a δa -> Eff r [a]
patch patchElem la δa =
--   trace (printf "Diff.List/patch(l: %s, δ: %s)"
--           (display . renderPrettyDefault . encloseSep lbracket rbracket comma $ map prettyDoc la)
--           (prettyStr δa)
--         ) >>
  go la δa
  where
    failWith = throwExc . printf "[Diff.List.patch] %s"
    go l = \case

      Same -> return l

      Insert e δ -> go l δ >>= return . (e :)

      Modify δe δ -> case l of
        h : t -> do
          ph <- patchElem h δe
          pt <- go t δ
          return $ ph : pt
        _     -> failWith "Modify, empty list"

      Permute p δ ->
        let ll = length l in
        if ll > length p || ll < maximum p
        then failWith "Permut, permutation exceeds list size"
        else go (permute p l) δ

      Keep δ -> case l of
        h : t -> go t δ >>= return . (h :)
        _     -> failWith "Keep, empty list"

      Remove δ -> go (tail l) δ

      Replace r -> return r
