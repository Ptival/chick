{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Diff.ListFold
  ( ΔListFold(..)
  , δListFoldConcatMap
  , δListFoldConcatMap'
  , δListFoldMkAppTerms
  , δListFoldMkAppVariables
  , δListFoldMkPiBinders
  , δListFoldMkPiGeneric
  , δListFoldMkPiGenericMaybe
  , δListFoldMkPiVariables
  ) where

import qualified Diff.Atom as ΔA
import qualified Diff.Binder as ΔB
import qualified Diff.List as ΔL
import qualified Diff.Pair as Δ2
import qualified Diff.Term as ΔT
import qualified Diff.Triple as Δ3
import           Diff.Utils
import           PrettyPrinting.PrettyPrintable
import           Term.Term
import           Utils

data ΔListFold τ δτ a = ΔListFold
  { onInsert  ::     τ ->      [τ] -> a -> a
  , onKeep    ::          τ -> [τ] -> a -> a
  , onModify  ::    δτ -> τ -> [τ] -> a -> a
  , onPermute :: [Int] ->      [τ] -> a -> a
  , onRemove  ::          τ -> [τ] -> a -> a
  , onReplace ::   [τ] ->      [τ] -> a -> a
  , onSame    ::               [τ] -> a -> a
  }

-- Input:  [a, b, c]
-- Output: [0, 1, 2]
indices :: [a] -> [Int]
indices = mapWithIndex (\ _ i -> i)

-- Input:  [   [a, b],    [c],    [d, e, f],   [g]]
-- Output: [0,         2,      3,            6    ]
countElementsBeforeGroup :: [[a]] -> [Int]
countElementsBeforeGroup = fst . foldl count ([], 0)
  where count (cs, c) l = (cs ++ [c], c + length l)

-- Input:  [[a, b], [c], [d, e, f]]
-- Output: [[0, 1], [2], [3, 4, 5]]
indicesListList :: [[a]] -> [[Int]]
indicesListList l =
  zipWith offsetBy (countElementsBeforeGroup l) (map indices l)
  where
    offsetBy offset = map ((+) offset)

-- say:
-- [        a,            b,    c ] = l
-- [ [a0, a1], [b0, b1, b2], [c0] ] = map f l
--     0   1     2   3   4     5   is their index
-- and p is the permutation [2, 1, 0]:
-- [    a,            b,        c ] = l
-- [    c,            b,        a ] = permute p l
-- [ [c0], [b0, b1, b2], [a0, a1] ] = map f $ permute p l
--     5     2   3   4     0   1   is their index in (map f l), our result
-- now:
-- [ [a0, a1], [b0, b1, b2], [c0] ] = map f l
-- [ [ 0,  1], [ 2,  3,  4], [ 5] ] = indicesListList $ map f l
-- [ [5], [2, 3, 4], [0, 1] ] = permute p . indicesListList $ map f l

-- How neat is that!? That's pretty neat.

δpermute :: [a] -> (a -> [b]) -> [Int] -> [Int]
δpermute l f p = concat . permute p . indicesListList $ map f l

δListFoldConcatMap ::
  (a -> [b]) ->
  (δa -> a -> Maybe a) ->
  ΔListFold a δa (Maybe (ΔL.Diff b δb))
δListFoldConcatMap f patchA = ΔListFold
  { onInsert, onKeep, onModify, onPermute, onRemove, onReplace, onSame }
  where
    onInsert  a   _ acc = foldr (\ b acc -> ΔL.Insert b <$> acc) acc (f a)
    onKeep    a   _ acc = foldr (\ _ acc -> ΔL.Keep     <$> acc) acc (f a)
    onModify δa a _ acc =
      case patchA δa a of
      Nothing -> Nothing
      Just a' -> foldrWith   (\ _ acc -> ΔL.Remove   <$> acc) (f a)
                 $ foldrWith (\ b acc -> ΔL.Insert b <$> acc) (f a')
                 $ acc
    onPermute p l acc = ΔL.Permute (δpermute (take (length p) l) f p) <$> acc
    onRemove  _ _ _acc = error "TODO: δListFoldConcatMap onRemove"
    onReplace _ _ _acc = error "TODO: δListFoldConcatMap onReplace"
    onSame      l acc = ΔL.nKeeps (length l) <$> acc

δListFoldConcatMap' ::
  (a -> [b]) ->
  (δa -> a -> Maybe (ΔL.Diff b δb -> ΔL.Diff b δb)) ->
  ΔListFold a δa (Maybe (ΔL.Diff b δb))
δListFoldConcatMap' f patchBs = ΔListFold
  { onInsert, onKeep, onModify, onPermute, onRemove, onReplace, onSame }
  where
    onInsert  a   _ acc = foldr (\ b acc -> ΔL.Insert b <$> acc) acc (f a)
    onKeep    a   _ acc = foldr (\ _ acc -> ΔL.Keep     <$> acc) acc (f a)
    onModify δa a _ acc =
      case patchBs δa a of
      Nothing -> Nothing
      Just δb -> δb <$> acc
    onPermute p l acc = ΔL.Permute (δpermute (take (length p) l) f p) <$> acc
    onRemove  _ _ _acc = error "TODO: δListFoldConcatMap' onRemove"
    onReplace _ _ _acc = error "TODO: δListFoldConcatMap' onReplace"
    onSame      l acc = ΔL.nKeeps (length l) <$> acc

δListFoldMkAppTerms ::
  ΔListFold (α, TermX α Variable) (Δ2.Diff (ΔA.Diff α) (ΔT.Diff α)) (ΔT.Diff α)
δListFoldMkAppTerms = ΔListFold
  { onInsert, onKeep, onModify, onPermute, onRemove, onReplace, onSame }
  where
    onInsert (α, t)   _  b = ΔT.InsApp α b (ΔT.Replace t)
    onKeep          _ _  b = ΔT.CpyApp b ΔT.Same
    onModify     δt _ _  b = ΔT.CpyApp b (Δ2.δsnd ΔT.Same δt)
    onPermute     _   _  _ = error "TODO: δListFoldMkAppTerms onPermute"
    onRemove        _ _  _ = error "TODO: δListFoldMkAppTerms onRemove"
    onReplace     _   _  _ = error "TODO: δListFoldMkAppTerms onReplace"
    onSame            l  b = ΔT.nCpyApps (length l) b

{-
This dictionary explains how a list gets modified when trying to build a nested
application `f a b c d e` out of a list `[a, b, c, d, e]`, when the list gets
modified.  Not sure why I did not build it as a diff-transformer rather than
fully-instantiated diffs...
-}
δListFoldMkAppVariables ::
  ΔListFold
  (α, Variable, TermX α Variable)
  (Δ3.Diff (ΔA.Diff α) (ΔA.Diff Variable) (ΔT.Diff α)) (ΔT.Diff α)
δListFoldMkAppVariables = ΔListFold
  { onInsert, onKeep, onModify, onPermute, onRemove, onReplace, onSame }
  where
    onInsert (α, v, _)   _ b = ΔT.InsApp α b (ΔT.Replace (Var Nothing v))
    onKeep _ _ b = ΔT.CpyApp b ΔT.Same
    onModify δ _ _ b =
      case δ of
      Δ3.Same -> ΔT.CpyApp b ΔT.Same
      Δ3.Modify _ δv _ ->
        case δv of
        ΔA.Same -> ΔT.CpyApp b ΔT.Same
        ΔA.Replace r -> ΔT.CpyApp b (ΔT.Replace (Var Nothing r))
    onPermute _ _ _ = error "TODO: δListFoldMkAppVariables onPermute"
    onRemove _ _ _ = ΔT.RemoveApp ΔT.Same
    onReplace _ _ _ = error "TODO: δListFoldMkAppVariables onReplace"
    onSame l b = ΔT.nCpyApps (length l) b

δListFoldMkPiGeneric ::
  PrettyPrintable τ =>
  (τ -> (α, ΔT.Diff α, Binder Variable)) ->
  (τ -> δτ -> (ΔT.Diff α, ΔA.Diff (Binder Variable))) ->
  ΔListFold τ δτ (ΔT.Diff α)
δListFoldMkPiGeneric pi δpi = ΔListFold
  { onInsert, onKeep, onModify, onPermute, onRemove, onReplace, onSame }
  where
    insert e δ = ΔT.InsPi α δτ b δ where (α, δτ, b) = pi e
    onInsert e _    δ = insert e δ
    onKeep _ _      δ = ΔT.CpyPi   ΔT.Same ΔA.Same δ
    onModify δe e _ δ = ΔT.CpyPi   δτ      δb      δ where (δτ, δb) = δpi e δe
    onPermute _p   _δ = error "TODO: δListFoldMkPiGeneric onPermute"
    onRemove _ _    δ = ΔT.RemovePi δ
    onReplace l l'  δ =
      foldrWith   insert                   l'
      $ foldrWith (\ _ δ -> ΔT.RemovePi δ) l
      $ δ
    onSame l        δ = ΔT.nCpyPis (length l) δ

δListFoldMkPiGenericMaybe ::
  (τ -> (α, ΔT.Diff α, Binder Variable)) ->
  (τ -> δτ -> Maybe (ΔT.Diff α, ΔA.Diff (Binder Variable))) ->
  ΔListFold τ δτ (Maybe (ΔT.Diff α))
δListFoldMkPiGenericMaybe pi δpi = ΔListFold
  { onInsert, onKeep, onModify, onPermute, onRemove, onReplace, onSame }
  where
    onInsert   e _   δ = ΔT.InsPi α   δτ          b       <$> δ
      where (α, δτ, b) =  pi  e
    onKeep       _ _ δ = ΔT.CpyPi     ΔT.Same     ΔA.Same <$> δ
    onModify  δe e _ δ = ΔT.CpyPi <$> δτ      <*> δb      <*> δ
      where (δτ, δb) = unzipMaybe (δpi e δe)
    onPermute    p _ δ = ΔT.PermutPis p <$> δ
    onRemove  _b    = error "TODO: δListFoldMkPiGenericMaybe onRemove"
    onReplace _l _δ = error "TODO: δListFoldMkPiGenericMaybe onReplace"
    onSame     l  δ = ΔT.nCpyPis (length l) <$> δ

δListFoldMkPiBinders ::
  PrettyPrintable α =>
  ΔListFold
  (α, Binder Variable, TermX α Variable)
  (Δ3.Diff (ΔA.Diff α) (ΔA.Diff (Binder Variable)) (ΔT.Diff α)) (ΔT.Diff α)
δListFoldMkPiBinders = δListFoldMkPiGeneric pi δpi
  where
    pi  (α, b, i) = (α, ΔT.Replace i, b)
    δpi _ δe = case δe of
      Δ3.Same         -> (ΔT.Same, ΔA.Same)
      Δ3.Modify _ δb δi -> (δi, δb)

δListFoldMkPiVariables ::
  PrettyPrintable α =>
  ΔListFold
  (α, Variable, TermX α Variable)
  (Δ3.Diff (ΔA.Diff α) (ΔA.Diff Variable) (ΔT.Diff α)) (ΔT.Diff α)
δListFoldMkPiVariables = δListFoldMkPiGeneric pi δpi
  where
    pi  (α, v, i) = (α, ΔT.Replace i, Binder (Just v))
    δpi _ δe = case δe of
      Δ3.Same         -> (ΔT.Same, ΔA.Same)
      Δ3.Modify _ δv δi -> (δi, ΔB.fromΔVariable δv)
