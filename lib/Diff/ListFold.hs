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

import qualified Diff.Atom as DA
import qualified Diff.Binder as DB
import qualified Diff.List as DL
import qualified Diff.Pair as D2
import qualified Diff.Term as DT
import qualified Diff.Triple as D3
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
  ΔListFold a δa (Maybe (DL.Diff b δb))
δListFoldConcatMap f patchA = ΔListFold
  { onInsert, onKeep, onModify, onPermute, onRemove, onReplace, onSame }
  where
    onInsert  a   _ acc = foldr (\ b acc -> DL.Insert b <$> acc) acc (f a)
    onKeep    a   _ acc = foldr (\ _ acc -> DL.Keep     <$> acc) acc (f a)
    onModify δa a _ acc =
      case patchA δa a of
      Nothing -> Nothing
      Just a' -> foldrWith   (\ _ acc -> DL.Remove   <$> acc) (f a)
                 $ foldrWith (\ b acc -> DL.Insert b <$> acc) (f a')
                 $ acc
    onPermute p l acc = DL.Permute (δpermute (take (length p) l) f p) <$> acc
    onRemove  _ _ _acc = error "TODO: δListFoldConcatMap onRemove"
    onReplace _ _ _acc = error "TODO: δListFoldConcatMap onReplace"
    onSame      l acc = DL.nKeeps (length l) <$> acc

δListFoldConcatMap' ::
  (a -> [b]) ->
  (δa -> a -> Maybe (DL.Diff b δb -> DL.Diff b δb)) ->
  ΔListFold a δa (Maybe (DL.Diff b δb))
δListFoldConcatMap' f patchBs = ΔListFold
  { onInsert, onKeep, onModify, onPermute, onRemove, onReplace, onSame }
  where
    onInsert  a   _ acc = foldr (\ b acc -> DL.Insert b <$> acc) acc (f a)
    onKeep    a   _ acc = foldr (\ _ acc -> DL.Keep     <$> acc) acc (f a)
    onModify δa a _ acc =
      case patchBs δa a of
      Nothing -> Nothing
      Just δb -> δb <$> acc
    onPermute p l acc = DL.Permute (δpermute (take (length p) l) f p) <$> acc
    onRemove  _ _ _acc = error "TODO: δListFoldConcatMap' onRemove"
    onReplace _ _ _acc = error "TODO: δListFoldConcatMap' onReplace"
    onSame      l acc = DL.nKeeps (length l) <$> acc

δListFoldMkAppTerms ::
  ΔListFold (α, TermX α Variable) (D2.Diff (DA.Diff α) (DT.Diff α)) (DT.Diff α)
δListFoldMkAppTerms = ΔListFold
  { onInsert, onKeep, onModify, onPermute, onRemove, onReplace, onSame }
  where
    onInsert (α, t)   _  b = DT.InsApp α b (DT.Replace t)
    onKeep          _ _  b = DT.CpyApp b DT.Same
    onModify     δt _ _  b = DT.CpyApp b (D2.δsnd DT.Same δt)
    onPermute     _   _  _ = error "TODO: δListFoldMkAppTerms onPermute"
    onRemove        _ _  _ = error "TODO: δListFoldMkAppTerms onRemove"
    onReplace     _   _  _ = error "TODO: δListFoldMkAppTerms onReplace"
    onSame            l  b = DT.nCpyApps (length l) b

δListFoldMkAppVariables ::
  ΔListFold
  (α, Variable, TermX α Variable)
  (D3.Diff (DA.Diff α) (DA.Diff Variable) (DT.Diff α)) (DT.Diff α)
δListFoldMkAppVariables = ΔListFold
  { onInsert, onKeep, onModify, onPermute, onRemove, onReplace, onSame }
  where
    onInsert (α, v, _)   _ b = DT.InsApp α b (DT.Replace (Var Nothing v))
    onKeep          _ _ b = DT.CpyApp b DT.Same
    onModify      δ _ _ b =
      case δ of
      D3.Same -> DT.CpyApp b DT.Same
      D3.Modify _ δv _ ->
        case δv of
        DA.Same -> DT.CpyApp b DT.Same
        DA.Replace r -> DT.CpyApp b (DT.Replace (Var Nothing r))
    onPermute _ _ _ = error "TODO: δListFoldMkAppVariables onPermute"
    onRemove  _ _ _ = error "TODO: δListFoldMkAppVariables onRemove"
    onReplace _ _ _ = error "TODO: δListFoldMkAppVariables onReplace"
    onSame      l b = DT.nCpyApps (length l) b

δListFoldMkPiGeneric ::
  PrettyPrintable τ =>
  (τ -> (α, DT.Diff α, Binder Variable)) ->
  (τ -> δτ -> (DT.Diff α, DA.Diff (Binder Variable))) ->
  ΔListFold τ δτ (DT.Diff α)
δListFoldMkPiGeneric pi δpi = ΔListFold
  { onInsert, onKeep, onModify, onPermute, onRemove, onReplace, onSame }
  where
    insert e δ = DT.InsPi α δτ b δ where (α, δτ, b) = pi e
    onInsert e _    δ = insert e δ
    onKeep _ _      δ = DT.CpyPi   DT.Same DA.Same δ
    onModify δe e _ δ = DT.CpyPi   δτ      δb      δ where (δτ, δb) = δpi e δe
    onPermute _p   _δ = error "TODO: δListFoldMkPiGeneric onPermute"
    onRemove _ _    δ = DT.RemovePi δ
    onReplace l l'  δ =
      foldrWith   insert                   l'
      $ foldrWith (\ _ δ -> DT.RemovePi δ) l
      $ δ
    onSame l        δ = DT.nCpyPis (length l) δ

δListFoldMkPiGenericMaybe ::
  (τ -> (α, DT.Diff α, Binder Variable)) ->
  (τ -> δτ -> Maybe (DT.Diff α, DA.Diff (Binder Variable))) ->
  ΔListFold τ δτ (Maybe (DT.Diff α))
δListFoldMkPiGenericMaybe pi δpi = ΔListFold
  { onInsert, onKeep, onModify, onPermute, onRemove, onReplace, onSame }
  where
    onInsert   e _   δ = DT.InsPi α   δτ          b       <$> δ
      where (α, δτ, b) =  pi  e
    onKeep       _ _ δ = DT.CpyPi     DT.Same     DA.Same <$> δ
    onModify  δe e _ δ = DT.CpyPi <$> δτ      <*> δb      <*> δ
      where (δτ, δb) = unzipMaybe (δpi e δe)
    onPermute    p _ δ = DT.PermutPis p <$> δ
    onRemove  _b    = error "TODO: δListFoldMkPiGenericMaybe onRemove"
    onReplace _l _δ = error "TODO: δListFoldMkPiGenericMaybe onReplace"
    onSame     l  δ = DT.nCpyPis (length l) <$> δ

δListFoldMkPiBinders ::
  PrettyPrintable α =>
  ΔListFold
  (α, Binder Variable, TermX α Variable)
  (D3.Diff (DA.Diff α) (DA.Diff (Binder Variable)) (DT.Diff α)) (DT.Diff α)
δListFoldMkPiBinders = δListFoldMkPiGeneric pi δpi
  where
    pi  (α, b, i) = (α, DT.Replace i, b)
    δpi _ δe = case δe of
      D3.Same         -> (DT.Same, DA.Same)
      D3.Modify _ δb δi -> (δi, δb)

δListFoldMkPiVariables ::
  PrettyPrintable α =>
  ΔListFold
  (α, Variable, TermX α Variable)
  (D3.Diff (DA.Diff α) (DA.Diff Variable) (DT.Diff α)) (DT.Diff α)
δListFoldMkPiVariables = δListFoldMkPiGeneric pi δpi
  where
    pi  (α, v, i) = (α, DT.Replace i, Binder (Just v))
    δpi _ δe = case δe of
      D3.Same         -> (DT.Same, DA.Same)
      D3.Modify _ δv δi -> (δi, DB.fromΔVariable δv)
