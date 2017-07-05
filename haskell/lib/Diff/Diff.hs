{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}

module Diff.Diff where

import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.State
import           Data.Foldable

import qualified Diff.Atom as DA
import qualified Diff.LocalContext as DLC
import qualified Diff.Term as DT
import           Diff.Utils
import           PrettyPrinting.PrettyPrintableUnannotated
import           Term.Binder
import           Term.Term
import qualified Term.Raw as Raw
import           Term.Variable
import           StandardLibrary

-- | `diffProof t (τ, d)` assumes `t` is a term whose type is `τ` and `d` is a diff describing
-- | how `τ` changed.  It attempts to build a patch `p` s.t. `patch t p` has type `patch τ d`.
diffProof ::
  ( Member (Exc String) r
  , Member (State (DLC.Diff Raw.Raw)) r
  ) =>
  Raw.Term Variable -> (Raw.Type Variable, DT.Diff Raw.Raw) -> Eff r (DT.Diff Raw.Raw)
diffProof t (τ, d) = case d of
  DT.Same                -> return DT.Same
  DT.Change _            -> return $ DT.Change $ Hole ()
  DT.CpyLam _ _          -> throwExc "diffProof: CpyLam"
  DT.InsLam _ _          -> throwExc "diffProof: InsLam"
  DT.PermutLams _ _      -> throwExc "diffProof: PermutLams"
  DT.CpyPi d1 DA.Same d2 ->
    case (t, τ) of
      (Lam _ bt, Pi _ _ bτ2) ->
        CpyLam DA.Same <$> diffProof (snd $ unscopeTerm bt) (snd $ unscopeTerm bτ2, d2)
      _ -> throwExc "diffProof: CpyPi Same"
  DT.CpyPi _ _ _         -> throwExc "diffProof: CpyPi"
  DT.InsPi _ b d2        ->
    -- I think what we want here is:
    -- - find a b' binder like b that is free in t
    -- - InsLam b'
    -- - recursively diff by substituting b' for b
    InsLam b <$> diffProof t (τ, d2)
  DT.PermutPis p d1      -> do
    (pis, τrest) <- extractPis (length p) τ
    let pis' = permute p pis
    (lams, trest) <- extractLams (length p) t -- TODO: catchError and try something else?
    let lams' = permute p lams
    PermutLams p <$> diffProof (mkLams lams' trest) (mkPis pis' τrest, d1)

patchProof :: Raw.Term Variable -> (Raw.Type Variable, DT.Diff Raw.Raw) -> Either String (Raw.Term Variable)
patchProof t (τ, d) = run . runError $ diffProof t (τ, d) >>= patch t

data PatchBenchmark = PatchBenchmark
  { patchFromTerm :: Raw.Term Variable
  , patchFromType :: Raw.Type Variable
  , patchToType   :: Raw.Type Variable
  , patchDiff     :: DT.Diff Raw.Raw
  }

patchBenchmark :: [PatchBenchmark]
patchBenchmark =
  [ PatchBenchmark
    { patchFromTerm = unsafeParseRaw "λ b . b"
    , patchFromType = unsafeParseRaw "B → B"
    , patchToType   = unsafeParseRaw "A → B → B"
    , patchDiff     = InsPi (Change "A") (Binder Nothing) Same
    }
  , PatchBenchmark
    { patchFromTerm = unsafeParseRaw "λ b . b"
    , patchFromType = unsafeParseRaw "B → B"
    , patchToType   = unsafeParseRaw "B → A → B"
    , patchDiff     = CpyPi Same DA.Same (InsPi (Change "A") (Binder Nothing) Same)
    }
  , PatchBenchmark
    { patchFromTerm = unsafeParseRaw "λ f a . f a"
    , patchFromType = unsafeParseRaw "(A → B) → A → B"
    , patchToType   = unsafeParseRaw "A → (A → B) → B"
    , patchDiff     = PermutPis [1, 0] Same
    }
  , PatchBenchmark
    { patchFromTerm = unsafeParseRaw "λ f a . f a"
    , patchFromType = unsafeParseRaw "(A → C) → A → C"
    , patchToType   = unsafeParseRaw "(A → B → C) → A → B → C"
    , patchDiff     =
      CpyPi
      (CpyPi Same DA.Same (InsPi (Change "B") (Binder Nothing) Same))
      DA.Same
      (CpyPi Same DA.Same (InsPi (Change "B") (Binder Nothing) Same))
    }
  ]

benchmark :: IO ()
benchmark = do
  for_ patchBenchmark $ \ (PatchBenchmark fromTerm fromType toType diff) -> do
    putStrLn $ printf "Attempting to patch `%s` assumed to have type `%s` to type `%s`" (prettyStrU fromTerm) (prettyStrU fromType) (prettyStrU toType)
    let diffed = run . runError $ patch fromType diff
    if diffed == (Right toType :: Either String (Raw.Type Variable))
      then case patchProof fromTerm (fromType, diff) of
             Left  e -> putStrLn $ printf "Patching failed: %s" e
             Right r -> putStrLn $ printf "Patching succeeded: %s" (prettyStrU r)
      else do
        putStrLn $ printf "Sanity-checking diff %s failed" (show diff)
        case diffed of
          Left  e -> do
            putStrLn $ printf "Diffing failed: %s" e
          Right d -> do
            putStrLn $ printf "Original type: %s" (prettyStrU toType)
            putStrLn $ printf "Diffed   type: %s" (prettyStrU d)
