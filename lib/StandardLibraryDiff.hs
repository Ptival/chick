--{-# LANGUAGE DataKinds #-}
--{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

--{-# LANGUAGE ScopedTypeVariables #-}

module StandardLibraryDiff
  ( δBoolToNat
  , δListToVec
  , δNatToList
  ) where

import Text.Printf

import qualified Diff.Atom as DA
import qualified Diff.Constructor as DC
import qualified Diff.Inductive as DI
import qualified Diff.List as DL
import qualified Diff.Pair as DP
import qualified Diff.Term as DT
import           Parsing
import           Term.Term
import qualified Term.Raw as Raw

-- do not use `unsafeParseRaw` anywhere else!
unsafeParseRaw :: String -> Raw.Term Variable
unsafeParseRaw s =
  case parseMaybeTerm s of
    Nothing -> error $ printf "unsafeParseRaw: could not parse %s" s
    Just t  -> t

δBoolToNat :: DI.Diff Raw.Raw
δBoolToNat = DI.Modify δn δps δis δcs

  where

    δn = DA.Replace "ℕ"
    δps = DL.Same
    δis = DL.Same
    -- for sake of testing, let's permute
    δcs = DL.Permute [1, 0] $ DL.Modify δfalse $ DL.Modify δtrue $ DL.Same

    δfalse = DC.Modify (DA.Replace "zero") DL.Same DL.Same
    δtrue = DC.Modify (DA.Replace "succ") δsuccPs DL.Same

    δsuccPs = DL.Insert ("n", "ℕ") DL.Same

δNatToList :: DI.Diff Raw.Raw
δNatToList = DI.Modify δn δps δis δcs

  where

    δn = DA.Replace "List"
    δps = DL.Insert ("A", Type) DL.Same
    δis = DL.Same
    δcs = DL.Modify δzeroToNil $ DL.Modify δsuccToCons $ DL.Same

    δzeroToNil = DC.Modify (DA.Replace "nil") DL.Same DL.Same
    δsuccToCons = DC.Modify (DA.Replace "cons") δsuccToConsPs DL.Same

    δsuccToConsPs =
      DL.Insert ("x", "A")
      $ DL.Modify (DP.Modify (DA.Replace "xs") (DT.Replace (unsafeParseRaw "List A")))
      $ DL.Same

δListToVec :: DI.Diff Raw.Raw
δListToVec = DI.Modify δn δps δis δcs

  where

    δn = DA.Replace "Vec"
    δps = DL.Same
    δis = DL.Insert ("size", "ℕ") DL.Same
    δcs = DL.Modify δnil $ DL.Modify δcons $ DL.Same

    δnil = DC.Modify (DA.Replace "vnil") DL.Same (DL.Insert "zero" DL.Same)
    δcons = DC.Modify (DA.Replace "vcons") δconsPs δconsIs

    δconsPs =
      DL.Keep
      $ DL.Insert ("n", "ℕ")
      $ DL.Modify
      (DP.Modify DA.Same
        (DT.InsApp ()
          (DT.CpyApp (DT.Replace "Vec") DT.Same)
          (DT.Replace "n")
        )
      )
      $ DL.Same
    δconsIs = DL.Insert (App () (Var Nothing "succ") (Var Nothing "n")) DL.Same
