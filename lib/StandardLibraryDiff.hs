--{-# LANGUAGE DataKinds #-}
--{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE ScopedTypeVariables #-}

module StandardLibraryDiff
  ( δListToVec
  )where

import qualified Diff.Atom as DA
import qualified Diff.Constructor as DC
import qualified Diff.Inductive as DI
import qualified Diff.List as DL
import qualified Diff.Pair as DP
import qualified Diff.Term as DT
import           Term.Term
import qualified Term.Raw as Raw

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
