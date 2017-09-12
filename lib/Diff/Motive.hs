{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Diff.Motive
  ( δmkMotiveType
  , δmkMotiveType'
  ) where

import qualified Diff.Atom as DA
import qualified Diff.Inductive as DI
import           Diff.ListFoldLeft
import           Diff.ListFoldRight
import qualified Diff.Term as DT
import           Inductive.Inductive
import           Term.Term

δonInductiveParameter ::
  α -> Φips α Variable -> DI.Δips α -> DT.Diff α -> DT.Diff α
δonInductiveParameter α = δListFoldLeft (δListFoldMkAppVariables α)

δonInductiveIndexInside ::
  α -> Φiis α Variable -> DI.Δiis α -> DT.Diff α -> DT.Diff α
δonInductiveIndexInside α = δListFoldLeft (δListFoldMkAppVariables α)

δonInductiveIndexOutside ::
  α -> Φiis α Variable -> DI.Δiis α -> DT.Diff α -> DT.Diff α
δonInductiveIndexOutside α = δListFoldRight (δListFoldMkPiVariables α)

δmkMotiveType'
  :: α -> DA.Diff Variable ->
  Φips α Variable -> DI.Δips α ->
  Φiis α Variable -> DI.Δiis α ->
  DT.Diff α
δmkMotiveType' α δn ips δips iis δiis =
  δonInductiveIndexOutside  α iis δiis
  $ (\ b -> DT.CpyPi b DA.Same DT.Same)
  $ δonInductiveIndexInside α iis δiis
  $ δonInductiveParameter   α ips δips
  $ DT.CpyVar δn

δmkMotiveType :: α -> Inductive α Variable -> DI.Diff α -> DT.Diff α
δmkMotiveType α (Inductive _ ips iis _) δi = case δi of
  DI.Same -> DT.Same
  DI.Modify δn δips δiis _ -> δmkMotiveType' α δn ips δips iis δiis
