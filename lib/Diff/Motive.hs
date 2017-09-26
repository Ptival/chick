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
import           PrettyPrinting.PrettyPrintable
import           Term.Term

δonInductiveParameter :: Φips α Variable -> DI.Δips α -> DT.Diff α -> DT.Diff α
δonInductiveParameter = δListFoldLeft δListFoldMkAppVariables

δonInductiveIndexInside :: Φiis α Variable -> DI.Δiis α -> DT.Diff α -> DT.Diff α
δonInductiveIndexInside = δListFoldLeft δListFoldMkAppVariables

δonInductiveIndexOutside ::
  PrettyPrintable α =>
  Φiis α Variable -> DI.Δiis α -> DT.Diff α -> DT.Diff α
δonInductiveIndexOutside = δListFoldRight δListFoldMkPiVariables

δmkMotiveType' ::
  PrettyPrintable α =>
  DA.Diff Variable ->
  Φips α Variable -> DI.Δips α ->
  Φiis α Variable -> DI.Δiis α ->
  DT.Diff α
δmkMotiveType' δn ips δips iis δiis =
  δonInductiveIndexOutside  iis δiis
  $ (\ b -> DT.CpyPi b DA.Same DT.Same)
  $ δonInductiveIndexInside iis δiis
  $ δonInductiveParameter   ips δips
  $ DT.CpyVar δn

δmkMotiveType :: PrettyPrintable α => Inductive α Variable -> DI.Diff α -> DT.Diff α
δmkMotiveType (Inductive _ ips iis _) δi = case δi of
  DI.Same -> DT.Same
  DI.Modify δn δips δiis _ -> δmkMotiveType' δn ips δips iis δiis
