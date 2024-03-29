{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Diff.Guess.Inductive
  ( guess,
  )
where

import Control.Monad (zipWithM)
import Data.Function.HT (nest)
import qualified Diff.Guess.Atom as ΔGA
import qualified Diff.Guess.Constructor as ΔGC
import qualified Diff.Guess.Term as ΔGT
import qualified Diff.Inductive as ΔI
import qualified Diff.List as ΔL
import Inductive.Inductive
  ( Inductive (Inductive),
    quantifyInductiveIndices,
    quantifyInductiveParameters,
  )
import Language (Language (Chick))
import Polysemy (Member, Sem)
import Polysemy.Trace (Trace, trace)
import PrettyPrinting.PrettyPrintable
  ( PrettyPrintable (prettyStr),
  )
import PrettyPrinting.Term ()
import qualified Term.Raw as Raw
import Term.Term (TermX (Var), Variable)
import Text.Printf (printf)
import Prelude hiding (product)

--import           StandardLibrary

guess ::
  Member Trace r =>
  Inductive Raw.Raw Variable ->
  Inductive Raw.Raw Variable ->
  Sem r (ΔI.Diff Raw.Raw)
guess i1@(Inductive n1 ips1 iis1 u1 cs1) i2@(Inductive n2 ips2 iis2 u2 cs2) =
  if i1 == i2
    then return ΔI.Same
    else do
      δn <- ΔGA.guess n1 n2

      {- To compute δips and δiis, I use a trick: instead of figuring out the
      permutations from the list itself, I build the telescope term from those and ask
      the term-diff-guesser to guess a diff for those terms.  From this term diff, I
      extract the information to create the list diff.
      -}

      let uniqueVar = Var Nothing "__UNIQUE__"

      let ipsTerm1 = quantifyInductiveParameters ips1 uniqueVar
      trace $ printf "ipsTerm1: %s" $ prettyStr @'Chick ipsTerm1
      let ipsTerm2 = quantifyInductiveParameters ips2 uniqueVar
      trace $ printf "ipsTerm2: %s" $ prettyStr @'Chick ipsTerm2
      δipsType <- ΔGT.guess ipsTerm1 ipsTerm2
      let δips = ΔGC.telescopeDiffToListDiffVariable ipsTerm1 δipsType
      trace "Guess for δips"
      trace $ show δips

      let iisTerm1 = quantifyInductiveIndices iis1 uniqueVar
      let iisTerm2 = quantifyInductiveIndices iis2 uniqueVar
      δiisType <- ΔGT.guess iisTerm1 iisTerm2
      let δiis = ΔGC.telescopeDiffToListDiff iisTerm1 δiisType iisTerm2
      trace "Guess for δiis:"
      trace $ show δiis

      δu <- ΔGA.guess u1 u2

      -- FIXME: for now I cheat and assume:
      -- - constructors are in the same order
      -- - if some are missing, they are the last ones
      -- - if some are introduced, they are the last ones
      -- Ideally, we'd have some heuristic to match seemingly-related constructors
      δcsList <- zipWithM ΔGC.guess cs1 cs2
      let (l1, l2) = (length cs1, length cs2)
      let base
            | l1 == l2 = ΔL.Same
            | l1 > l2 = nest (l1 - l2) ΔL.Remove ΔL.Same
            | otherwise = foldr ΔL.Insert ΔL.Same (drop l1 cs2)
      let δcs = foldr ΔL.Modify base δcsList

      return $ ΔI.Modify δn δips δiis δu δcs
