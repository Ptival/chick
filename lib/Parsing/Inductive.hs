{-# language LambdaCase #-}
{-# language RankNTypes #-}

module Parsing.Inductive
  ( inductiveP
  ) where

import Control.Applicative
import Control.Monad.Fix
import Text.Megaparsec.String

import Inductive.Inductive
import Parsing
import Parsing.Utils
import Term.Raw as Raw
import Term.Term

inductiveP :: Parser (Inductive Raw.Raw Variable)
inductiveP = do
  rword "Inductive"
  n <- variableP
  ips <- ipsP
  symbol ":"
  (iis, u) <- iisP
  symbol ":="
  cs <- csP ips
  return $ fix $ \ ind -> Inductive n ips iis u (map ($ ind) cs)
  where

    mkIps (bs, τ) = map (\ b -> ((), b, τ)) bs
    ipsP = concatMap mkIps <$> bindingsP variableP

    iisP = do
      τ <- termP
      let (cis, univTerm) = peelPis ([], τ)
      let univ = case univTerm of
              Type u -> u
              _ -> error "ill-formed inductive indices"
      -- check sort?
      return (cis, univ)

    csP ips = many $ do
      symbol "|"
      cn <- variableP
      symbol ":"
      cτ <- termP
      let (cps, cis) = analyzeConstructor ips cτ
      return $ \ ind -> Constructor ind cn cps cis

    analyzeConstructor ips τ =
      let (pis,  τ1) = peelPis  ([], τ ) in
      let (apps,  _) = peelApps ([], τ1) in
      let apps' = reverse (drop (length ips) (reverse apps)) in
      -- check τ2?
      (pis, apps')

    peelPis (rpis, Pi α τ1 bτ2) =
      let (_, τ2) = unscopeTerm bτ2 in
      let v = getName bτ2 in
      peelPis ((α, v, τ1) : rpis, τ2)
    peelPis (rpis, rest) = (reverse rpis, rest)

    peelApps (rapps, App α t1 t2) = peelApps ((α, t2) : rapps, t1)
    peelApps (rapps, rest) = (reverse rapps, rest)
