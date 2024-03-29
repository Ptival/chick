{-# LANGUAGE TupleSections #-}

module Parsing.Inductive
  ( inductiveP,
  )
where

import Control.Applicative (Alternative (many))
import Control.Monad.Fix (fix)
import Inductive.Inductive
  ( Constructor (Constructor),
    Inductive (Inductive),
  )
import Parsing (bindingsP, termP, variableP)
import Parsing.Chick.Utils (rword, symbol)
import Parsing.Types (Parser)
import Term.Raw as Raw (Raw)
import Term.Term (TermX (App, Pi, Type), Variable, unscopeTerm)

inductiveP :: Parser (Inductive Raw.Raw Variable)
inductiveP = do
  rword "Inductive"
  n <- variableP
  ips <- ipsP
  symbol ":"
  (iis, u) <- iisP
  symbol ":="
  cs <- csP ips
  return $ fix $ \ind -> Inductive n ips iis u (map ($ ind) cs)
  where
    mkIps (bs, τ) = map ((),,τ) bs
    ipsP = concatMap mkIps <$> bindingsP variableP

    iisP = do
      τ <- termP
      let (cis, univTerm) = peelPis ([], τ)
      let univ = case univTerm of
            Type u -> u
            _ -> error "ill-formed inductive indices"
      -- check sort?
      return (cis, univ)

    csP ips = many do
      symbol "|"
      cn <- variableP
      symbol ":"
      cτ <- termP
      let (cps, cis) = analyzeConstructor ips cτ
      return $ \ind -> Constructor ind cn cps cis

    analyzeConstructor ips τ =
      let (pis, τ1) = peelPis ([], τ)
       in let (apps, _) = peelApps ([], τ1)
           in let apps' = reverse (drop (length ips) (reverse apps))
               in -- check τ2?
                  (pis, apps')

    -- TODO: is this the same as extractPis from Utils?
    peelPis (rpis, Pi α τ1 bτ2) =
      let (b, τ2) = unscopeTerm bτ2
       in peelPis ((α, b, τ1) : rpis, τ2)
    peelPis (rpis, rest) = (reverse rpis, rest)

    peelApps (rapps, App α t1 t2) = peelApps ((α, t2) : rapps, t1)
    peelApps (rapps, rest) = (reverse rapps, rest)
