{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}

module PrettyPrinting.Chick.Term where

import Bound.Name
import Text.PrettyPrint.Annotated.WL

import Precedence
import PrettyPrinting.PrettyPrintable
import PrettyPrinting.Utils
import Term.Term
import Term.Variable

prettyTermDocPrec ::
  forall a ξ. PrecedenceTable -> TermX ξ Variable -> (Doc a, Precedence)
prettyTermDocPrec precs = goTerm

  where

    go :: (Precedence, Tolerance) -> TermX ξ Variable -> Doc a
    go pt = par precs pt . prettyTermDocPrec precs

    goTerm :: TermX ξ Variable -> (Doc a, Precedence)
    goTerm = \case

      Annot _ t τ ->
        (fillSep
         [ go (PrecAnnot, TolerateHigher) t
         , text annotSymbol
         , go (PrecAnnot, TolerateHigher) τ
         ]
        , PrecAnnot)

      App _ t1 t2 ->
        (fillSep
         [ go (PrecApp, TolerateEqual) t1
         , go (PrecApp, TolerateHigher) t2
         ]
        , PrecApp)

      Hole _ -> (text holeSymbol, PrecAtom)

      l@(Lam _ _) -> (goLams [] l, PrecLam)

      Let _ t1 bt2 ->
        let n = getName bt2 in
        (fillSep
         [ text "let"
         , prettyDoc n
         , char '='
         , go (PrecMin, TolerateEqual) t1
         , text "in"
         , go (PrecLet, TolerateEqual) (instantiate1Name (Var Nothing n) bt2)
         ]
        , PrecLet)

      Pi _ τ1 bτ2 ->
        let n = getName bτ2 in
          case unVariable n of
            "_" ->
              (fillSep
               [ go (PrecArrow, TolerateHigher) τ1
               , char '→'
               , go (PrecArrow, TolerateEqual) (instantiate1Name (Var Nothing n) bτ2)
               ]
              , PrecArrow)
            _ ->
              (fillSep
               [ parens $ fillSep
                 [ prettyDoc n
                 , char ':'
                 , go (PrecMin, TolerateEqual) τ1
                 ]
               , char '→'
               , go (PrecArrow, TolerateEqual) (instantiate1Name (Var Nothing n) bτ2)
               ]
              , PrecArrow)

      Type -> (text "Type", PrecAtom)

      Var _ v -> (prettyDoc v, PrecAtom)

    goLams :: [Doc a] -> TermX ξ Variable -> Doc a
    goLams l = \case
      Lam _ bt ->
        let n = getName bt in
        goLams (prettyDoc n : l) (instantiate1Name (Var Nothing n) bt)
      t -> fillSep
          [ char 'λ'
          , fillSep . reverse $ l
          , char '.'
          , go (PrecMin, TolerateEqual) t
          ]
