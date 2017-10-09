{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language UnicodeSyntax #-}

module PrettyPrinting.Chick.Term where

import Bound.Name
import Text.PrettyPrint.Annotated.WL

import Precedence
import PrettyPrinting.PrettyPrintable
import PrettyPrinting.Utils
import Term.Binder
import Term.Term

prettyTermDocPrec :: ∀ α. PrecedenceTable -> TermX α Variable -> (Doc (), Precedence)
prettyTermDocPrec precs = goTerm

  where

    go :: (Precedence, Tolerance) -> TermX α Variable -> Doc ()
    go pt = par precs pt . prettyTermDocPrec precs

    goTerm :: TermX α Variable -> (Doc (), Precedence)
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
         , text ":="
         , go (PrecMin, TolerateEqual) t1
         , text "in"
         , go (PrecLet, TolerateEqual) (instantiate1Name (Var Nothing n) bt2)
         ]
        , PrecLet)

      Match _ d bs ->
        (line <> (indent 2 . align . vsep $ matchDoc), PrecMatch)
        where
          matchDoc = []
            ++ [ fillSep [ text "match", go (PrecMin, TolerateEqual) d, text "with" ] ]
            ++ map goBranch bs
            ++ [ text "end" ]
          goBranch (ctor, nbArgs, bbody) =
            let (namer, body) = unscopeNames bbody in
            fillSep $
            [ text "|"
            , prettyDoc ctor
            , fillSep $ map (maybeVarDoc . namer) [0..nbArgs-1]
            , text "=>"
            , go (PrecMin, TolerateEqual) body
            ]
          maybeVarDoc = \case
            Nothing -> text "_"
            Just v  -> prettyDoc v

      Pi _ τ1 bτ2 ->
        let (b, τ2) = unscopeTerm bτ2 in
        case unBinder b of
          Nothing ->
            (fillSep
              [ go (PrecArrow, TolerateHigher) τ1
              , text arrowSymbol
              , go (PrecArrow, TolerateEqual) τ2
              ]
            , PrecArrow)
          Just _ ->
            (hcat
              [ text "∀"
              , space
              , parens $ fillSep
                [ prettyDoc b
                , char ':'
                , go (PrecMin, TolerateEqual) τ1
                ]
              , comma
              , softline
              , go (PrecArrow, TolerateEqual) τ2
              ]
            , PrecArrow)

      Type u -> (text (show u), PrecAtom)

      Var _ v -> (prettyDoc v, PrecAtom)

    goLams :: [Doc ()] -> TermX α Variable -> Doc ()
    goLams l = \case
      Lam _ bt ->
        let n = getName bt in
        goLams (prettyDoc n : l) (instantiate1Name (Var Nothing n) bt)
      t ->
        hcat
        [ text lamSymbol
        , space
        , fillSep . reverse $ l
        , text postLamSymbol
        , softline
        , go (PrecMin, TolerateEqual) t
        ]
