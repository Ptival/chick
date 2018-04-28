{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}

module PrettyPrinting.Chick.Term
  ( boundTermDocBinder
  , boundTermDocVariable
  ) where

import Bound.Name
import Control.Lens
import Control.Monad.Reader
import Data.Default
import Text.PrettyPrint.Annotated.WL

import Language (Language(Chick))
import Precedence
import PrettyPrinting.Chick.Binder ()
import PrettyPrinting.Chick.Variable ()
import PrettyPrinting.PrettyPrintable
import PrettyPrinting.PrettyPrintableUnannotated
import PrettyPrinting.Utils
import Term.Binder
import Term.Term

instance PrettyPrintableUnannotated 'Chick (Branch α Variable) where
  prettyDocU b = do
    precs <- ask
    return $ prettyBranchDocPrec precs b

instance PrettyPrintable 'Chick (Branch α Variable) where
  prettyDoc t = runReader (prettyDocU @'Chick t) def
  prettyStr = prettyStrU @'Chick

instance PrettyPrintableUnannotated 'Chick (TermX α Variable) where
  prettyDocU t = do
    precs <- ask
    return $ par precs (PrecMin, TolerateEqual) . prettyTermDocPrec precs $ t

instance PrettyPrintable 'Chick (TermX α Variable) where
  prettyDoc t = runReader (prettyDocU @'Chick t) def
  prettyStr = prettyStrU @'Chick

prettyBranchDocPrec :: PrecedenceTable -> Branch α Variable -> Doc ()
prettyBranchDocPrec precs b =
  let (ctor, args, body) = unpackBranch b in
  fillSep $
  [ text "|"
  , prettyDoc @'Chick ctor
  , fillSep $ map (prettyDoc @'Chick) args
  , text "=>"
  , fst $ prettyTermDocPrec precs body
  ]

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
        let n = originalVariable bt2 in
        (fillSep
         [ text "let"
         , prettyDoc @'Chick n
         , text ":="
         , go (PrecMin, TolerateEqual) t1
         , text "in"
         , go (PrecLet, TolerateEqual) (instantiate1Name (Var Nothing n) (view scopedTerm bt2))
         ]
        , PrecLet)

      Match _ d bs ->
        (line <> (indent 2 . align . vsep $ matchDoc), PrecMatch)
        where
          matchDoc = []
            ++ [ fillSep [ text "match", go (PrecMin, TolerateEqual) d, text "with" ] ]
            ++ map (prettyBranchDocPrec precs) bs
            ++ [ text "end" ]

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
          Just v ->
            (hcat
              [ text "∀"
              , space
              , parens $ fillSep
                [ prettyDoc @'Chick (if unVariable v == "_" then error "NOOOO" else v)
                , char ':'
                , go (PrecMin, TolerateEqual) τ1
                ]
              , comma
              , softline
              , go (PrecArrow, TolerateEqual) τ2
              ]
            , PrecArrow)

      Type u -> (text (show u), PrecAtom)

      Var _ v -> (prettyDoc @'Chick v, PrecAtom)

    goLams :: [Doc ()] -> TermX α Variable -> Doc ()
    goLams l = \case
      Lam _ bt ->
        let n = originalVariable bt in
        goLams (prettyDoc @'Chick n : l) (instantiate1Name (Var Nothing n) (view scopedTerm bt))
      t ->
        hcat
        [ text lamSymbol
        , space
        , fillSep . reverse $ l
        , text postLamSymbol
        , softline
        , go (PrecMin, TolerateEqual) t
        ]

boundTermDocBinder :: ∀ l m α.
  ( MonadReader PrecedenceTable m
  , PrettyPrintable l Variable
  , PrettyPrintableUnannotated l (TermX α Variable)
  ) =>
  (α, Binder Variable, TermX α Variable) -> m (Doc ())
boundTermDocBinder (α, Binder b, t) =
  case b of
    Nothing -> prettyDocU @l t
    Just v -> boundTermDocVariable @l (α, v, t)

boundTermDocVariable :: ∀ l m α.
  ( MonadReader PrecedenceTable m
  , PrettyPrintable l Variable
  , PrettyPrintableUnannotated l (TermX α Variable)
  ) =>
  (α, Variable, TermX α Variable) -> m (Doc ())
boundTermDocVariable (_, v, t) = do
  tDoc <- prettyDocU @l t
  return $ parens . fillSep $ [ prettyDoc @l v, text ":", tDoc ]
