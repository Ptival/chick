{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module PrettyPrinting.Chick.Term
  ( boundTermDocBinder,
    boundTermDocVariable,
  )
where

import Bound.Name (instantiate1Name)
import Control.Lens (view)
import Control.Monad.Reader (MonadReader, ask, runReader)
import Data.Default (Default (def))
import Language (Language (Chick))
import Precedence
  ( Precedence
      ( PrecAnnot,
        PrecApp,
        PrecArrow,
        PrecAtom,
        PrecLam,
        PrecLet,
        PrecMatch,
        PrecMin
      ),
    PrecedenceTable,
    Tolerance (TolerateEqual, TolerateHigher),
  )
import PrettyPrinting.Chick.Binder ()
import PrettyPrinting.Chick.Variable ()
import PrettyPrinting.PrettyPrintable
  ( PrettyPrintable (prettyDoc, prettyStr),
  )
import PrettyPrinting.PrettyPrintableUnannotated
  ( PrettyPrintableUnannotated (..),
  )
import PrettyPrinting.Utils (par)
import qualified Prettyprinter as Doc
import Term.Binder (Binder (..))
import Term.Term
  ( Branch,
    GuardAndBody (branchBody, branchGuard),
    TermX (..),
    Variable (..),
    annotSymbol,
    arrowSymbol,
    forallSymbol,
    hasTypeSymbol,
    holeSymbol,
    lamSymbol,
    originalVariable,
    postForallSymbol,
    postLamSymbol,
    postLetSymbol,
    scopedTerm,
    unpackBranch,
    unscopeTerm,
  )

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

prettyGuardAndBodyDocPrec :: PrecedenceTable -> GuardAndBody (TermX α) Variable -> Doc.Doc ()
prettyGuardAndBodyDocPrec precs gb = Doc.fillCat [guard, "=", Doc.space, body]
  where
    guard = case branchGuard gb of
      Nothing -> ""
      Just g -> Doc.fillCat [fst $ prettyTermDocPrec precs g, Doc.space]
    body = fst $ prettyTermDocPrec precs $ branchBody gb

prettyBranchDocPrec :: PrecedenceTable -> Branch α Variable -> Doc.Doc ()
prettyBranchDocPrec precs b =
  let (ctor, args, guardbody) = unpackBranch b
   in Doc.fillSep
        [ "|",
          prettyDoc @'Chick ctor,
          Doc.fillSep $ map (prettyDoc @'Chick) args,
          "=>",
          prettyGuardAndBodyDocPrec precs guardbody
        ]

prettyTermDocPrec :: forall α. PrecedenceTable -> TermX α Variable -> (Doc.Doc (), Precedence)
prettyTermDocPrec precs = goTerm
  where
    go :: (Precedence, Tolerance) -> TermX α Variable -> Doc.Doc ()
    go pt = par precs pt . prettyTermDocPrec precs

    goTerm :: TermX α Variable -> (Doc.Doc (), Precedence)
    goTerm = \case
      Annot _ t τ ->
        ( Doc.fillSep
            [ go (PrecAnnot, TolerateHigher) t,
              Doc.pretty annotSymbol,
              go (PrecAnnot, TolerateHigher) τ
            ],
          PrecAnnot
        )
      App _ t1 t2 ->
        ( Doc.fillSep
            [ go (PrecApp, TolerateEqual) t1,
              go (PrecApp, TolerateHigher) t2
            ],
          PrecApp
        )
      Hole _ -> (Doc.pretty holeSymbol, PrecAtom)
      l@(Lam _ _) -> (goLams [] l, PrecLam)
      Let _ t1 bt2 ->
        let n = originalVariable bt2
         in ( Doc.fillSep
                [ "let",
                  prettyDoc @'Chick n,
                  Doc.pretty postLetSymbol,
                  go (PrecMin, TolerateEqual) t1,
                  "in",
                  go (PrecLet, TolerateEqual) (instantiate1Name (Var Nothing n) (view scopedTerm bt2))
                ],
              PrecLet
            )
      Match _ d bs ->
        (Doc.line <> (Doc.indent 2 . Doc.align . Doc.vsep $ matchDoc), PrecMatch)
        where
          matchDoc =
            []
              ++ [Doc.fillSep ["match", go (PrecMin, TolerateEqual) d, "with"]]
              ++ map (prettyBranchDocPrec precs) bs
              ++ ["end"]
      Pi _ τ1 bτ2 ->
        let (b, τ2) = unscopeTerm bτ2
         in case unBinder b of
              Nothing ->
                ( Doc.fillSep
                    [ go (PrecArrow, TolerateHigher) τ1,
                      Doc.pretty arrowSymbol,
                      go (PrecArrow, TolerateEqual) τ2
                    ],
                  PrecArrow
                )
              Just v ->
                ( Doc.hcat
                    [ Doc.pretty forallSymbol,
                      Doc.space,
                      Doc.parens $
                        Doc.fillSep
                          [ prettyDoc @'Chick (if unVariable v == "_" then error "NOOOO" else v),
                            Doc.pretty hasTypeSymbol,
                            go (PrecMin, TolerateEqual) τ1
                          ],
                      Doc.pretty postForallSymbol,
                      Doc.softline,
                      go (PrecArrow, TolerateEqual) τ2
                    ],
                  PrecArrow
                )
      Type u -> (Doc.pretty $ show u, PrecAtom)
      Var _ v -> (prettyDoc @'Chick v, PrecAtom)
      UnsupportedOCaml _o -> error "TODO"

    goLams :: [Doc.Doc ()] -> TermX α Variable -> Doc.Doc ()
    goLams l = \case
      Lam _ bt ->
        let n = originalVariable bt
         in goLams (prettyDoc @'Chick n : l) (instantiate1Name (Var Nothing n) (view scopedTerm bt))
      t ->
        Doc.hcat
          [ Doc.pretty lamSymbol,
            Doc.space,
            Doc.fillSep . reverse $ l,
            Doc.pretty postLamSymbol,
            Doc.softline,
            go (PrecMin, TolerateEqual) t
          ]

boundTermDocBinder ::
  forall l m α.
  ( MonadReader PrecedenceTable m,
    PrettyPrintable l Variable,
    PrettyPrintableUnannotated l (TermX α Variable)
  ) =>
  (α, Binder Variable, TermX α Variable) ->
  m (Doc.Doc ())
boundTermDocBinder (α, Binder b, t) =
  case b of
    Nothing -> prettyDocU @l t
    Just v -> boundTermDocVariable @l (α, v, t)

boundTermDocVariable ::
  forall l m α.
  ( MonadReader PrecedenceTable m,
    PrettyPrintable l Variable,
    PrettyPrintableUnannotated l (TermX α Variable)
  ) =>
  (α, Variable, TermX α Variable) ->
  m (Doc.Doc ())
boundTermDocVariable (_, v, t) = do
  tDoc <- prettyDocU @l t
  return $ Doc.parens . Doc.fillSep $ [prettyDoc @l v, ":", tDoc]
