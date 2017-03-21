{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}

module PrettyPrinting.Term where

import Data.Default
import Text.PrettyPrint.Annotated.WL

import DictMetaOut
import Precedence
import PrettyPrinting.Binder
import PrettyPrinting.Utils
import PrettyPrinting.Variable
import Term.Term

prettyTerm :: TermX ξ -> String
prettyTerm = withDefaultTermParser prettyTermDoc

prettyTermDoc' :: TermX ξ -> Doc ()
prettyTermDoc' = prettyTermDoc ignoreAnnotations def

prettyTermDoc :: forall a ξ. DictMetaOut a ξ -> PrecedenceTable -> TermX ξ -> Doc a
prettyTermDoc dict precs = go (PrecMin, TolerateEqual)
  where

    go :: (Precedence, Tolerance) -> TermX ξ -> Doc a
    go pt = par precs pt . prettyTermDocPrec dict precs

prettyTermDocPrec ::
  forall a ξ. DictMetaOut a ξ -> PrecedenceTable -> TermX ξ -> (Doc a, Precedence)
prettyTermDocPrec dict precs = goTerm

  where

    go :: (Precedence, Tolerance) -> TermX ξ -> Doc a
    go pt = par precs pt . prettyTermDocPrec dict precs

    goTerm :: TermX ξ -> (Doc a, Precedence)
    goTerm term =
      (\ (doc, p) -> (annotate (metaOut dict term) doc, p))
      $ case term of

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

      l@(Lam _ _ _) -> (goLams [] l, PrecLam)

      Let _ n t1 t2 ->
        (fillSep
         [ text "let"
         , prettyBinderDoc n
         , char '='
         , go (PrecMin, TolerateEqual) t1
         , text "in"
         , go (PrecLet, TolerateEqual) t2
         ]
        , PrecLet)

      Pi _ (Binder Nothing) τ1 τ2 ->
        (fillSep
         [ go (PrecArrow, TolerateHigher) τ1
         , char '→'
         , go (PrecArrow, TolerateEqual) τ2
         ]
        , PrecArrow)

      Pi _ (Binder (Just (Variable n))) τ1 τ2 ->
        (fillSep
         [ parens $ fillSep
           [ text n
           , char ':'
           , go (PrecMin, TolerateEqual) τ1
           ]
         , char '→'
         , go (PrecArrow, TolerateEqual) τ2
         ]
        , PrecArrow)

      Type _ -> (text "Type", PrecAtom)

      Var _ v -> (prettyVariableDoc v, PrecAtom)

    goLams :: [Doc a] -> TermX ξ -> Doc a
    goLams l = \case
      Lam _ n t -> goLams (prettyBinderDoc n : l) t
      t -> fillSep
          [ char 'λ'
          , fillSep . reverse $ l
          , char '.'
          , go (PrecMin, TolerateEqual) t
          ]

prettyBindingDoc ::
  DictMetaOut a ξ -> PrecedenceTable -> (Binder, TermX ξ) -> Doc a
prettyBindingDoc dict precs (Binder b, t) =
  case b of
  Nothing -> prettyTermDoc dict precs t
  Just v ->
    parens . fillSep $
    [ prettyVariableDoc v
    , text ":"
    , prettyTermDoc dict precs t
    ]
