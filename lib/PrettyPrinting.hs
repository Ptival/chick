{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}

module PrettyPrinting where

import Data.Default
--import Data.List
import Text.PrettyPrint.Annotated.WL
--import Text.Printf

import DictMetaOut
import Precedence
--import Term.Raw                      as Raw
import Term.Term
--import Term.TypeChecked
import Typing.LocalContext

{-
par :: Precedence -> Precedence -> Doc a -> Doc a
par pOut pIn d = if pIn >= pOut then parens (nest 2 d) else d
-}

{-
sep [hang (text "let") tabWidth (vcat (map prettyPrint decls)),
            hang (text "within") tabWidth (prettyPrint exp)]
-}

tabWidth :: Int
tabWidth = 20

doc2String :: Doc a -> String
doc2String = display . renderPretty 1.0 80

dictIgnore :: DictMetaOut () ξ
dictIgnore = DictMetaOut f f f f f f f f
  where
    f :: a -> ()
    f _ = ()

prettyTerm :: TermX ξ -> String
prettyTerm = doc2String . prettyTermDoc dictIgnore def

prettyVariable :: Variable -> String
prettyVariable v = doc2String $ prettyVariableDoc v

prettyVariableDoc :: Variable -> Doc a
prettyVariableDoc (Variable s) = text s

prettyBinderDoc :: Binder -> Doc a
prettyBinderDoc (Binder Nothing)  = text "_"
prettyBinderDoc (Binder (Just v)) = prettyVariableDoc v

prettyLocalDeclaration ::
  DictMetaOut a ξ -> PrecedenceTable -> LocalDeclaration ξ -> Doc a
prettyLocalDeclaration dict precs (LocalAssum (Variable v) τ) =
  fillSep
  [ text v
  , char ':'
  , prettyTermDoc dict precs τ
  ]
prettyLocalDeclaration dict precs (LocalDef (Variable v) t τ) =
  fillSep
  [ text v
  , text ":="
  , prettyTermDoc dict precs t
  , char ':'
  , prettyTermDoc dict precs τ
  ]

prettyTermDoc :: forall a ξ. DictMetaOut a ξ -> PrecedenceTable -> TermX ξ -> Doc a
prettyTermDoc dict precs = go (PrecMin, TolerateEqual)
  where
    par :: (Precedence, Tolerance) -> (Doc a, Precedence) -> Doc a
    par (pOut, t) (d, pIn) =
      if isTolerable (tableToOrdering precs) pIn (pOut, t)
      then d
      else parens . nest 2 $ d

    go :: (Precedence, Tolerance) -> TermX ξ -> Doc a
    go pt = par pt . goTerm

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
