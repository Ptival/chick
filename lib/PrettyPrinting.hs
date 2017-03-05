{-# language LambdaCase #-}
{-# language TypeFamilies #-}

module PrettyPrinting where

import Data.Default
import Text.PrettyPrint.Annotated.WL

import Precedence
import Term.RawTerm
import Term.Term

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

prettyTerm :: TermX ξ -> String
prettyTerm t = prettyTermString def (raw t)

prettyTermString :: ForallX ((~) a) ξ => PrecedenceTable -> TermX ξ -> String
prettyTermString precs = doc2String . prettyTermDoc precs

prettyVariable :: Variable -> Doc a
prettyVariable (Variable s) = text s

prettyBinder :: Binder -> Doc a
prettyBinder (Binder Nothing)  = text "_"
prettyBinder (Binder (Just v)) = prettyVariable v

prettyTermDoc :: ForallX ((~) a) ξ => PrecedenceTable -> TermX ξ -> Doc a
prettyTermDoc precs = go (PrecMin, TolerateEqual)
  where
    par :: (Precedence, Tolerance) -> (Doc a, Precedence) -> Doc a
    par (pOut, t) (d, pIn) =
      if isTolerable (tableToOrdering precs) pIn (pOut, t)
      then d
      else parens . nest 2 $ d

    go :: ForallX ((~) a) ξ => (Precedence, Tolerance) -> TermX ξ -> Doc a
    go pt = par pt . goTerm

    goTerm :: ForallX ((~) a) ξ => TermX ξ -> (Doc a, Precedence)
    goTerm = \case

      Annot a t τ ->
        (annotate a $ fillSep
         [ go (PrecAnnot, TolerateHigher) t
         , text annotSymbol
         , go (PrecAnnot, TolerateHigher) τ
         ]
        , PrecAnnot)

      App a t1 t2 ->
        (annotate a $ fillSep
         [ go (PrecApp, TolerateEqual) t1
         , go (PrecApp, TolerateHigher) t2
         ]
        , PrecApp)

      Hole a -> (annotate a $ text holeSymbol, PrecAtom)

      Lam a n t -> (goLams [] (Lam a n t), PrecLam)

      Let a n t1 t2 ->
        (annotate a $ fillSep
         [ text "let"
         , prettyBinder n
         , char '='
         , go (PrecMin, TolerateEqual) t1
         , text "in"
         , go (PrecLet, TolerateEqual) t2
         ]
        , PrecLet)

      Pi a (Binder Nothing) τ1 τ2 ->
        (annotate a $ fillSep
         [ go (PrecArrow, TolerateHigher) τ1
         , char '→'
         , go (PrecArrow, TolerateEqual) τ2
         ]
        , PrecArrow)

      Pi a (Binder (Just (Variable n))) τ1 τ2 ->
        (annotate a $ fillSep
         [ parens $ fillSep
           [ text n
           , char ':'
           , go (PrecMin, TolerateEqual) τ1
           ]
         , char '→'
         , go (PrecArrow, TolerateEqual) τ2
         ]
        , PrecArrow)

      Type a -> (annotate a $ text "Type", PrecAtom)

      Var a v -> (annotate a $ prettyVariable v, PrecAtom)

    goLams :: ForallX ((~) a) ξ => [Doc a] -> TermX ξ -> Doc a
    goLams l = \case
      Lam a n t -> goLams ((annotate a $ prettyBinder n) : l) t
      t -> fillSep
          [ char 'λ'
          , sep . reverse $ l
          , char '.'
          , go (PrecMin, TolerateEqual) t
          ]
