{-# language LambdaCase #-}
{-# language TypeFamilies #-}

module PrettyPrinting where

import Data.Maybe
import Text.PrettyPrint.Annotated.WL

import Precedence
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

prettyTermString :: ForallX ((~) a) ξ => PrecedenceTable -> TermX ξ -> String
prettyTermString precs = display . renderPrettyDefault . prettyTerm precs

prettyTerm :: ForallX ((~) a) ξ => PrecedenceTable -> TermX ξ -> Doc a
prettyTerm precs = go (PrecMin, TolerateEqual)
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
        (annotate a $ sep
         [ go (PrecAnnot, TolerateHigher) t
         , char '∷'
         , go (PrecAnnot, TolerateHigher) τ
         ]
        , PrecAnnot)

      App a t1 t2 ->
        (annotate a $ sep
         [ go (PrecApp, TolerateEqual) t1
         , go (PrecApp, TolerateHigher) t2
         ]
        , PrecApp)

      Hole a -> (annotate a $ char '_', PrecAtom)

      Lam a n t -> (goLams [] (Lam a n t), PrecLam)

      Let a n t1 t2 ->
        (annotate a $ sep
         [ text "let"
         , goBinder n
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

      Pi a (Binder (Just n)) τ1 τ2 ->
        (annotate a $ sep
         [ parens $ sep
           [ text n
           , char ':'
           , go (PrecMin, TolerateEqual) τ1
           ]
         , char '→'
         , go (PrecArrow, TolerateEqual) τ2
         ]
        , PrecArrow)

      Type a -> (annotate a $ text "Type", PrecAtom)

      Var a x -> (annotate a $ text x, PrecAtom)

    goBinder :: Binder -> Doc a
    goBinder (Binder b) = text . fromMaybe "_" $ b

    goLams :: ForallX ((~) a) ξ => [Doc a] -> TermX ξ -> Doc a
    goLams l = \case
      Lam a n t -> goLams ((annotate a $ goBinder n) : l) t
      t -> sep
          [ char 'λ'
          , sep . reverse $ l
          , char '.'
          , go (PrecMin, TolerateEqual) t
          ]
