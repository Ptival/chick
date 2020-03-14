module PrettyPrinting.PrettyPrintableAnnotated where

{-

import Bound.Name
import Bound.Scope
import Control.Monad.Reader
import Data.Default
import Text.PrettyPrint.Annotated.WL

import DictMetaOut
import Precedence
import PrettyPrinting.PrettyPrintable
import PrettyPrinting.Utils
import Term.Binder
import Term.Term
import Term.Variable
import Term.TypeChecked

-- No, you cannot change Variable to ν
-- because prettyTermDocPrec instantiates the name

class PrettyPrintableAnnotated t where
  prettyDocA ::
    (MonadReader (DictMetaOut a ξ, PrecedenceTable) m) =>
    t ξ Variable -> m (Doc a)
  prettyStrA :: t (TypeChecked Variable) Variable -> String
  prettyStrA t =
    display . renderPretty 1.0 80 . runReader (prettyDocA t) $
    (ignoreAnnotations, def)

instance PrettyPrintableAnnotated TermX where
  prettyDocA t = do
    (dict, precs) <- ask
    return $ par precs (PrecMin, TolerateEqual) . prettyTermDocPrec dict precs $ t

prettyTermDocPrec ::
  forall a ξ. DictMetaOut a ξ -> PrecedenceTable -> TermX ξ Variable -> (Doc a, Precedence)
prettyTermDocPrec dict precs = goTerm

  where

    go :: (Precedence, Tolerance) -> TermX ξ Variable -> Doc a
    go pt = par precs pt . prettyTermDocPrec dict precs

    goTerm :: TermX ξ Variable -> (Doc a, Precedence)
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

      l@(Lam _ _) -> (goLams [] l, PrecLam)

      Let _ t1 bt2 ->
        let [Name n ()] = bindings bt2 in
        (fillSep
         [ text "let"
         , prettyDoc n
         , char '='
         , go (PrecMin, TolerateEqual) t1
         , text "in"
         , go (PrecLet, TolerateEqual) (instantiate1Name (Var n) bt2)
         ]
        , PrecLet)

      Pi _ τ1 bτ2 ->
        let [Name n ()] = bindings bτ2 in
          case unVariable n of
            "_" ->
              (fillSep
               [ go (PrecArrow, TolerateHigher) τ1
               , char '→'
               , go (PrecArrow, TolerateEqual) (instantiate1Name (Var n) bτ2)
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
               , go (PrecArrow, TolerateEqual) (instantiate1Name (Var n) bτ2)
               ]
              , PrecArrow)

      Type _ -> (text "Type", PrecAtom)

      Var v -> (prettyDoc v, PrecAtom)

    goLams :: [Doc a] -> TermX ξ Variable -> Doc a
    goLams l = \case
      Lam _ bt ->
        let [Name n ()] = bindings bt in
        goLams (prettyDoc n : l) (instantiate1Name (Var n) bt)
      t -> fillSep
          [ char 'λ'
          , fillSep . reverse $ l
          , char '.'
          , go (PrecMin, TolerateEqual) t
          ]
-}
