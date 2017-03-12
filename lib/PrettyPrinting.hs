{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}

module PrettyPrinting where

import Data.Default
--import Data.List
import Text.PrettyPrint.Annotated.WL
--import Text.Printf

import DictMetaOut
import Inductive.Constructor
import Inductive.Inductive
import Precedence
--import Term.Raw                      as Raw
import Term.Term
import Term.TypeChecked         as TypeChecked
import Typing.LocalContext
import Typing.GlobalEnvironment

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

ignoreAnnotations :: DictMetaOut () ξ
ignoreAnnotations = DictMetaOut f f f f f f f f
  where
    f :: a -> ()
    f _ = ()

prettyTerm :: TermX ξ -> String
prettyTerm = doc2String . prettyTermDoc ignoreAnnotations def

prettyVariable :: Variable -> String
prettyVariable v = doc2String $ prettyVariableDoc v

prettyVariableDoc :: Variable -> Doc a
prettyVariableDoc (Variable s) = text s

prettyBinderDoc :: Binder -> Doc a
prettyBinderDoc (Binder Nothing)  = text "_"
prettyBinderDoc (Binder (Just v)) = prettyVariableDoc v

prettyLocalContext :: LocalContext ξ -> String
prettyLocalContext = doc2String . prettyLocalContextDoc ignoreAnnotations def

prettyLocalContextDoc ::
  DictMetaOut a ξ -> PrecedenceTable -> LocalContext ξ -> Doc a
prettyLocalContextDoc dict precs ctxt =
  vsep (map (prettyLocalDeclarationDoc dict precs) (reverse ctxt))

prettyLocalDeclarationDoc ::
  DictMetaOut a ξ -> PrecedenceTable -> LocalDeclaration ξ -> Doc a
prettyLocalDeclarationDoc dict precs = \case
  LocalAssum (Variable v) τ ->
    fillSep
    [ text v
    , char ':'
    , prettyTermDoc dict precs τ
    ]
  LocalDef (Variable v) t τ ->
    fillSep
    [ text v
    , text ":="
    , prettyTermDoc dict precs t
    , char ':'
    , prettyTermDoc dict precs τ
    ]

prettyGlobalEnvironment :: GlobalEnvironment TypeChecked -> String
prettyGlobalEnvironment =
  doc2String . prettyGlobalEnvironmentDoc ignoreAnnotations def

prettyGlobalEnvironmentDoc ::
  DictMetaOut a TypeChecked -> PrecedenceTable -> GlobalEnvironment TypeChecked -> Doc a
prettyGlobalEnvironmentDoc dict precs ge =
  vsep (map (prettyGlobalDeclarationDoc dict precs) (reverse ge))

prettyGlobalDeclarationDoc ::
  DictMetaOut a TypeChecked -> PrecedenceTable -> GlobalDeclaration TypeChecked -> Doc a
prettyGlobalDeclarationDoc dict precs = \case
  GlobalAssum (Variable v) τ ->
    fillSep
    [ text v
    , char ':'
    , prettyTermDoc dict precs τ
    ]
  GlobalDef (Variable v) t τ ->
    fillSep
    [ text v
    , text ":="
    , prettyTermDoc dict precs t
    , char ':'
    , prettyTermDoc dict precs τ
    ]
  GlobalInd ind -> prettyInductiveDoc dict precs ind

arrows :: [Doc a] -> Doc a
arrows = encloseSep mempty mempty (text " →")

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

prettyInductiveDoc ::
  DictMetaOut a TypeChecked -> PrecedenceTable -> Inductive TypeChecked -> Doc a
prettyInductiveDoc dict precs (Inductive n ps is cs) =
  vsep $
  [ fillSep $
    [ text "inductive"
    , prettyVariableDoc n
    ]
    ++
    (
      -- mempty creates an unwanted space, so have to use []
      if length ps == 0
      then []
      else [encloseSep mempty mempty mempty (map (prettyBindingDoc dict precs) ps)]
    )
    ++
    [ text ":"
    , arrows (map (prettyTermDoc dict precs) is ++ [text "Type"])
    , text "where"
    ]
  ] ++ map (indent 2 . prettyConstructorDoc dict precs n ps) cs

prettyConstructorDoc ::
  DictMetaOut a TypeChecked -> PrecedenceTable ->
  Variable -> [(Binder, TypeChecked.Type)] ->
  Constructor TypeChecked -> Doc a
prettyConstructorDoc dict precs ind indps (Constructor n ps is) =
  fillSep
  [ prettyVariableDoc n
  , text ":"
  , prettyTermDoc dict precs (constructorType ind indps ps is)
    {-
  , arrows (map (prettyBindingDoc dict precs) ps)
  , text "→"
  , prettyVariableDoc ind
  , encloseSep mempty mempty mempty
    (map (par precs (PrecApp, TolerateHigher) . prettyTermDocPrec dict precs) is)
    -}
  ]

prettyTermDoc' :: TermX ξ -> Doc ()
prettyTermDoc' = prettyTermDoc ignoreAnnotations def

par :: PrecedenceTable -> (Precedence, Tolerance) -> (Doc a, Precedence) -> Doc a
par precs (pOut, t) (d, pIn) =
  if isTolerable (tableToOrdering precs) pIn (pOut, t)
  then d
  else parens . nest 2 $ d

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
