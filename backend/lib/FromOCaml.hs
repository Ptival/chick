{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}

module FromOCaml
  ( FromOCaml(..)
  ) where

import Control.Arrow
import Data.String.QQ
import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser
import Text.Megaparsec

import Definition
import DefinitionObjectKind
import Inductive.Inductive
import Language
import PrettyPrinting.PrettyPrintableUnannotated
import PrettyPrinting.Chick ()
import Script
import Term.Term as Term
import Term.Universe as Universe
import Utils
import Vernacular

class FromOCaml a b | a -> b where
  fromOCaml :: a -> b

instance FromOCaml StructureItem (Vernacular () Variable) where
  fromOCaml ocaml = (fromOCaml . pstrDesc $ ocaml) ocaml

instance FromOCaml RecFlag DefinitionObjectKind where
  fromOCaml = \case
    Recursive    -> Fixpoint
    NonRecursive -> DefinitionObjectKind.Definition

instance FromOCaml CoreTypeDesc (CoreType -> TermX () Variable) where
  fromOCaml ocaml =
    let todo = error $ "TODO: " ++ show ocaml in
    case ocaml of
    PtypAny -> error "TODO"
    PtypVar v -> const $ Var Nothing $ mkVariable v
    PtypArrow l t1 t2 ->
      let st2 =
            case l of
            Nolabel    -> abstractAnonymous (fromOCaml t2)
            Labelled v -> abstractVariable (mkVariable v) (fromOCaml t2)
            Optional _ -> todo
      in
      const $ Pi () (fromOCaml t1) st2
    PtypTuple [] -> todo
    PtypTuple l -> const $ foldr1 (\ t ts -> App () (App () "prod" t) ts) (map fromOCaml l)
    PtypConstr i l ->
      case fromOCaml $ txt i of
      Nothing -> Term.UnsupportedOCaml . UnsupportedCoreType
      Just i' ->
        const $ foldl (\ acc elt -> App () acc elt) (Var Nothing i') (map fromOCaml l)
    PtypClass _ _ -> todo
    PtypAlias _ _ -> todo
    PtypPoly  _ _ -> todo
    PtypObject _ _ -> todo
    PtypVariant _ _ _ -> todo
    PtypPackage _ -> todo
    PtypExtension _ -> todo

instance FromOCaml CoreType (TermX () Variable) where
  fromOCaml t = fromOCaml (ptypDesc t) t

instance FromOCaml ConstructorArguments [TermX () Variable] where
  fromOCaml ocaml =
    case ocaml of
    PcstrTuple l -> map fromOCaml l
    PcstrRecord _ -> error "TODO"

instance FromOCaml
         ConstructorDeclaration
         (Inductive () Variable -> Constructor () Variable) where
  fromOCaml (ConstructorDeclaration { pcdName, pcdArgs, pcdRes }) ind =
    case pcdRes of
    Just _ -> error "TODO"
    Nothing -> Constructor
      { constructorInductive  = ind
      , constructorName       = mkVariable $ txt $ pcdName
      , constructorParameters = map (\ t -> ((), Binder Nothing, t)) (fromOCaml pcdArgs)
      , constructorIndices    = []
      }

instance FromOCaml StructureItemDesc (StructureItem -> Vernacular () Variable) where
  fromOCaml ocaml =
    let todo = error $ "TODO: " ++ show ocaml in
    case ocaml of

    PstrType _ [TypeDeclaration { ptypeKind, ptypeName, ptypeParams }] ->
      case ptypeKind of
        PtypeVariant cs ->
          let cs' = map fromOCaml cs in
          let ind = Inductive.Inductive.Inductive
                { inductiveName         = mkVariable $ txt $ ptypeName
                , inductiveParameters   =
                  let fromParam (typ, _) =
                        case fromOCaml typ of
                        Var _ v -> ((), v, Term.Type Universe.Type)
                        _ -> error "NO"
                  in
                  map fromParam ptypeParams
                , inductiveIndices      = []
                , inductiveUniverse     = Universe.Type
                , inductiveConstructors = map ($ ind) cs'
                }
          in
          const $ Vernacular.Inductive ind
        _ -> todo

    PstrValue r [vb] ->
      case ppatDesc . pvbPat $ vb of
      PpatVar v -> const . Vernacular.Definition $ Definition.Definition
        { definitionKind = fromOCaml r
        , definitionName = mkVariable (txt v)
        , definitionType = Hole ()
        , definitionTerm = fromOCaml . pvbExpr $ vb
        }
      _ -> todo

    _ -> todo

instance FromOCaml Expression (TermX () Variable) where
  fromOCaml ocaml = (fromOCaml . pexpDesc $ ocaml) ocaml

instance FromOCaml Longident (Maybe Variable) where
  fromOCaml ocaml = case ocaml of
    Lident i   -> Just $ mkVariable i
    Ldot   _ _ -> Nothing
    Lapply _ _ -> Nothing

instance FromOCaml ExpressionDesc (Expression -> TermX () Variable) where
  fromOCaml ocaml =
    let unsupported = Term.UnsupportedOCaml . UnsupportedExpression in
    case ocaml of

    PexpApply f as ->
      const $ mkApps (fromOCaml f) (map ((const ()) *** fromOCaml) as)

    PexpFunction l ->
      let arg :: Variable = "__arg__" in
      const
      $ Lam () $ abstractVariable arg
      $ Match () (Var Nothing arg)
      $ map fromOCaml l

    PexpIdent i -> case fromOCaml $ txt i of
      Nothing -> unsupported
      Just v -> const $ Var Nothing v

    PexpLet NonRecursive [vb] e ->
      case ppatDesc $ pvbPat vb of
      PpatVar v ->
        let e1 = fromOCaml . pvbExpr $ vb in
        let b  = mkBinder $ txt v in
        let e2 = fromOCaml e in
        const $ Let () e1 (abstractBinder b e2)
      _ -> unsupported

    _ -> error $ show ocaml

instance FromOCaml Case (Branch () Variable) where
  fromOCaml ocaml = case pcGuard ocaml of
    Nothing -> error "TODO"
    Just _ -> error "TODO"

_testProgram :: String
_testProgram = [s|
type 'a list =
  | Nil
  | Cons of ('a * 'a list)
|]

_test :: Maybe (Script () Variable)
_test = Script . map fromOCaml <$> parseMaybe implementationP _testProgram

_prettyTest :: Maybe String
_prettyTest = prettyStrU @'Chick <$> _test
