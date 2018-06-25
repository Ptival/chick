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
import Term.Term as Term
import Term.Universe as Universe
import Utils
import Vernacular

class FromOCaml a b | a -> b where
  fromOCaml :: a -> b

instance FromOCaml Structure_item (Vernacular () Variable) where
  fromOCaml ocaml = (fromOCaml . pstr_desc $ ocaml) ocaml

instance FromOCaml Rec_flag DefinitionObjectKind where
  fromOCaml = \case
    Recursive    -> Fixpoint
    Nonrecursive -> DefinitionObjectKind.Definition

instance FromOCaml Core_type_desc (Core_type -> TermX () Variable) where
  fromOCaml ocaml =
    let todo = error $ "TODO: " ++ show ocaml in
    case ocaml of
    Ptyp_any -> error "TODO"
    Ptyp_var v -> const $ Var Nothing $ mkVariable v
    Ptyp_arrow l t1 t2 ->
      let st2 =
            case l of
            Nolabel    -> abstractAnonymous (fromOCaml t2)
            Labelled v -> abstractVariable (mkVariable v) (fromOCaml t2)
            Optional _ -> todo
      in
      const $ Pi () (fromOCaml t1) st2
    Ptyp_tuple [] -> todo
    Ptyp_tuple l -> const $ foldr1 (\ t ts -> App () (App () "prod" t) ts) (map fromOCaml l)
    Ptyp_constr i l ->
      case fromOCaml $ txt i of
      Nothing -> Term.UnsupportedOCaml . UnsupportedCoreType
      Just i' ->
        const $ foldl (\ acc elt -> App () acc elt) (Var Nothing i') (map fromOCaml l)
    Ptyp_class  _ _ -> todo
    Ptyp_alias  _ _ -> todo
    Ptyp_poly   _ _ -> todo

instance FromOCaml Core_type (TermX () Variable) where
  fromOCaml t = fromOCaml (ptyp_desc t) t

instance FromOCaml Constructor_arguments [TermX () Variable] where
  fromOCaml ocaml =
    case ocaml of
    Pcstr_tuple l -> map fromOCaml l

instance FromOCaml
         Constructor_declaration
         (Inductive () Variable -> Constructor () Variable) where
  fromOCaml (Constructor_declaration { pcd_name, pcd_args, pcd_res }) ind =
    case pcd_res of
    Just _ -> error "TODO"
    Nothing -> Constructor
      { constructorInductive  = ind
      , constructorName       = mkVariable $ txt $ pcd_name
      , constructorParameters = map (\ t -> ((), Binder Nothing, t)) (fromOCaml pcd_args)
      , constructorIndices    = []
      }

instance FromOCaml Structure_item_desc (Structure_item -> Vernacular () Variable) where
  fromOCaml ocaml =
    let todo = error $ "TODO: " ++ show ocaml in
    case ocaml of

    Pstr_type _ [Type_declaration { ptype_kind, ptype_name, ptype_params }] ->
      case ptype_kind of
        Ptype_variant cs ->
          let cs' = map fromOCaml cs in
          let ind = Inductive.Inductive.Inductive
                { inductiveName         = mkVariable $ txt $ ptype_name
                , inductiveParameters   =
                  let fromParam (typ, _) =
                        case fromOCaml typ of
                        Var _ v -> ((), v, Term.Type Universe.Type)
                        _ -> error "NO"
                  in
                  map fromParam ptype_params
                , inductiveIndices      = []
                , inductiveUniverse     = Universe.Type
                , inductiveConstructors = map ($ ind) cs'
                }
          in
          const $ Vernacular.Inductive ind
        _ -> todo

    Pstr_value r [vb] ->
      case ppat_desc . pvb_pat $ vb of
      Ppat_var v -> const . Vernacular.Definition $ Definition.Definition
        { definitionKind = fromOCaml r
        , definitionName = mkVariable (txt v)
        , definitionType = Hole ()
        , definitionTerm = fromOCaml . pvb_expr $ vb
        }
      _ -> todo

    _ -> todo

instance FromOCaml Expression (TermX () Variable) where
  fromOCaml ocaml = (fromOCaml . pexp_desc $ ocaml) ocaml

instance FromOCaml Longident (Maybe Variable) where
  fromOCaml ocaml = case ocaml of
    Lident i   -> Just $ mkVariable i
    Ldot   _ _ -> Nothing
    Lapply _ _ -> Nothing

instance FromOCaml Expression_desc (Expression -> TermX () Variable) where
  fromOCaml ocaml =
    let unsupported = Term.UnsupportedOCaml . UnsupportedExpression in
    case ocaml of

    Pexp_apply f as ->
      const $ mkApps (fromOCaml f) (map ((const ()) *** fromOCaml) as)

    Pexp_function l ->
      let arg :: Variable = "__arg__" in
      const
      $ Lam () $ abstractVariable arg
      $ Match () (Var Nothing arg)
      $ map fromOCaml l

    Pexp_ident i -> case fromOCaml $ txt i of
      Nothing -> unsupported
      Just v -> const $ Var Nothing v

    Pexp_let Nonrecursive [vb] e ->
      case ppat_desc $ pvb_pat vb of
      Ppat_var v ->
        let e1 = fromOCaml . pvb_expr $ vb in
        let b  = mkBinder $ txt v in
        let e2 = fromOCaml e in
        const $ Let () e1 (abstractBinder b e2)
      _ -> unsupported

    _ -> error $ show ocaml

instance FromOCaml Case (Branch () Variable) where
  fromOCaml ocaml = case pc_guard ocaml of
    Nothing -> error "TODO"
    Just _ -> error "TODO"

_testProgram :: String
_testProgram = [s|
type 'a list =
  | Nil
  | Cons of ('a * 'a list)
|]

_test :: Maybe [Vernacular () Variable]
_test = map fromOCaml <$> parseMaybe implementation_P _testProgram

_prettyTest :: Maybe String
_prettyTest = prettyStrU @'Chick <$> _test
