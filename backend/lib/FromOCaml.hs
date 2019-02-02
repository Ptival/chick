{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module FromOCaml
  ( FromOCaml(..)
  ) where

import Control.Arrow
import Data.List (genericLength)
import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree

import Definition
import DefinitionObjectKind
import Inductive.Inductive
import PrettyPrinting.Chick ()
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
    -- problem: in OCaml, we can distinguish (a * (b * c)) from (a * b * c)
    PtypTuple [l, r] -> const $ App () (App () "prod" (fromOCaml l)) (fromOCaml r)
    PtypTuple _ -> Term.UnsupportedOCaml . UnsupportedCoreType
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
      -- OCaml constructors are uncurried
      , constructorParameters = [((), Binder Nothing, mkParameter pcdArgs)]
      , constructorIndices    = []
      }
      where
        mkParameter :: ConstructorArguments -> TermX () Variable
        mkParameter (PcstrTuple args) = mkApps (Var Nothing "tuple") (((), genericLength args) : map (const () &&& fromOCaml) args)
        mkParameter (PcstrRecord _) = error "TODO"

instance FromOCaml StructureItemDesc (StructureItem -> Vernacular () Variable) where
  fromOCaml ocaml =
    let todo = error $ "TODO: " ++ show ocaml in
    case ocaml of

    PstrType _ [TypeDeclaration { ptypeKind, ptypeName, ptypeParams }] ->
      case ptypeKind of

      PtypeAbstract -> error "TODO: PtypeAbstract"

      PtypeVariant cs ->
        let cs' = map fromOCaml cs in
        let ind = Inductive.Inductive.Inductive
                  { inductiveName         = mkVariable $ txt $ ptypeName
                  , inductiveParameters   =
                    let fromParam (typ, _) =
                          case fromOCaml typ of
                          -- FIXME: Chick requires named parameters, OCaml does not
                          Var _ v -> ((), "_param", Term.Var Nothing v)
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

    PexpConstruct ctor maybeArgsExpr ->
      case maybeArgsExpr of
      Nothing ->
        case fromOCaml ctor of
        Nothing -> error "TODO"
        Just ctor' -> const $ Var Nothing ctor'
      Just (Expression { pexpDesc = argsExpr }) ->
        case argsExpr of
        PexpTuple args ->
          case fromOCaml ctor of
          Nothing -> error "TODO"
          Just ctor' ->
            -- OCaml constructors are uncurried
            let argsAsTuple = mkApps (Var Nothing "tuple") (((), genericLength args) : map ((,) () . fromOCaml) args) in
            const $ App () (Var Nothing ctor') argsAsTuple
        _ -> error $ "\n\n" ++ show argsExpr

    PexpFun Nolabel Nothing patt expr ->
      case ppatDesc patt of
      PpatVar ls ->
        let v = mkVariable $ txt ls in
        const
        $ Lam ()
        $ abstractVariable v
        $ fromOCaml expr
      _ -> error $ show ocaml

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

    PexpMatch disc cases ->
      const
      $ Match () (fromOCaml disc)
      $ map fromOCaml cases

    _ -> error $ show ocaml

instance FromOCaml Case (Branch () Variable) where
  fromOCaml (Case { pcGuard, pcLHS, pcRHS }) =
    let (ctor, args) = fromOCaml pcLHS in
    let guard = fromOCaml <$> pcGuard in
    let body = fromOCaml pcRHS in
    packBranch (ctor, args, GuardAndBody guard body)

instance FromOCaml Pattern (Variable, [Binder Variable]) where
  fromOCaml (Pattern { ppatDesc }) =
    case ppatDesc of
    PpatConstruct constr maybeArgsPattern ->
      let ctor = case fromOCaml constr of
            Nothing -> error "TODO"
            Just ctor' -> ctor'
      in
      let args = case maybeArgsPattern of
            Nothing -> []
            Just argsPattern ->
              let Pattern { ppatDesc = argsPpatDesc } = argsPattern in
              case argsPpatDesc of
              PpatTuple args' ->
                let patternToBinder (Pattern { ppatDesc = innerPpatDesc }) = case innerPpatDesc of
                      PpatVar v -> Binder (Just $ mkVariable $ fromOCaml v)
                      PpatAny -> Binder Nothing
                      _ -> error "TODO"
                in
                map patternToBinder args'
              _ -> error "TODO"
      in
      (ctor, args)
    _ -> error "TODO"

instance FromOCaml a b => FromOCaml (Loc a) b where
  fromOCaml = fromOCaml . txt

instance FromOCaml String String where
  fromOCaml = id
