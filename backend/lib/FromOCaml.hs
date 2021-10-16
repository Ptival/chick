{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module FromOCaml
  ( FromOCaml (..),
  )
where

import Control.Arrow (Arrow ((&&&), (***)))
import Data.List (genericLength)
import Data.String.QQ (s)
import Definition
  ( Definition
      ( Definition,
        definitionKind,
        definitionName,
        definitionTerm,
        definitionType
      ),
  )
import DefinitionObjectKind (DefinitionObjectKind (..))
import Inductive.Inductive (Constructor (..), Inductive (..))
import Language (Language (Chick))
import Language.OCaml.Definitions.Parsing.ASTTypes
  ( ArgLabel (Labelled, Nolabel, Optional),
    Loc (txt),
    RecFlag (..),
  )
import Language.OCaml.Definitions.Parsing.ParseTree
  ( Case (..),
    ConstructorArguments (..),
    ConstructorDeclaration (..),
    CoreType (ptypDesc),
    CoreTypeDesc (..),
    Expression (Expression, pexpDesc),
    ExpressionDesc
      ( PexpApply,
        PexpConstruct,
        PexpFun,
        PexpFunction,
        PexpIdent,
        PexpLet,
        PexpMatch,
        PexpTuple
      ),
    Longident (..),
    Pattern (..),
    PatternDesc (PpatAny, PpatConstruct, PpatTuple, PpatVar),
    StructureItem (pstrDesc),
    StructureItemDesc (PstrType, PstrValue),
    TypeDeclaration
      ( TypeDeclaration,
        ptypeKind,
        ptypeName,
        ptypeParams
      ),
    TypeKind (PtypeAbstract, PtypeVariant),
    ValueBinding (pvbExpr, pvbPat),
  )
import Language.OCaml.Parser (parseImplementation)
import PrettyPrinting.Chick ()
import PrettyPrinting.PrettyPrintableUnannotated
  ( PrettyPrintableUnannotated (prettyStrU),
  )
import Script (Script (Script))
import Term.Term as Term
  ( Binder (Binder),
    Branch,
    GuardAndBody (GuardAndBody),
    TermX (App, Hole, Lam, Let, Match, Pi, UnsupportedOCaml, Var),
    UnsupportedOCamlTerm (UnsupportedCoreType, UnsupportedExpression),
    Variable,
    abstractAnonymous,
    abstractBinder,
    abstractVariable,
    mkBinder,
    mkVariable,
    packBranch,
  )
import Term.Universe as Universe (Universe (Type))
import Utils (mkApps)
import Vernacular (Vernacular (Definition, Inductive))

class FromOCaml a b | a -> b where
  fromOCaml :: a -> b

instance FromOCaml StructureItem (Vernacular () Variable) where
  fromOCaml ocaml = (fromOCaml . pstrDesc $ ocaml) ocaml

instance FromOCaml RecFlag DefinitionObjectKind where
  fromOCaml = \case
    Recursive -> Fixpoint
    NonRecursive -> DefinitionObjectKind.Definition

instance FromOCaml CoreTypeDesc (CoreType -> TermX () Variable) where
  fromOCaml ocaml =
    let todo :: CoreType -> TermX () Variable = error $ "TODO: " ++ show ocaml
     in case ocaml of
          PtypAny -> error "TODO"
          PtypVar v -> const $ Var Nothing $ mkVariable v
          PtypArrow l t1 t2 ->
            let st2 =
                  case l of
                    Nolabel -> abstractAnonymous (fromOCaml t2)
                    Labelled v -> abstractVariable (mkVariable v) (fromOCaml t2)
                    Optional _ -> error "TODO"
             in const $ Pi () (fromOCaml t1) st2
          PtypTuple [] -> todo
          -- problem: in OCaml, we can distinguish (a * (b * c)) from (a * b * c)
          PtypTuple [l, r] -> const $ App () (App () "prod" (fromOCaml l)) (fromOCaml r)
          PtypTuple _ -> Term.UnsupportedOCaml . UnsupportedCoreType
          PtypConstr i l ->
            case fromOCaml $ txt i of
              Nothing -> Term.UnsupportedOCaml . UnsupportedCoreType
              Just i' ->
                const $ foldl (App ()) (Var Nothing i') (map fromOCaml l)
          PtypClass _ _ -> todo
          PtypAlias _ _ -> todo
          PtypPoly _ _ -> todo
          PtypObject _ _ -> todo
          PtypVariant {} -> todo
          PtypPackage _ -> todo
          PtypExtension _ -> todo

instance FromOCaml CoreType (TermX () Variable) where
  fromOCaml t = fromOCaml (ptypDesc t) t

instance FromOCaml ConstructorArguments [TermX () Variable] where
  fromOCaml ocaml =
    case ocaml of
      PcstrTuple l -> map fromOCaml l
      PcstrRecord _ -> error "TODO"

instance
  FromOCaml
    ConstructorDeclaration
    (Inductive () Variable -> Constructor () Variable)
  where
  fromOCaml ConstructorDeclaration {..} ind =
    case pcdRes of
      Just _ -> error "TODO"
      Nothing ->
        Constructor
          { constructorInductive = ind,
            constructorName = mkVariable . txt $ pcdName,
            -- OCaml constructors are uncurried
            constructorParameters = [((), Binder Nothing, mkParameter pcdArgs)],
            constructorIndices = []
          }
        where
          mkParameter :: ConstructorArguments -> TermX () Variable
          mkParameter (PcstrTuple args) = mkApps (Var Nothing "tuple") (((), genericLength args) : map (const () &&& fromOCaml) args)
          mkParameter (PcstrRecord _) = error "TODO"

instance FromOCaml StructureItemDesc (StructureItem -> Vernacular () Variable) where
  fromOCaml ocaml =
    let todo = error $ "TODO: " ++ show ocaml
     in case ocaml of
          PstrType _ [TypeDeclaration {ptypeKind, ptypeName, ptypeParams}] ->
            case ptypeKind of
              PtypeAbstract -> error "TODO: PtypeAbstract"
              PtypeVariant cs ->
                let cs' = map fromOCaml cs
                 in let ind =
                          Inductive.Inductive.Inductive
                            { inductiveName = mkVariable . txt $ ptypeName,
                              inductiveParameters =
                                let fromParam (typ, _) =
                                      case fromOCaml typ of
                                        -- FIXME: Chick requires named parameters, OCaml does not
                                        Var _ v -> ((), "_param", Term.Var Nothing v)
                                        _ -> error "NO"
                                 in map fromParam ptypeParams,
                              inductiveIndices = [],
                              inductiveUniverse = Universe.Type,
                              inductiveConstructors = map ($ ind) cs'
                            }
                     in const $ Vernacular.Inductive ind
              _ -> todo
          PstrValue r [vb] ->
            case ppatDesc . pvbPat $ vb of
              PpatVar v ->
                const . Vernacular.Definition $
                  Definition.Definition
                    { definitionKind = fromOCaml r,
                      definitionName = mkVariable (txt v),
                      definitionType = Hole (),
                      definitionTerm = fromOCaml . pvbExpr $ vb
                    }
              _ -> todo
          _ -> todo

instance FromOCaml Expression (TermX () Variable) where
  fromOCaml ocaml = (fromOCaml . pexpDesc $ ocaml) ocaml

instance FromOCaml Longident (Maybe Variable) where
  fromOCaml ocaml = case ocaml of
    Lident i -> Just $ mkVariable i
    Ldot _ _ -> Nothing
    Lapply _ _ -> Nothing

instance FromOCaml ExpressionDesc (Expression -> TermX () Variable) where
  fromOCaml ocaml =
    let unsupported = Term.UnsupportedOCaml . UnsupportedExpression
     in case ocaml of
          PexpApply f as ->
            const $ mkApps (fromOCaml f) (map (const () *** fromOCaml) as)
          PexpConstruct ctor maybeArgsExpr ->
            case maybeArgsExpr of
              Nothing ->
                case fromOCaml ctor of
                  Nothing -> error "TODO"
                  Just ctor' -> const $ Var Nothing ctor'
              Just Expression {pexpDesc = argsExpr} ->
                case argsExpr of
                  PexpTuple args ->
                    case fromOCaml ctor of
                      Nothing -> error "TODO"
                      Just ctor' ->
                        -- OCaml constructors are uncurried
                        let argsAsTuple = mkApps (Var Nothing "tuple") (((), genericLength args) : map ((,) () . fromOCaml) args)
                         in const $ App () (Var Nothing ctor') argsAsTuple
                  _ -> error $ "\n\n" ++ show argsExpr
          PexpFun Nolabel Nothing patt expr ->
            case ppatDesc patt of
              PpatVar ls ->
                let v = mkVariable $ txt ls
                 in const $
                      Lam () $
                        abstractVariable v $
                          fromOCaml expr
              _ -> error $ show ocaml
          PexpFunction l ->
            let arg :: Variable = "__arg__"
             in const $
                  Lam () $
                    abstractVariable arg $
                      Match () (Var Nothing arg) $
                        map fromOCaml l
          PexpIdent i -> case fromOCaml $ txt i of
            Nothing -> unsupported
            Just v -> const $ Var Nothing v
          PexpLet NonRecursive [vb] e ->
            case ppatDesc $ pvbPat vb of
              PpatVar v ->
                let e1 = fromOCaml . pvbExpr $ vb
                 in let b = mkBinder $ txt v
                     in let e2 = fromOCaml e
                         in const $ Let () e1 (abstractBinder b e2)
              _ -> unsupported
          PexpMatch disc cases ->
            const $
              Match () (fromOCaml disc) $
                map fromOCaml cases
          _ -> error $ show ocaml

instance FromOCaml Case (Branch () Variable) where
  fromOCaml Case {..} =
    let (ctor, args) = fromOCaml pcLHS
     in let guard = fromOCaml <$> pcGuard
         in let body = fromOCaml pcRHS
             in packBranch (ctor, args, GuardAndBody guard body)

instance FromOCaml Pattern (Variable, [Binder Variable]) where
  fromOCaml Pattern {..} =
    case ppatDesc of
      PpatConstruct constr maybeArgsPattern ->
        let ctor = case fromOCaml constr of
              Nothing -> error "TODO"
              Just ctor' -> ctor'
         in let args = case maybeArgsPattern of
                  Nothing -> []
                  Just argsPattern ->
                    let Pattern {ppatDesc = argsPpatDesc} = argsPattern
                     in case argsPpatDesc of
                          PpatTuple args' ->
                            let patternToBinder Pattern {ppatDesc = innerPpatDesc} =
                                  case innerPpatDesc of
                                    PpatVar v -> Binder (Just $ mkVariable $ fromOCaml v)
                                    PpatAny -> Binder Nothing
                                    _ -> error "TODO"
                             in map patternToBinder args'
                          _ -> error "TODO"
             in (ctor, args)
      _ -> error "TODO"

instance FromOCaml a b => FromOCaml (Loc a) b where
  fromOCaml = fromOCaml . txt

instance FromOCaml String String where
  fromOCaml = id

_testProgram :: String
_testProgram =
  [s|
type 'a list =
  | Nil
  | Cons of ('a * 'a list)
|]

_test :: Either String (Script () Variable)
_test = Script . map fromOCaml <$> parseImplementation _testProgram

_prettyTest :: Either String String
_prettyTest = prettyStrU @'Chick <$> _test
