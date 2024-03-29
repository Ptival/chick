{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module ToOCaml
  ( ToOCaml,
    guessAndPatch,
    testDiffGuess,
    theWholeThing,
  )
where

import Control.Arrow ((***))
import Control.Lens (view)
import Data.Char (isUpper)
import Data.Default (def)
import Data.Either.Utils (fromEither)
import Data.String.QQ (s)
import Definition
  ( Definition (definitionKind, definitionName, definitionTerm),
  )
import DefinitionObjectKind (DefinitionObjectKind (..))
import Diff.Guess.Script (guess)
import qualified Diff.Script as ΔS
import FromOCaml (FromOCaml (fromOCaml))
import Inductive.Inductive as Inductive
  ( Constructor (..),
    Inductive
      ( Inductive,
        inductiveConstructors,
        inductiveName,
        inductiveParameters
      ),
    ipType,
  )
import Language.OCaml.Definitions.Parsing.ASTHelper.Exp as Exp
  ( CaseOpts (guard),
    case',
  )
import qualified Language.OCaml.Definitions.Parsing.ASTHelper.Str as Str
import Language.OCaml.Definitions.Parsing.ASTHelper.Type as Type
  ( ConstructorOpts (args),
    MkOpts (kind, params),
    constructor,
    mk,
  )
import Language.OCaml.Definitions.Parsing.ASTHelper.Vb as Vb (mk)
import Language.OCaml.Definitions.Parsing.ASTTypes
  ( ArgLabel (Nolabel),
    Loc,
    RecFlag (..),
    Variance (Invariant),
  )
import Language.OCaml.Definitions.Parsing.ParseTree
  ( Case,
    ConstructorArguments (PcstrTuple),
    ConstructorDeclaration,
    CoreType,
    CoreTypeDesc (PtypConstr, PtypTuple, PtypVar),
    Expression,
    ExpressionDesc
      ( PexpApply,
        PexpConstruct,
        PexpFun,
        PexpIdent,
        PexpLet,
        PexpMatch,
        PexpTuple
      ),
    Longident (Lident),
    Pattern,
    PatternDesc (PpatAny, PpatConstruct, PpatTuple, PpatVar),
    Structure,
    StructureItem,
    StructureItemDesc (PstrType, PstrValue),
    TypeKind (PtypeVariant),
    none,
  )
import Language.OCaml.Parser (parseImplementation)
import Language.OCaml.Parser.Common
  ( mkLoc,
    mkexp,
    mkpat,
    mkstrExt,
    mktyp,
  )
import Language.OCaml.PrettyPrinter (structurePP)
import Polysemy (Member, Sem, run, runM)
import Polysemy.Error (runError)
import Polysemy.Trace (Trace, ignoreTrace, trace, traceToStdout)
import PrettyPrinting.Chick ()
import Repair.Script (runRepair')
import Script (Script (..))
import Term.Term
  ( Binder (..),
    Branch,
    GuardAndBody (GuardAndBody, branchBody, branchGuard),
    TermX (App, Lam, Let, Match, Type, Var),
    TypeX,
    Variable (..),
    unpackBranch,
    unscopeTerm,
  )
import Utils (extractApps)
import Vernacular (Vernacular (..))

class ToOCaml a b where
  toOCaml :: a -> b

instance ToOCaml (Constructor () Variable) ConstructorDeclaration where
  toOCaml Constructor {..} =
    let args = case constructorParameters of
          [(_, _, tuple)] ->
            let (fun, args') = run $ extractApps tuple
             in case fun of
                  Var _ v | unVariable v == "tuple" -> case args' of
                    [] -> error "TODO"
                    _arity : actualArgs -> PcstrTuple $ map (toOCaml . snd) actualArgs
                  _ -> error "TODO"
          _ -> error "TODO"
     in -- let args = PcstrTuple $ map (\ (_, _, t) -> toOCaml t) constructorParameters in
        let res = Nothing
         in Type.constructor
              ( def
                  { args
                  }
              )
              res
              -- name
              (mkLoc (unVariable constructorName) none)

instance ToOCaml (Vernacular () Variable) StructureItem where
  toOCaml = \case
    Vernacular.Definition d ->
      let r = toOCaml $ definitionKind d
       in let pat = mkpat $ PpatVar $ toOCaml $ definitionName d
           in let expr = toOCaml $ definitionTerm d
               in let vb = Vb.mk def pat expr
                   in Str.mk def $ PstrValue r [vb]
    Vernacular.Inductive
      Inductive.Inductive
        { inductiveConstructors,
          inductiveName,
          inductiveParameters
        } ->
        -- so, OCaml generates a PtypeAbstract for empty types
        -- I'll try a PtypeVariant with no constructor instead, until it breaks!
        let kind = PtypeVariant $ map toOCaml inductiveConstructors
         in let params = map ((,Invariant) . toOCaml . view ipType) inductiveParameters
             in let t =
                      Type.mk
                        ( def
                            { kind,
                              params
                            }
                        )
                        -- manifest
                        Nothing
                        -- name
                        (mkLoc (unVariable inductiveName) none)
                 in mkstrExt (PstrType NonRecursive [t]) Nothing
    Vernacular.UnsupportedOCaml o -> o

--mkStr Nothing

instance ToOCaml DefinitionObjectKind RecFlag where
  toOCaml = \case
    Fixpoint -> Recursive
    DefinitionObjectKind.Definition -> NonRecursive

instance ToOCaml (StructureItem -> Vernacular () Variable) StructureItemDesc where
  toOCaml = error "TODO"

-- Note: the two next instances are overlapping in their first argument

instance ToOCaml (TypeX () Variable) CoreType where
  toOCaml chick = case chick of
    App {} ->
      let (fun, args) = run $ extractApps chick
       in case fun of
            Var _ v -> case unVariable v of
              "prod" -> mktyp $ PtypTuple $ map (toOCaml . snd) args
              "tuple" -> mktyp $ PtypConstr (mkLoc (Lident (unVariable v)) none) $ map (toOCaml . snd) args
              _ -> mktyp $ PtypConstr (mkLoc (Lident (unVariable v)) none) $ map (toOCaml . snd) args
            _ -> error "TODO"
    Type {} -> mktyp . PtypVar $ "FIXME: Type?"
    Var _ v -> mktyp . PtypVar $ unVariable v
    _ -> error $ "TODO: " ++ show chick

instance ToOCaml (TermX () Variable) Expression where
  toOCaml chick = case chick of
    App {} ->
      let (fun, args) = run $ extractApps chick
       in case fun of
            Var _ v ->
              case unVariable v of
                "" -> error "TODO"
                -- In OCaml, a constructor call is a different constructor from a function call
                c : _
                  | isUpper c ->
                    let args' = case args of
                          [] -> Nothing
                          [(_, argsAsTuple)] -> Just $ toOCaml argsAsTuple --Just $ mkexp $ PexpTuple $ map (toOCaml . snd) args
                          _ -> error "TODO"
                     in mkexp $ PexpConstruct (toOCaml v) args'
                "prod" -> error "prod"
                "tuple" ->
                  case args of
                    _ : rest -> mkexp $ PexpTuple $ map (toOCaml . snd) rest
                    [] -> error "TODO"
                _ ->
                  let mkArgs = (const Nolabel *** toOCaml)
                   in mkexp $ PexpApply (toOCaml fun) (map mkArgs args)
            _ ->
              let mkArgs = (const Nolabel *** toOCaml)
               in mkexp $ PexpApply (toOCaml fun) (map mkArgs args)
    Lam _ sBody ->
      let (binder, body) = unscopeTerm sBody
       in let binder' = maybe "_" unVariable (unBinder binder)
           in let patt = mkpat $ PpatVar (mkLoc binder' none)
               in let body' = toOCaml body
                   in mkexp $ PexpFun Nolabel Nothing patt body'
    Let _ e1 be2 ->
      let (b, e2) = unscopeTerm be2
       in let pat = toOCaml b
           in let expr = toOCaml e1
               in let vb = Vb.mk def pat expr
                   in let e = toOCaml e2
                       in mkexp $ PexpLet NonRecursive [vb] e
    Match _ discriminee branches ->
      let discriminee' = toOCaml discriminee
       in let branches' = map toOCaml branches
           in mkexp $ PexpMatch discriminee' branches'
    Var _ v -> mkexp $ PexpIdent $ mkLoc (Lident (unVariable v)) none
    _ -> error $ show chick

instance ToOCaml Variable (Loc String) where
  toOCaml v = mkLoc (unVariable v) none

instance ToOCaml (Binder Variable) Pattern where
  toOCaml (Binder Nothing) = mkpat PpatAny
  toOCaml (Binder (Just v)) = mkpat . PpatVar $ toOCaml v

instance ToOCaml (Branch () Variable) Case where
  toOCaml b =
    let (ctor, args, GuardAndBody {branchGuard, branchBody}) = unpackBranch b
     in let guard' = case branchGuard of
              Nothing -> guard def
              Just guard -> Just $ toOCaml guard
         in let args' = case args of
                  [] -> Nothing
                  _ -> Just $ mkpat $ PpatTuple $ map toOCaml args
             in let patt = mkpat $ PpatConstruct (toOCaml ctor) args'
                 in Exp.case' (def {guard = guard'}) patt (toOCaml branchBody)

instance ToOCaml Variable (Loc Longident) where
  toOCaml v = mkLoc (Lident $ unVariable v) none

--instance ToOCaml (Maybe Variable) Longident where

--instance ToOCaml (Expression -> TermX () Variable) Expression_desc where

_fromProgram :: String
_fromProgram =
  [s|
type 'a list =
  | Nil
  | Cons of ('a * 'a list)

let rec map f l =
  match l with
  | Nil -> Nil
  | Cons(h, t) -> Cons(f h, map f t)
|]

_toProgram :: String
_toProgram =
  [s|
type 'a list =
  | Nihilism
  | Cons of ('a * 'a list)

let rec map f l =
  match l with
  | Nil -> Nil
  | Cons(h, t) -> Cons(f h, map f t)
|]

roundtrip0 :: String -> Either String Structure
roundtrip0 = parseImplementation

roundtrip1 :: String -> Either String [Vernacular () Variable]
roundtrip1 input = map fromOCaml <$> roundtrip0 input

roundtrip2 :: String -> Either String Structure
roundtrip2 input = map toOCaml <$> roundtrip1 input

roundtrip :: String -> String
roundtrip input = fromEither $ show . structurePP <$> roundtrip2 input

_from :: Either String [Vernacular () Variable]
_from = roundtrip1 _fromProgram

_to :: Either String Structure
_to = roundtrip2 _fromProgram

_prettyTest :: String
_prettyTest = roundtrip _fromProgram

_main :: IO ()
_main = putStrLn _prettyTest

_test1 :: Either String Structure
_test1 = parseImplementation "let f x = y"

_test2 :: Either String Structure
_test2 = parseImplementation "let f = fun x -> y"

testDiffGuess :: IO (ΔS.Diff ())
testDiffGuess =
  let p1 = case roundtrip1 _fromProgram of
        Left e -> error e
        Right r -> r
   in let p2 = case roundtrip1 _toProgram of
            Left e -> error e
            Right r -> r
       in runM . ignoreTrace . ignoreTrace $ guess (Script p1) (Script p2)

unsafeParseOCaml :: String -> Script () Variable
unsafeParseOCaml o = case map fromOCaml <$> parseImplementation o of
  Left e -> error e
  Right r -> Script r

guessAndPatch ::
  ( Member Trace r
  ) =>
  Script () Variable ->
  Script () Variable ->
  Sem r (Either String (Script () Variable))
guessAndPatch p1 p2 = do
  δ <- guess p1 p2
  trace "\n\n=== DIFF ===\n\n"
  trace $ show δ
  trace "\n\n=== DONE WITH GUESS, NOW PATCHING ===\n\n"
  runError $ ΔS.patch p1 δ

unsafeChickToOCaml :: Script () Variable -> Structure
unsafeChickToOCaml = map toOCaml . unScript

theWholeThing :: String -> String -> IO ()
theWholeThing ocaml1 ocaml2 = do
  let p1 = unsafeParseOCaml ocaml1
  let p2 = unsafeParseOCaml ocaml2
  δ <- runM . traceToStdout $ guess p1 p2
  ep3 <- runM . traceToStdout $ runRepair' p1 δ
  case ep3 of
    Left e -> error e
    Right p3 -> print . structurePP $ unsafeChickToOCaml p3
