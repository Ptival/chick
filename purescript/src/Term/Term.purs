module Term.Term where

import Prelude
import Bound.Bound ((>>>=))
import Bound.Name (Name(Name), abstract1Name, abstractName)
import Bound.Scope (Scope, bindings, instantiate)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array (fold, head, reverse, uncons, (:))
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Tuple (Tuple, lookup)
import Parsing.Precedence (Precedence(..), PrecedenceTable, Tolerance(..), par)
import PrettyPrint.PrettyPrint (Doc, char, fillSep, parens, text)
import PrettyPrint.PrettyPrintable (prettyDoc)
import PrettyPrint.PrettyPrintable1 (class PrettyPrintable1, prettyStr1Default)
import Term.Binder (Binder(..), unBinder)
import Term.Variable (Variable(..), unVariable)

arrowSymbol :: String
arrowSymbol = "→"

holeSymbol :: String
holeSymbol = "_"

lambdaSymbol :: String
lambdaSymbol = "λ"

lambdaBodySymbol :: String
lambdaBodySymbol = "."

type NameScope = Scope (Name Variable Unit)

data TermX α ν
  = App  α (TermX α ν) (TermX α ν)
  | Hole α
  | Lam  α             (NameScope (TermX α) ν)
  | Let  α (TermX α ν) (NameScope (TermX α) ν)
  | Pi   α (TypeX α ν) (NameScope (TypeX α) ν)
  | Type
  | Var   ν

type TypeX = TermX

derive instance functorTerm :: Functor (TermX α)

instance showTerm :: Show ν => Show (TermX α ν) where
  show = case _ of
    App  _ t1 t2 -> fold ["App (", show t1, ") (", show t2, ")"]
    Hole _       -> "Hole"
    Lam  _ t     -> fold ["Lam (", show t, ")"]
    Let  _ t1 t2 -> fold ["Let (", show t1, ") (", show t2, ")"]
    Pi   _ τ1 τ2 -> fold ["Pi  (", show τ1, ") (", show τ2, ")"]
    Type         -> "Type"
    Var  v       -> fold ["Var ", show v]

data TermOrScope α ν
  = ToS_Term (TermX α ν)
  | ToS_Scope (Scope (Name Variable Unit) (TermX α) ν)

instance foldableTerm :: Foldable (TermX α) where
  foldMap f term0 = tailRec go { acc : mempty, next : ToS_Term term0, rest : [] }
    where
      go { acc, next, rest } =
        case next of
          ToS_Term term ->
            case term of
              App  _ t1 t2 -> Loop { acc, next : ToS_Term t1, rest : ToS_Term t2 : rest }
              Hole _       -> goRest { acc, rest }
              Lam  _ t     -> Loop { acc, next : ToS_Scope t, rest }
              Let  _ t1 t2 -> Loop { acc, next : ToS_Term t1, rest : ToS_Scope t2 : rest }
              Pi   _ τ1 τ2 -> Loop { acc, next : ToS_Term τ1, rest : ToS_Scope τ2 : rest }
              Type         -> goRest { acc, rest }
              Var  v       ->
                let fv = f v in
                let acc' = acc <> fv in
                goRest { acc : acc', rest }
          ToS_Scope scope -> goRest { acc : acc <> foldMap f scope, rest }
      goRest { acc, rest } = case uncons rest of
        Nothing             -> Done $ acc
        Just { head, tail } -> Loop $ { acc, next : head, rest : tail }
  foldl f = foldlDefault f
  foldr f = foldrDefault f

instance applyTerm :: Apply (TermX α) where
  apply ft x = case ft of
    App  a t1 t2  -> App  a (apply t1 x) (apply t2 x)
    Hole a        -> Hole a
    Lam  a bt     -> Lam a $ bt >>>= (_ <$> x)
    Let  a t1 bt2 -> Let a (apply t1 x) $ bt2 >>>= (_ <$> x)
    Pi   a τ1 bτ2 -> Pi  a (apply τ1 x) $ bτ2 >>>= (_ <$> x)
    Type          -> Type
    Var v         -> v <$> x

instance applicativeTerm :: Applicative (TermX α) where
  pure = Var

instance bindTerm :: Bind (TermX α) where
  bind (App   a t1 t2 ) f = App   a (t1 >>= f) (t2 >>= f)
  bind (Hole  a       ) _ = Hole  a
  bind (Lam   a bt    ) f = Lam   a            (bt  >>>= f)
  bind (Let   a t1 bt2) f = Let   a (t1 >>= f) (bt2 >>>= f)
  bind (Pi    a τ1 bτ2) f = Pi    a (τ1 >>= f) (bτ2 >>>= f)
  bind (Type          ) _ = Type
  bind (Var   v       ) f = f v

instance monadTerm :: Monad (TermX α)

substitute :: ∀ a f. Eq a => Monad f => a -> f a -> f a -> f a
substitute v t1 t2 = t2 >>= \ b -> if b == v then t1 else pure b

simultaneousSubstitute :: ∀ a f. Eq a => Monad f => Array (Tuple a (f a)) -> f a -> f a
simultaneousSubstitute l w =
  w >>= \ b -> case lookup b l of
                Just p -> p
                Nothing -> pure b

abstractAnonymous :: ∀ f ν. Monad f => f ν -> Scope (Name ν Unit) f ν
abstractAnonymous = abstractName (const Nothing)

abstractBinder :: ∀ f ν. Monad f => Eq ν => Binder ν -> f ν -> Scope (Name ν Unit) f ν
abstractBinder b =
  case unBinder b of
    Nothing -> abstractAnonymous
    Just v  -> abstract1Name v

getName :: ∀ f a. Foldable f => Scope (Name Variable Unit) f a -> Variable
getName bt = case head $ bindings bt of
  Nothing         -> Variable "_"
  Just (Name v _) -> v

unscopeTerm ::
  ∀ α.
  Scope (Name Variable Unit) (TermX α) Variable ->
  { binder :: Binder Variable
  , term :: TermX α Variable
  }
unscopeTerm t =
  let n = getName t in
  let b = if unVariable n == "_" then Nothing else Just n in
  { binder : Binder b
  , term   : instantiate (\ (Name v _) -> Var v) t
  }

instantiateTerm :: ∀ α ν. Scope (Name ν Unit) (TermX α) ν -> TermX α ν
instantiateTerm s = instantiate (\ (Name v _) -> Var v) s

prettyTermDocPrec ::
  ∀ α a.
  PrecedenceTable -> TermX α Variable ->
  { doc :: Doc a, precedence :: Precedence }
prettyTermDocPrec precs = goTerm

  where

    go :: { precedence :: Precedence, tolerance :: Tolerance } -> TermX α Variable -> Doc a
    go pt = par precs pt <<< goTerm

    goTerm :: TermX α Variable -> { doc :: Doc a, precedence :: Precedence }
    goTerm = case _ of

      -- Annot _ t τ ->
      --   (fillSep
      --    [ go (PrecAnnot, TolerateHigher) t
      --    , text annotSymbol
      --    , go (PrecAnnot, TolerateHigher) τ
      --    ]
      --   , PrecAnnot)

      App _ t1 t2 ->
        { doc : fillSep [ go { precedence : PrecApp, tolerance : TolerateEqual  } t1
                        , go { precedence : PrecApp, tolerance : TolerateHigher } t2
                        ]
        , precedence : PrecApp
        }

      Hole _ ->
        { doc        : text holeSymbol
        , precedence : PrecAtom
        }

      l@(Lam _ _) ->
        { doc        : goLams [] l
        , precedence : PrecLam
        }

      Let _ t1 bt2 ->
        let t2 = unscopeTerm bt2 in
        { doc : fillSep [ text "let"
                        , prettyDoc $ t2.binder
                        , char '='
                        , go { precedence : PrecMin
                             , tolerance : TolerateEqual
                             }
                          t1
                        , text "in"
                        , go { precedence : PrecLet
                             , tolerance : TolerateEqual
                             }
                          $ t2.term
                        ]
        , precedence : PrecLet
        }

      Pi _ τ1 bτ2 ->
        let τ2 = unscopeTerm bτ2 in
        case unBinder $ τ2.binder of
          Nothing ->
            { doc : fillSep [ go { precedence : PrecArrow
                                 , tolerance : TolerateHigher
                                 }
                              τ1
                            , char '→'
                            , go { precedence : PrecArrow
                                 , tolerance : TolerateEqual
                                 }
                              $ τ2.term
                            ]
            , precedence : PrecArrow
            }
          Just v ->
            { doc : fillSep [ parens $ fillSep
                              [ prettyDoc τ2.binder
                              , char ':'
                              , go { precedence : PrecMin
                                   , tolerance  : TolerateEqual
                                   }
                                τ1
                              ]
                            , char '→'
                            , go { precedence : PrecArrow
                                 , tolerance  : TolerateEqual
                                 }
                              $ τ2.term
                            ]
            , precedence : PrecArrow
            }

      Type ->
        { doc        : text "Type"
        , precedence : PrecAtom
        }

      Var v ->
        { doc        : prettyDoc v
        , precedence : PrecAtom
        }

    goLams :: Array (Doc a) -> TermX α Variable -> Doc a
    goLams l = case _ of
      Lam _ bt ->
        let t = unscopeTerm bt in
        goLams (prettyDoc t.binder : l) t.term
      t -> fillSep $ [ char 'λ'
                     , fillSep <<< reverse $ l
                     , char '.'
                     , go { precedence : PrecMin
                          , tolerance  : TolerateEqual
                          }
                       t
                     ]

instance prettyPrintable1Term :: PrettyPrintable1 (TermX ξ) where
  prettyDoc1 t = do
    precs <- ask
    pure $ par precs { precedence : PrecMin, tolerance : TolerateEqual } <<< prettyTermDocPrec precs $ t
  prettyStr1 t = prettyStr1Default t
