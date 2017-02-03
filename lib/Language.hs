{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language DeriveGeneric #-}
{-# language EmptyCase #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Language where

{-
--import Control.Monad.Cont
--import Data.Constraint    (Constraint)
--import Data.Default
import Data.Typeable
--import Data.Void
import GHC.Generics
import GHC.Exts          (Constraint)
--import Text.Printf        (printf)

type Name = String

type family X_Type ξ
type family X_Pi   ξ
type family X_Var  ξ
type family X_App  ξ

data TermX ξ
  = Type  (X_Type ξ)
  | Pi    (X_Pi ξ)   (Maybe Name) (TypeX ξ) (TermX ξ)
  | Var   (X_Var ξ)  Name
--  | Lam   a (Maybe Name) (Term a)
  | App   (X_App ξ)  (TermX ξ) (TermX ξ)
--  | Let   a (Maybe Name) (Term a) (Term a)
--  | Annot a (Term a)     (Type a)
--  | Hole  a
  deriving (Generic, Typeable)

type TypeX = TermX

type ForallX (φ :: * -> Constraint) ξ =
  (φ (X_Type ξ), φ (X_Pi ξ), φ (X_Var ξ), φ (X_App ξ))

deriving instance ForallX Show ξ => Show (TermX ξ)

-- Instance

data Raw
type RawTerm = TermX Raw
type RawType = RawTerm
type instance X_Type Raw = ()
type instance X_Pi   Raw = ()
type instance X_Var  Raw = ()
type instance X_App  Raw = ()

type TypeContext ξ = [(Name, TypeX ξ)]

{-
data Task
  = Synth (TypeContext Raw) RawTerm
  | Check (TypeContext Raw) RawTerm RawType

instance Show Task where
  show (Synth _ t) = printf "(Synth %s)" (show t)
  show (Check _ t1 t2) = printf "(Check %s %s)" (show t1) (show t2)
-}

data TypeAnnotated
type TypeAnnotatedTerm = TermX TypeAnnotated
type TypeAnnotatedType = TypeAnnotatedTerm
type TypeAnnotation = Either String (Maybe (TypeContext Raw), RawType)
type instance X_Type TypeAnnotated = TypeAnnotation
type instance X_Pi   TypeAnnotated = TypeAnnotation
type instance X_Var  TypeAnnotated = TypeAnnotation
type instance X_App  TypeAnnotated = TypeAnnotation

type TCResult = TypeAnnotatedTerm
--TermX (Either String (Maybe (TypeContext ()), TypeX ()))

{-
data Worky t r
  = Todoy t (t -> Worky t r)
  | Doney r

instance Functor (Worky t) where
  fmap f (Todoy t k) = Todoy t (\ tr -> fmap f (k tr))
  fmap f (Doney r)   = Doney (f r)

instance Applicative (Worky t) where
  pure = Doney
  Todoy ft fk <*> x =
    case fk ft of
    Todoy ft' fk' -> Todoy ft' fk' <*> x
    Doney fr -> fr <$> x
  Doney f <*> x = f <$> x

instance Monad (Worky t) where
  Todoy t k >>= kk =
    case k t of
    Todoy t' k' ->
      Todoy t' (\ t'' -> k' t'' >>= kk)
    Doney r -> kk r
  Doney r >>= kk = kk r

foo :: Worky Int Int
foo = Todoy 40 (\ x -> Doney (x + 2))

bar :: Worky Int Int
bar = do
  x <- Todoy 1 (\ x -> Todoy (x * 10) Doney)
  y <- Todoy 2 Doney
  z <- Todoy 3 Doney
  return (x + y + z)

workIt :: Worky t r -> r
workIt (Doney r) = r
workIt (Todoy v k) = workIt (k v)

workIt' :: (Show t, Show r) => Worky t r -> IO r
workIt' (Doney r) = do
  putStrLn ("Doney: " ++ show r)
  return r
workIt' (Todoy v k) = do
  putStrLn ("Todoy: " ++ show v)
  workIt' (k v)

data Work t tr r
  = Todo t (tr -> Work t tr r)
  | Done r

instance (Show t, Show r) => Show (Work t tr r) where
  show (Todo t _) = printf "(Todo %s)" (show t)
  show (Done r)   = printf "(Done %s)" (show r)

instance Functor (Work t tr) where
  fmap f (Todo t k) = Todo t (\ tr -> fmap f (k tr))
  fmap f (Done r)   = Done (f r)

allTheWay :: (t -> Work t tr r) -> Work t tr r -> r
allTheWay step (Todo t k) = allTheWay step (step t)
allTheWay step (Done r)   = r
-}

{-
instance Applicative (Work t tr) where
  pure x = Done x
  --Todo ft fk <*> Todo xt xk = Todo ft (\ ftr -> Todo xt (\ xtr -> _))
  Todo t k <*> Done r = _
  Done f <*> w = f <$> w
-}

{-
type TCWork = Work Task TaskResult TCResult

-- within a better type system, I'd have Success be Synthesized or Checked
data TaskResult
  = Success RawType TCResult
  | Failure TCResult
  deriving (Show)

set :: Default (X_Type ξ) => TypeX ξ
set = Type def

eqβ :: TermX ξ -> TermX ξ -> Bool
eqβ _ _ = True
--a `eqβ` b = (redβ a) `eqα` (redβ b)

errorExpectedType :: ForallX Show ξ => TermX ξ -> TermX ξ -> String
errorExpectedType actual expected =
  printf
  "This term has type:\n%s\nbut a term of type:\n%s\nis expected"
  (show actual)
  (show expected) --(redβ τ))

typeAnnotateHeadWith :: TypeAnnotation -> TypeAnnotatedTerm -> TypeAnnotatedTerm
typeAnnotateHeadWith a t = case t of
  Type _ -> Type a
  --Pi _ n τ1 τ2 -> Pi a n τ1 τ2
  Var _ x -> Var a x
  --Lam _ n t -> Lam a n t
  App _ t1 t2 -> App a t1 t2
  --Let _ n t1 t2 -> Let a n t1 t2
  --Annot _ t τ -> Annot a t τ
  --Hole _ -> Hole a

typeCheck :: Task -> Cont r TaskResult

-- conversion rule
typeCheck (Check γ t τ) = do
  tcSynth <- callCC (\ k -> k $ typeCheck (Synth γ t))
  res <- tcSynth
  case res of
    Success τ' t' ->
      return $
      if τ `eqβ` τ'
      then Success τ t'
      else
        Failure $ typeAnnotateHeadWith
        (Left $ printf
         "This term has type:\n%s\nbut a term of type:\n%s\nis expected"
         (show τ') --(pprint τ')
         (show τ) -- (pprint (redβ τ))
        )
        t'
    Failure t' ->
      return $ Failure t'

{-
-- conversion rule
typeCheck (Check γ t τ) = do
  res1 <- typeCheck (Synth γ t)
  case res1 of
    Success τ' t' ->
      return $
      if τ `eqβ` τ'
      then Success τ t'
      else
        Failure $ typeAnnotateHeadWith
        (Left $ printf
         "This term has type:\n%s\nbut a term of type:\n%s\nis expected"
         (show τ') --(pprint τ')
         (show τ) -- (pprint (redβ τ))
        )
        t'
    Failure t' ->
      return $ Failure t'
-}

typeCheck (Synth γ (Var _ n)) = do
  case lookup n γ of -- (γ ++ stdlib) of
    Just τ -> return $ Success τ (Var (Right (Nothing, τ)) n)
    Nothing ->
      return $ Failure $ Var (Left "Undefined reference") n

typeCheck (Synth _ (Type _)) = do
  return $ Success set (Type (Right (Nothing, set)))

typeCheck _ = error "TODO"

typeCheckShowSteps :: TaskResult
typeCheckShowSteps =
  runCont (typeCheck (Synth [] term1)) id

term1 :: RawTerm
term1 = set
-}

-}
