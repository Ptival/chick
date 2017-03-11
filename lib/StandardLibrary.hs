{-# language OverloadedStrings #-}

module StandardLibrary where

import Control.Monad
--import Data.Maybe

--import Notations

import Context
import Inductive.Constructor
import Inductive.Inductive
import Parsing
import PrettyPrinting
import Term.Raw              as Raw
import Term.Term
import Text.Printf
import Work

{-
checkStdLibInductives :: (MonadState TypeCheckedContext m, MonadIO m) => m ()
checkStdLibInductives = do
  forM_ stdlibInductives $ \ i -> do
    ctxt <- get
    case runStateT (checkInductive i) ctxt of
      Left  l -> liftIO $ do
        putStrLn
          $ printf "Failed to check %s:" (prettyVariable (Inductive.name i))
        putStrLn l
      Right (_, ctxt') -> do
        put $ ctxt'
        liftIO . putStrLn
          $ printf "Success check %s" (prettyVariable (Inductive.name i))

doCheck :: IO ()
doCheck = do
  (_, ctxt) <- runStateT checkStdLibInductives []
  forM_ (reverse ctxt) $ \ (v, τ) -> do
    putStrLn $ printf "%s : %s" (prettyVariable v) (prettyTerm τ)
-}

addTerm :: Variable -> (Raw.Term, Raw.Type) -> TypeCheckedContext -> Either String TypeCheckedContext
addTerm v (t, τ) ctxt =
  case tc (checkF ctxt t τ id) of
  Left  _ ->
    Left $
    printf "Could not typecheck %s at type %s"
    (prettyTerm t) (prettyTerm τ)
  Right r -> Right $ (v, r) : ctxt

stdlib :: TypeCheckedContext
stdlib =
  fromRight $ foldM (flip ($)) []
  [ addTerm "id" (tId, τId)
  , addTerm "flip" (τFlip, tFlip)
  , addInductives
    [ inductiveBool
    , inductiveNat
    , inductiveList
    , inductiveFin
    , inductiveVec
    , inductiveEmpty
    , inductiveUnit
    ]
  ]
  where
    fromRight (Left  l) = error l
    fromRight (Right r) = r

-- do not use `unsafeParseRaw` anywhere else!
unsafeParseRaw :: String -> Raw.Term
unsafeParseRaw s =
  case parseMaybeTerm s of
    Nothing -> error $ printf "unsafeParseRaw: could not parse %s" s
    Just t  -> t

τId, tId :: Raw.Term
τId = unsafeParseRaw "(T : Type) → T → T"
tId = unsafeParseRaw "λ T x . x"

τFlip, tFlip :: Raw.Term
τFlip = unsafeParseRaw
  "(A B C : Type) → (A → B → C) → (B → A → C)"
tFlip = unsafeParseRaw "λ A B C f b a . f a b"

{-
inductive Bool : Type where
  true  : Bool
  false : Bool
-}
inductiveBool :: Inductive ξ
inductiveBool =
  Inductive "𝔹" [] []
  [ Constructor "true"  [] []
  , Constructor "false" [] []
  ]

{-
inductive ℕ : Type where
  zero : ℕ
  succ : (n : ℕ) → ℕ
-}
inductiveNat :: Inductive Raw
inductiveNat =
  Inductive "ℕ" [] []
  [ Constructor "zero"  [] []
  , Constructor "succ" [(Binder (Just "n"), Var () "ℕ")] []
  ]

{-
inductive List (A : Type) : Type where
  nil  :                         List A
  cons : (x : A) (xs : List A) → List A
-}
inductiveList :: Inductive Raw
inductiveList =
  Inductive "List" [("A", Type ())] []
  [ Constructor "nil"  [] []
  , Constructor "cons"
    [ (Binder (Just "x"), Var () "A")
    , (Binder (Just "xs"), App () (Var () "List") (Var () "A"))
    ]
    []
  ]

{-
inductive Fin : ℕ → Type where
  zero : {n : ℕ} → Fin (suc n)
  suc  : {n : ℕ} (i : Fin n) → Fin (suc n)
-}
inductiveFin :: Inductive Raw
inductiveFin =
  Inductive "Fin" [] [Var () "ℕ"]
  [ Constructor "zero"
    [ (Binder (Just "n"), Var () "ℕ") ]
    [ App () (Var () "succ") (Var () "n") ]
  , Constructor "succ"
    [ (Binder (Just "n"), Var () "ℕ")
    , (Binder (Just "i"), App () (Var () "Fin") (Var () "n"))
    ]
    [ App () (Var () "succ") (Var () "n") ]
  ]

{-
inductive Vec (A : Type) : ℕ → Type where
  nil  : Vec A zero
  cons : {n : ℕ} → (x : A) (xs : Vec A n) → Vec A (suc n)
-}
inductiveVec :: Inductive Raw
inductiveVec =
  Inductive "Vec" [("A", Type ())] [Var () "ℕ"]
  [ Constructor "nil"  [] [Var () "zero"]
  , Constructor "cons"
    [ (Binder (Just "n"), Var () "ℕ")
    , (Binder (Just "x"), Var () "A")
    , (Binder (Just "xs"), App () (App () (Var () "Vec") (Var () "A")) (Var () "n"))
    ]
    [ App () (Var () "succ") (Var () "n") ]
  ]

{-
inductive ⊥ : Set where
-}
inductiveEmpty :: Inductive Raw
inductiveEmpty =
  Inductive "⊥" [] [] []

{-
inductive ⊤ : Set where
  tt : ⊤
-}
inductiveUnit :: Inductive Raw
inductiveUnit =
  Inductive "⊤" [] [] [Constructor "tt" [] []]
