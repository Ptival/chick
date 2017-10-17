{-# language OverloadedStrings #-}
{-# language PartialTypeSignatures #-}

module StandardLibrary
  ( τId
  , τFlip
  , inductives
  , indAnd
  , indBool
  , indFalse
  , indEq
  , indFin
  , indList
  , indNat
  , indOr
  , indUnit
  , indVec
  , tId
  , tFlip
  ) where

-- import Control.Monad
-- import Control.Monad.Reader
-- import Data.Default

import Inductive.Inductive
import Parsing
import Parsing.Inductive
import Term.Raw                                  as Raw
import Term.Term
import Text.Megaparsec
import Text.Printf

-- main :: IO ()
-- main = forM_ (unGlobalEnvironment stdlib) $ \ d ->
--   putStrLn $ prettyStrU d

-- addTerm ::
--   Variable -> (Raw.Term Variable, Raw.Type Variable) ->
--   GlobalEnvironment (Checked Variable) Variable ->
--   Either String (GlobalEnvironment (Checked Variable) Variable)
-- addTerm v (t, τ) ge =
--   case tc (checkF (toLocalContext ge) t τ id) of
--   Left  e ->
--     Left $
--     printf "Could not typecheck %s : %s at type %s\n%s"
--     (prettyStr v) (prettyStrU t) (prettyStrU τ) (prettyStrU e)
--   Right r -> Right $ addGlobalAssum (Binder (Just v), r) ge
--
-- traceTypeChecking ::
--   GlobalEnvironment (Checked Variable) Variable ->
--   TermX α Variable -> TermX ψ Variable -> IO ()
-- traceTypeChecking ge t τ = do
--   let trace = tcTrace stepTypeCheckerF (checkF (toLocalContext ge) t τ id)
--   forM_ trace $ \ item -> do
--     putStrLn $ doc2String $ runReader (prettyTypeCheckerF item) def
--
-- debug :: IO ()
-- debug = traceTypeChecking (GlobalEnvironment []) tId τId

-- stdlib :: GlobalEnvironment (Checked Variable) Variable
-- stdlib =
--   fromRight $ foldM (flip ($)) (GlobalEnvironment [])
--   [ addTerm "id" (tId, τId)
--   , addTerm "flip" (τFlip, tFlip)
--   , addInductives
--     [ inductiveBool
--     , inductiveNat
--     , inductiveList
--     , inductiveFin
--     , inductiveVec
--     , inductiveEmpty
--     , inductiveUnit
--     ]
--   ]
--   where
--     fromRight (Left  l) = error l
--     fromRight (Right r) = r

-- do not use `unsafeParseRaw` anywhere else!
unsafeParseRaw :: String -> Raw.Term Variable
unsafeParseRaw s =
  case parseMaybeTerm s of
    Nothing -> error $ printf "unsafeParseRaw: could not parse %s" s
    Just t  -> t

unsafeParseInductive :: [String] -> Inductive Raw.Raw Variable
unsafeParseInductive ss =
  let s = unlines ss in
  case parseMaybe inductiveP s of
    Nothing -> error $ printf "unsafeParseInductive: could not parse\n%s" s
    Just t  -> t

τId, tId :: Raw.Term Variable
τId = unsafeParseRaw "(T : Type) → T → T"
tId = unsafeParseRaw "λ T x . x"

τFlip, tFlip :: Raw.Term Variable
τFlip = unsafeParseRaw
  "(A B C : Type) → (A → B → C) → (B → A → C)"
tFlip = unsafeParseRaw "λ A B C f b a . f a b"

indAnd :: Inductive Raw.Raw Variable
indAnd = unsafeParseInductive
  [ "Inductive and (A B : Prop) : Prop :="
  , "| conj : ∀ (a : A) (b : B), and A B"
  ]

indBool :: Inductive Raw.Raw Variable
indBool = unsafeParseInductive
  [ "Inductive bool : Set :="
  , "| true : bool"
  , "| false : bool"
  ]

indEq :: Inductive Raw.Raw Variable
indEq = unsafeParseInductive
  [ "Inductive eq (A : Type) (x : A) : ∀ (other : A), Prop :="
  , "| eq_refl : eq A x x"
  ]

indNat :: Inductive Raw.Raw Variable
indNat = unsafeParseInductive
  [ "Inductive nat : Set :="
  , "| O : nat"
  , "| S : nat → nat"
  ]

indOr :: Inductive Raw.Raw Variable
indOr = unsafeParseInductive
  [ "Inductive or (A B : Prop) : Prop :="
  , "| or_introl : ∀ (a : A), or A B"
  , "| or_intror : ∀ (b : B), or A B"
  ]

indList :: Inductive Raw.Raw Variable
indList = unsafeParseInductive
  [ "Inductive list (A : Type) : Type :="
  , "| nil : list A"
  , "| cons : ∀ (x : A) (xs : list A), list A"
  ]

indFin :: Inductive Raw.Raw Variable
indFin = unsafeParseInductive
  [ "Inductive Fin : ∀ (bound : nat), Set :="
  , "| fzero : ∀ (n : nat), Fin (S n)"
  , "| fsucc : ∀ (n : nat) (i : Fin n), Fin (S n)"
  ]

indVec :: Inductive Raw.Raw Variable
indVec = unsafeParseInductive
  [ "Inductive Vec (A : Type) : ∀ (size : nat), Type :="
  , "| vnil : Vec A O"
  , "| vcons : ∀ (h : A) (n : nat) (t : Vec A n), Fin (S n)"
  ]

indFalse :: Inductive Raw.Raw Variable
indFalse = unsafeParseInductive
  [ "Inductive False : Prop :="
  ]

indUnit :: Inductive Raw.Raw Variable
indUnit = unsafeParseInductive
  [ "Inductive unit : Set :="
  , "| tt : unit"
  ]

inductives :: [Inductive Raw.Raw Variable]
inductives =
  [ indAnd
  , indBool
  , indFalse
  , indEq
  , indNat
  , indOr
  , indList
  , indFin
  , indUnit
  , indVec
  ]
