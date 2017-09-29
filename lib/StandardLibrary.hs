{-# language OverloadedStrings #-}
{-# language PartialTypeSignatures #-}

module StandardLibrary
  ( τId
  , τFlip
  , inductives
  , indBool
  , indEmpty
  , indFin
  , indList
  , indNat
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

indBool :: Inductive Raw.Raw Variable
indBool = unsafeParseInductive
  [ "Inductive bool : Type :="
  , "| true : bool"
  , "| false : bool"
  , "."
  ]

indNat :: Inductive Raw.Raw Variable
indNat = unsafeParseInductive
  [ "Inductive nat : Type :="
  , "| O : nat"
  , "| S : nat → nat"
  , "."
  ]

indList :: Inductive Raw.Raw Variable
indList = unsafeParseInductive
  [ "Inductive List (A : Type) : Type :="
  , "| nil : List A"
  , "| cons : ∀ (x : A) (xs : List A), List A"
  , "."
  ]

indFin :: Inductive Raw.Raw Variable
indFin = unsafeParseInductive
  [ "Inductive Fin : ∀ (bound : nat), Type :="
  , "| fzero : ∀ (n : nat), Fin (S n)"
  , "| fsucc : ∀ (n : nat) (i : Fin n), Fin (S n)"
  , "."
  ]

indVec :: Inductive Raw.Raw Variable
indVec = unsafeParseInductive
  [ "Inductive Vec (A : Type) : ∀ (size : nat), Type :="
  , "| vnil : Vec A O"
  , "| vcons : ∀ (h : A) (n : nat) (t : Vec A n), Fin (S n)"
  , "."
  ]

indEmpty :: Inductive Raw.Raw Variable
indEmpty = unsafeParseInductive
  [ "Inductive False : Type :="
  , "."
  ]

indUnit :: Inductive Raw.Raw Variable
indUnit = unsafeParseInductive
  [ "Inductive unit : Type :="
  , "| tt : unit"
  , "."
  ]

inductives :: [Inductive Raw.Raw Variable]
inductives =
  [ indBool
  , indNat
  , indList
  , indFin
  , indVec
  , indEmpty
  , indUnit
  ]
