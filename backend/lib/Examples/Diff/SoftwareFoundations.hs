module Examples.Diff.SoftwareFoundations
  ( scriptBefore
  , scriptAfter
  ) where

import           Parsing.Unsafe
import qualified Term.Raw as Raw
import           Term.Term
import           Script

commonPrefix :: String
commonPrefix = unlines
  [ "Inductive ty : Type :=   "
  , "  | TArrow : ty → ty → ty"
  , "  | TNat : ty            "
  , "  | TUnit : ty           "
  , "  | TProd : ty → ty → ty "
  , "  | TSum : ty → ty → ty  "
  , "  | TList : ty → ty.     "
  , "                         "
  , "Inductive id : Type :=.  "
  ]

codeBefore :: String
codeBefore =
  commonPrefix
  ++ unlines
  [ "Inductive tm : Type :=       "
  , "  (* pure STLC *)            "
  , "  | tvar : id → tm           "
  , "  | tapp : tm → tm → tm      "
  , "  | tabs : id → ty → tm → tm."
  ]
  ++ commonSuffix

codeAfter :: String
codeAfter =
  commonPrefix
  ++ unlines
  [ "Inductive tm : Type :=       "
  , "  (* pure STLC *)            "
  , "  | tvar : id → tm           "
  , "  | tapp : tm → tm → tm      "
  , "  | tabs : id → ty → tm → tm "
  , "  (* numbers *)              "
  , "  | tnat : nat → tm          "
  , "  | tsucc : tm → tm          "
  , "  | tpred : tm → tm          "
  , "  | tmult : tm → tm → tm     "
  , "  | tif0 : tm → tm → tm → tm."
  ]
  ++ commonSuffix

commonSuffix :: String
commonSuffix = unlines
  [ "Fixpoint subst : id → tm → tm → tm := λ x s t ,           "
  , "  match t with                                            "
  , "  | tvar y =>                                             "
  , "      if beq_id x y then s else t                         "
  , "  | tabs y T t1 =>                                        "
  , "      tabs y T (if beq_id x y then t1 else (subst x s t1))"
  , "  | tapp t1 t2 =>                                         "
  , "      tapp (subst x s t1) (subst x s t2)                  "
  , "  end.                                                    "
  ]

scriptAfter :: Script Raw.Raw Variable
scriptAfter = unsafeParseScript codeAfter

scriptBefore :: Script Raw.Raw Variable
scriptBefore = unsafeParseScript codeBefore
