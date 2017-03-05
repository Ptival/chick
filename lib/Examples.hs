{-# language DeriveAnyClass #-}
{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language UndecidableInstances #-}

module Examples where

import Control.Monad.Trans.Free
import Text.Megaparsec
import Text.PrettyPrint.GenericPretty (pp)

import Notations
import Parsing
import PrettyPrinting
import Term.RawTerm
import Term.AlphaEquivalence
import Term.NumberedTerm
import Term.TypeCheckedTerm
import Term.TypeErroredTerm
import Work

τFlip :: RawType
τFlip = π [ ("A", type'), ("B", type'), ("C", type') ] $
     (var "A" ^-> var "B" ^-> var "C") ^->
     (var "B" ^-> var "A" ^-> var "C")

tFlip :: RawTerm
tFlip = (^\) ["A", "B", "C", "f", "b", "a"] $
        var "f" ^$ var "a" ^$ var "b"

testing ::
  Either
  TypeErroredTerm
  (FreeF TypeCheckerF TypeCheckedTerm (TCMonad TypeCheckedTerm))
testing = runTypeCheck2 $ runCheck' [] tFlip τFlip

trace :: [TypeCheckerF (TCMonad TypeCheckedTerm)]
trace = tcTrace stepTypeCheckerF $ checkF [] tFlip τFlip id

{-
didItWork :: [TypeCheckerF (TCMonad TypeCheckedTerm)] -> Bool
didItWork (a:b:c) = didItWork (b:c)
didItWork (a:[])  =
  case a of
  Success t ->
    let τ = typeOf t in
    raw τ == τFlip
  _ -> error "sucks"
-}

testNumberize :: NumberedTerm
testNumberize = numberize tFlip

manyRightApps :: Int -> RawTerm
manyRightApps 0 = var "x"
manyRightApps n = var "x" ^$ manyRightApps (n - 1)

manyLeftApps :: Int -> RawTerm
manyLeftApps 0 = var "x"
manyLeftApps n = manyLeftApps (n - 1) ^$ var "x"

testPretty :: IO ()
testPretty = do
  let p = (putStrLn . prettyTerm :: RawTerm -> IO ())
  p $ var "a" ^$ (var "b" ^$ (var "c" ^$ var "d"))
  p $ ((var "a" ^$ var "b") ^$ var "c") ^$ var "d"
  p $ manyLeftApps 40
  p $ manyRightApps 40
  p tFlip
  p τFlip

mainy :: IO ()
mainy = do
  --print testNumberize
  --pp testNumberize

  pp $ parseMaybe termP " A  B  C " -- this should parse as:
  pp $ parseMaybe termP "(A  B) C " -- this
  pp $ parseMaybe termP " A (B  C)"

  pp $ parseMaybe termP " A →  B  → C " -- this should parse as:
  pp $ parseMaybe termP "(A →  B) → C "
  pp $ parseMaybe termP " A → (B  → C)" -- this

  pp $ parseMaybe termP " (x : A) →  B  → C " -- this should parse as:
  pp $ parseMaybe termP "((x : A) →  B) → C "
  pp $ parseMaybe termP " (x : A) → (B  → C)" -- this

  pp $ parseMaybe termP "(_ : A) →  B" -- this should parse as:
  pp $ parseMaybe termP "     A  →  B" -- this
