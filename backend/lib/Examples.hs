{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Examples where

import           Control.Monad.Trans.Free
import           Text.Megaparsec
import           Text.PrettyPrint.GenericPretty (pp)

import           Notations
import           Parsing
-- import PrettyPrinting
import qualified Term.Raw                       as Raw
--import Term.AlphaEquivalence
import           Term.Numbered                  as Numbered
import qualified Term.TypeChecked               as C
import qualified Term.TypeErrored               as E
import           Work

τFlip :: Raw.Type ν
τFlip = π [ ("A", type'), ("B", type'), ("C", type') ] $
     (var "A" ^-> var "B" ^-> var "C") ^->
     (var "B" ^-> var "A" ^-> var "C")

tFlip :: Raw.Term ν
tFlip = (^\) ["A", "B", "C", "f", "b", "a"] $
        var "f" ^$ var "a" ^$ var "b"

testing ::
  Either
  (E.Term ν)
  (FreeF (TypeCheckerF ν) (C.Term ν) (TCMonad ν (C.Term ν)))
testing = runTypeCheck2 $ runCheck' [] tFlip τFlip

trace :: [TypeCheckerF ν (TCMonad ν (C.Term ν))]
trace = tcTrace stepTypeCheckerF $ checkF [] tFlip τFlip id

{-
didItWork :: [TypeCheckerF (TCMonad TypeChecked.Term)] -> Bool
didItWork (a:b:c) = didItWork (b:c)
didItWork (a:[])  =
  case a of
  Success t ->
    let τ = typeOf t in
    raw τ == τFlip
  _ -> error "sucks"
-}

testNumberize :: Numbered.Term ν
testNumberize = numberize tFlip

manyRightApps :: Int -> Raw.Term ν
manyRightApps 0 = var "x"
manyRightApps n = var "x" ^$ manyRightApps (n - 1)

manyLeftApps :: Int -> Raw.Term ν
manyLeftApps 0 = var "x"
manyLeftApps n = manyLeftApps (n - 1) ^$ var "x"

testPretty :: IO ()
testPretty = do
  let p = (putStrLn . prettyTerm :: Raw.Term ν -> IO ())
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
