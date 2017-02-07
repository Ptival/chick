module TestShow where

import Text.PrettyPrint.GenericPretty (pp)
import Text.Printf                    (printf)

import Examples
import RawTerm
import Term

testShow :: TermX ξ -> String -> Bool
testShow t s = show t == s

test :: RawTerm -> String -> IO ()
test t s = do
  if testShow t s
    then putStrLn "✓"
    else do
    putStrLn "Failed for expression:"
    pp t
    putStrLn $ printf "Expected:\n%s\nObtained:" s
    print t

mainTestShow :: IO ()
mainTestShow = do
  let a = var "a"
  let b = var "b"
  test (lam ["a"] $ a) "λ a . a"
  test (lam ["a", "b"] $ a $$ b) "λ a b . a b"
