-- {-# LANGUAGE OverloadedStrings #-}

module TestShow where

-- import Text.PrettyPrint.GenericPretty (pp)
-- import Text.Printf                    (printf)

-- --import Examples
-- import Notations
-- import Term.Raw
-- import Term.Term

-- testShow :: ForallX Show ξ => TermX ξ -> String -> Bool
-- testShow t s = show t == s

-- test :: RawTerm -> String -> IO ()
-- test t s = do
--   if testShow t s
--     then putStrLn "✓"
--     else do
--     putStrLn "Failed for expression:"
--     pp t
--     putStrLn $ printf "Expected:\n%s\nObtained:" s
--     print t

-- mainTestShow :: IO ()
-- mainTestShow = do
--   let a = var "a"
--   let b = var "b"
--   test ((^\) ["a"] $ a) "λ a . a"
--   test ((^\) ["a", "b"] $ a ^$ b) "λ a b . a b"
