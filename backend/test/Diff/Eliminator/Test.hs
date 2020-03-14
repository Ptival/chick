module Diff.Eliminator.Test where

import           Control.Monad
import           Polysemy
import           Polysemy.Error
import           Polysemy.Trace
import           System.Exit
import           Text.Printf

import           Diff.Eliminator
import qualified Diff.Inductive                 as DI
import qualified Diff.Term                      as DT
import           Inductive.Eliminator
import           Inductive.Inductive
import           Language
import           PrettyPrinting.PrettyPrintable
import           StandardLibrary
import           StandardLibraryDiff
import           Term.Term

test ::
  ( Member (Error String) r
  , Member Trace r
  ) =>
  Inductive () Variable -> DI.Diff () -> Inductive () Variable -> Sem r Bool
test indFrom δind indTo =
  let elimFrom = mkEliminatorType () indFrom in
  let elimTo   = mkEliminatorType () indTo   in
  case δmkEliminatorType () indFrom δind of

  Nothing -> do
    trace "δmkEliminatorType failed"
    return False

  Just δelim -> do
    trace $ "-- Eliminator before:"
    trace $ prettyStr @'Chick elimFrom
    trace $ replicate 80 '-'
    trace $ "-- δ computed to patch eliminator:"
    trace $ prettyStr @'Chick δelim
    trace $ replicate 80 '-'
    trace $ "-- Eliminator expected:"
    trace $ prettyStr @'Chick elimTo
    trace $ replicate 80 '-'
    elimTo' <- DT.patch elimFrom δelim
    trace $ "-- Eliminator obtained:"
    trace $ prettyStr @'Chick elimTo'
    trace $ replicate 80 '-'
    trace $ "-- Eliminator expected and obtained are α-equivalent?"
    return (elimTo' == elimTo)

type Test r =
  ( Member (Error String) r
  , Member Trace r
  ) => Sem r Bool

testδBoolToNat ::
  ( Member (Error String) r
  , Member Trace r
  ) =>
  Sem r Bool
testδBoolToNat = test indBool δBoolToNat indNat

testδNatToList ::
  ( Member (Error String) r
  , Member Trace r
  ) =>
  Sem r Bool
testδNatToList = test indNat δNatToList indList

testδListToVec ::
  ( Member (Error String) r
  , Member Trace r
  ) =>
  Sem r Bool
testδListToVec = test indList δListToVec indVec

unitTests ::
  ( Member (Error String) r
  , Member Trace r
  ) =>
  [(String, Sem r Bool)]
unitTests =
  [ ("testδBoolToNat", testδBoolToNat)
  , ("testδNatToList", testδNatToList)
  , ("testδListToVec", testδListToVec)
  ]

main :: IO ()
main = do
  putStrLn "\n"
  forM_ unitTests $ \ (name, t) -> do
    (runM . ignoreTrace $ runError t) >>= \case
      Right True -> putStrLn $ printf "[✓] %s" name
      Right False -> do
        putStrLn $ printf "[✗] %s: patching succeeded, but unexpected result" name
        exitFailure
      Left (err :: String) -> do
        putStrLn $ printf "[✗] %s: %s" name err
        exitFailure
  exitSuccess
