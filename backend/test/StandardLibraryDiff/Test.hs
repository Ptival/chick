module StandardLibraryDiff.Test where

import           Polysemy
import           Polysemy.Error
import           Polysemy.Trace

import qualified Diff.Inductive                 as DI
import           Inductive.Inductive
import           Language
import           PrettyPrinting.PrettyPrintable
import           StandardLibrary
import           StandardLibraryDiff
import           Term.Term

test ::
  PrettyPrintable 'Chick α =>
  Show α =>
  Inductive α Variable -> DI.Diff α -> Inductive α Variable -> IO Bool
test indFrom δind indTo =
  case run . ignoreTrace . runError $ DI.patch indFrom δind of
    Left e -> do
      putStrLn e
      return False
    Right indTo' ->
      return $ indTo == indTo'

testδBoolToNat :: IO Bool
testδBoolToNat = test indBool δBoolToNat indNat

testδNatToList :: IO Bool
testδNatToList = test indNat δNatToList indList

testδListToVec :: IO Bool
testδListToVec = test indList δListToVec indVec
