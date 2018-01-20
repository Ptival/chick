{-# LANGUAGE ScopedTypeVariables #-}

module StandardLibraryDiff.Test where

import           Control.Monad.Freer.Exception

import qualified Diff.Inductive as DI
import           Inductive.Inductive
import           PrettyPrinting.PrettyPrintable
import           StandardLibrary
import           StandardLibraryDiff
import           Term.Term
import           Utils

test ::
  ( PrettyPrintable α
  , Show α
  ) =>
  Inductive α Variable -> DI.Diff α -> Inductive α Variable -> IO Bool
test indFrom δind indTo = do
  case skipTrace . runError $ DI.patch indFrom δind of
    Left (e :: String) -> do
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
