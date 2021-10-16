{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Term.Variable
  ( Variable,
    mkVariable,
    unVariable,
  )
where

import Data.Aeson (ToJSON)
import Data.String (IsString (..))
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
import Test.QuickCheck.Gen (choose)
import Test.SmallCheck.Series (Serial (..), cons2)
import Text.PrettyPrint.GenericPretty (Out)

newtype Variable = Variable {unVariable :: String}
  deriving (Eq, Generic, Out, Show)

mkVariable :: String -> Variable
mkVariable "_" = error "Trying to make an underscore variable"
mkVariable s = Variable s

instance Arbitrary Variable where
  arbitrary = do
    c <- choose ('a', 'e')
    return $ mkVariable [c]

instance IsString Variable where
  fromString s = mkVariable s

instance Monad m => Serial m Variable where
  series = mkVariable <$> cons2 (:)

instance ToJSON Variable
