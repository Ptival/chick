{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Term.Binder
  ( Binder(..)
  , mkBinder
  ) where

import Data.Aeson
import Data.String
import GHC.Generics
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.SmallCheck.Series
import Text.PrettyPrint.GenericPretty (Out)

import Term.Variable

newtype Binder ν
  = Binder { unBinder :: Maybe ν }
  deriving (Eq, Generic, Out, Serial m, Show)

instance Arbitrary ν => Arbitrary (Binder ν) where
  arbitrary =
    frequency
    [ (1, return . Binder $ Nothing)
    , (9, Binder . Just <$> arbitrary)
    ]
  shrink (Binder b) = case b of
    Nothing -> []
    Just v  -> [Binder Nothing] ++ [Binder (Just v') | v' <- shrink v]

instance IsString ν => IsString (Binder ν) where
  fromString s = Binder (Just (fromString s))

instance ToJSON ν => ToJSON (Binder ν) where

mkBinder :: String -> Binder Variable
mkBinder = Binder . Just . mkVariable
