{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Term.Binder
  ( Binder (..),
    mkBinder,
  )
where

import Data.Aeson (ToJSON)
import Data.String (IsString (..))
import GHC.Generics (Generic)
import Prettyprinter ()
import Term.Variable (Variable, mkVariable)
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen (frequency)
import Test.SmallCheck.Series (Serial)
import Text.PrettyPrint.GenericPretty (Out)

newtype Binder ν = Binder {unBinder :: Maybe ν}
  deriving (Eq, Generic, Out, Serial m, Show)

instance Arbitrary ν => Arbitrary (Binder ν) where
  arbitrary =
    frequency
      [ (1, return . Binder $ Nothing),
        (9, Binder . Just <$> arbitrary)
      ]
  shrink (Binder b) = case b of
    Nothing -> []
    Just v -> Binder Nothing : [Binder (Just v') | v' <- shrink v]

instance IsString ν => IsString (Binder ν) where
  fromString s = Binder (Just (fromString s))

instance ToJSON ν => ToJSON (Binder ν)

mkBinder :: String -> Binder Variable
mkBinder = Binder . Just . mkVariable
