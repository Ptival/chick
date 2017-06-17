{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Term.Binder where

import Data.String
import GHC.Generics
import PrettyPrinting.PrettyPrintable
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.SmallCheck.Series
import Text.PrettyPrint.Annotated.WL
import Text.PrettyPrint.GenericPretty (Out)

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

instance PrettyPrintable ν => PrettyPrintable (Binder ν) where
  prettyDoc (Binder Nothing)  = text "_"
  prettyDoc (Binder (Just v)) = prettyDoc v
