{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Term.Binder where

import Data.String
import GHC.Generics
import PrettyPrinting.PrettyPrintable
import Term.Variable
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.SmallCheck.Series
import Text.PrettyPrint.Annotated.WL
import Text.PrettyPrint.GenericPretty (Out)

newtype Binder
  = Binder { unBinder :: Maybe Variable }
  deriving (Eq, Generic, Out, Serial m, Show)

instance Arbitrary Binder where
  arbitrary =
    frequency
    [ (1, return . Binder $ Nothing)
    , (9, Binder . Just <$> arbitrary)
    ]
  shrink (Binder b) = case b of
    Nothing -> []
    Just v  -> [Binder Nothing] ++ [Binder (Just v') | v' <- shrink v]

instance IsString Binder where
  fromString s = Binder (Just (fromString s))

instance PrettyPrintable Binder where
  prettyDoc (Binder Nothing)  = text "_"
  prettyDoc (Binder (Just v)) = prettyDoc v
