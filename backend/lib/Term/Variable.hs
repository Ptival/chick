{-# language ConstraintKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Term.Variable
  ( Variable
  , mkVariable
  , unVariable
  ) where

import Data.Aeson
import Data.String
import GHC.Generics
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.SmallCheck.Series
import Text.PrettyPrint.Annotated.WL
import Text.PrettyPrint.GenericPretty (Out)

import PrettyPrinting.PrettyPrintable

newtype Variable
  = Variable { unVariable :: String }
  deriving (Eq, Generic, Out, Show)

mkVariable :: String -> Variable
mkVariable "_" = error "Trying to make an underscore variable"
mkVariable s   = Variable s

instance Arbitrary Variable where
  arbitrary = do
    c <- choose ('a', 'e')
    return $ mkVariable [c]

instance IsString Variable where
  fromString s = mkVariable s

instance Monad m => Serial m Variable where
  series = mkVariable <$> cons2 (:)

instance PrettyPrintable Variable where
  prettyDoc (Variable s) = text s

instance ToJSON Variable where
