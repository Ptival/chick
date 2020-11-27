{-# LANGUAGE AllowAmbiguousTypes #-}

module PrettyPrinting.HasNonBindingPattern
  ( HasNonBindingPattern (..),
  )
where

import Language (Language (..))

class HasNonBindingPattern (l :: Language) where
  nonBindingPattern :: String

instance HasNonBindingPattern 'Chick where
  nonBindingPattern = "_"
