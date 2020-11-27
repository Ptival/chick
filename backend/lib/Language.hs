module Language
  ( defaultLanguage,
    Language (..),
  )
where

import GHC.Generics (Generic)

data Language
  = Chick
  | Coq
  | OCaml
  deriving (Eq, Generic, Show)

defaultLanguage :: Language
defaultLanguage = Chick
