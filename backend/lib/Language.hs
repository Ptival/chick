module Language
  ( defaultLanguage
  , Language(..)
  ) where

data Language
  = Chick
  | Coq
  | OCaml

defaultLanguage :: Language
defaultLanguage = Chick
