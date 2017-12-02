module Ports.Sexp where

import Prelude
import Data.Function.Uncurried (Fn3, Fn5, runFn3, runFn5)
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(..))

data Sexp
  = Atom String
  | List (Array Sexp)

derive instance genericSexp :: Generic Sexp

instance showSexp :: Show Sexp where
  show = gShow

foreign import _sexp ::
  Fn5 (∀ a. Maybe a) (∀ a. a -> Maybe a) (String -> Sexp) (Array Sexp -> Sexp) String (Maybe Sexp)
sexp :: String -> Maybe Sexp
sexp = runFn5 _sexp Nothing Just Atom List

foreign import _jsonParseArray :: Fn3 (∀ a. Maybe a) (∀ a. a -> Maybe a) String (Maybe (Array String))
jsonParseArray :: String -> Maybe (Array String)
jsonParseArray = runFn3 _jsonParseArray Nothing Just
