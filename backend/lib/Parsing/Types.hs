module Parsing.Types
  ( Parser
  ) where

import           Control.Applicative
import           Data.Functor
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Printf

type Parser = Parsec Void String
