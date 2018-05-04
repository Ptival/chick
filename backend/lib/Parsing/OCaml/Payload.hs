module Parsing.OCaml.Payload
  ( payload_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml
import Parsing.OCaml.Structure

payload_P :: Parser Payload
payload_P = choice
  [ PStr <$> structure_P
  ]
