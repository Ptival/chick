module StandardLibrary.Prelude where

import Term.Raw as Raw
import Data.Maybe (Maybe)
import Parsing.Term (parseMaybeTerm)
import Term.Variable (Variable)

τId :: Maybe (Raw.Term Variable)
τId = parseMaybeTerm "(T : Type) → T → T"

tId :: Maybe (Raw.Term Variable)
tId = parseMaybeTerm "λ T x . x"

τFlip :: Maybe (Raw.Term Variable)
τFlip = parseMaybeTerm "(A B C : Type) → (A → B → C) → (B → A → C)"

tFlip :: Maybe (Raw.Term Variable)
tFlip = parseMaybeTerm "λ A B C f b a . f a b"
