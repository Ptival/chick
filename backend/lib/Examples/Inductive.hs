module Examples.Inductive
  ( inductives
  ) where

import           Inductive.Inductive
import           Parsing.Unsafe
import qualified StandardLibrary as STDLIB
import qualified Term.Raw as Raw
import           Term.Term

inductives :: [Inductive Raw.Raw Variable]
inductives = []
  ++ STDLIB.inductives
  ++ [ unsafeParseInductive . unlines $
       [ "Inductive foo (a : A) (b :B) : Type :="
       , "| constr : foo a b"
       ]  
     ]
