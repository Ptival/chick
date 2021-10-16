module Examples.Inductive
  ( inductives,
  )
where

import Inductive.Inductive (Inductive)
import Parsing.Unsafe (unsafeParseInductive)
import qualified StandardLibrary as STDLIB
import qualified Term.Raw as Raw
import Term.Variable ( Variable )

inductives :: [Inductive Raw.Raw Variable]
inductives =
  []
    ++ STDLIB.inductives
    ++ [ unsafeParseInductive . unlines $
           [ "Inductive foo (a : A) (b :B) : Type :=",
             "| constr : foo a b"
           ]
       ]
