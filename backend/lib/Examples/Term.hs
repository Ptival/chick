module Examples.Term
  ( terms,
  )
where

import Examples.Inductive (inductives)
import Inductive.Eliminator (mkEliminatorRawType)
import Parsing.Unsafe (unsafeParseTerm)
import qualified StandardLibrary as STDLIB
import qualified Term.Raw as Raw
import Term.Variable (Variable)

terms :: [Raw.Term Variable]
terms =
  []
    ++ STDLIB.terms
    ++ [mkEliminatorRawType i | i <- inductives]
    -- These contain duplicates:
    -- ++ [ constructorRawType True c | i <- inductives
    --                                , c <- inductiveConstructors i
    --    ]
    ++ [unsafeParseTerm "λ x, x y"]
    ++ [ unsafeParseTerm $
           unlines
             [ "λ x s t, match t with                                     ",
               "  | tvar y =>                                             ",
               "      if beq_id x y then s else t                         ",
               "  | tabs y T t1 =>                                        ",
               "      tabs y T (if beq_id x y then t1 else (subst x s t1))",
               "  | tapp t1 t2 =>                                         ",
               "      tapp (subst x s t1) (subst x s t2)                  ",
               "  end                                                     "
             ]
       ]
