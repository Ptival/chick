module Term.Numbered where

-- import           DictMetaMap
-- import qualified Term.Raw    as Raw
import           Term.Term

data Numbered

type Term = TermX Numbered
type Type = Term

type Numbering = Int

-- type instance X_Annot Numbered = Numbering
-- type instance X_App   Numbered = Numbering
-- type instance X_Hole  Numbered = Numbering
-- type instance X_Lam   Numbered = Numbering
-- type instance X_Let   Numbered = Numbering
-- type instance X_Pi    Numbered = Numbering
-- type instance X_Type  Numbered = Numbering
-- type instance X_Var   Numbered = Numbering

-- numberOf :: Term -> Int
-- numberOf = annotationOf

-- numberize :: Raw.Term -> Term
-- numberize = metaMap (dictMetaMap' (\ (acc, ()) -> (acc + 1, acc))) 0
