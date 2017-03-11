module Typing.GlobalEnvironment where

import Term.Term
import Term.TypeChecked

data GlobalDeclaration ξ
  = GlobalAssum Variable (TypeX ξ)
  | GlobalDef   Variable (TermX ξ) (TypeX ξ)
  | GlobalInd   (Inductive ξ)

deriving instance (ForallX Eq   ξ) => Eq   (Declaration ξ)
deriving instance (ForallX Show ξ) => Show (Declaration ξ)

instance (ForallX Arbitrary ξ) => Arbitrary (Declaration ξ) where
  arbitrary =
    oneof
    [ LocalAssum <$> arbitrary <*> genTerm 2
    , LocalDef   <$> genTerm 2 <*> arbitrary <*> genTerm 2
    ]

prettyDeclaration :: ForallX ((~) a) ξ => PrecedenceTable -> Declaration ξ -> Doc a
prettyDeclaration precs (LocalAssum (Variable v) τ) =
  sep [text v, char ':', prettyTermDoc precs τ]
prettyDeclaration precs (LocalDef t (Variable v) τ) =
  sep [text v, text ":=", prettyTermDoc precs t , char ':', prettyTermDoc precs τ]

nameOf :: LocalDeclaration ξ -> Variable
nameOf (LocalAssum v _)   = v
nameOf (LocalDef   _ v _) = v
