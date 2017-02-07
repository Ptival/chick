{-# language DeriveAnyClass #-}
{-# language FlexibleContexts #-}
{-# language StandaloneDeriving #-}
{-# language UndecidableInstances #-}

module Examples where

import Control.Monad.Trans.Free
import Data.Default
import Text.PrettyPrint.GenericPretty (pp)

import Context
import NumberedTerm
import RawTerm
import Term
import TypeCheckedTerm
import TypeErroredTerm
import Work

infixr 1 -->
(-->) :: ForallX Default ξ => TypeX ξ -> TermX ξ -> TermX ξ
τ --> t = Pi def Nothing τ t

infixl 9 $$
($$) :: ForallX Default ξ => TermX ξ -> TermX ξ -> TermX ξ
t1 $$ t2 = App def t1 t2

lam :: ForallX Default ξ => [Name] -> TermX ξ -> TermX ξ
lam []       t = t
lam (n : ns) t = Lam def (Just n) (lam ns t)

π :: ForallX Default ξ => Context ξ -> TermX ξ -> TermX ξ
π []             t = t
π ((n, τ) : nτs) t = Pi def (Just n) τ (π nτs t)

var :: ForallX Default ξ => Name -> TermX ξ
var = Var def

set :: ForallX Default ξ => TypeX ξ
set = Type def

τFlip :: RawType
τFlip = π [ ("A", set), ("B", set), ("C", set) ] $
     (var "A" --> var "B" --> var "C") -->
     (var "B" --> var "A" --> var "C")

tFlip :: RawTerm
tFlip = lam ["A", "B", "C", "f", "b", "a"] $
        var "f" $$ var "a" $$ var "b"

testing ::
  Either
  TypeErroredTerm
  (FreeF TypeCheckerF TypeCheckedTerm (TCMonad TypeCheckedTerm))
testing = runTypeCheck2 $ runCheck' [] tFlip τFlip

trace :: [TypeCheckerF (TCMonad TypeCheckedTerm)]
trace = tcTrace stepTypeCheckerF $ checkF [] tFlip τFlip id

testNumberize :: NumberedTerm
testNumberize = numberize tFlip

mainy :: IO ()
mainy = do
  print testNumberize
  pp testNumberize
