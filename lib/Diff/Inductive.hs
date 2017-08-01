{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Diff.Inductive
  ( Diff(..)
  , patch
  ) where

import           Control.Monad.Fix
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception

import qualified Diff.Atom as DA
import qualified Diff.Constructor as DC
import qualified Diff.List as DL
import qualified Diff.Term as DT
import           Inductive.Inductive
import           Term.Binder
import qualified Term.Raw as Raw
import           Term.Term
import           Term.Variable

type Term α = TypeX α Variable
type BoundTerm α = (Binder Variable, Term α)

data Diff α
  = Same
  | Change
    (DA.Diff Variable)
    (DL.Diff (BoundTerm α)            (DA.Diff (BoundTerm α)))
    (DL.Diff (BoundTerm α)            (DA.Diff (BoundTerm α)))
    (DL.Diff (Constructor α Variable) (DC.Diff α))
  deriving (Show)

patch ::
  Member (Exc String) r =>
  Inductive α Variable -> Diff α -> Eff r (Inductive α Variable)
patch ind@(Inductive n ps is cs) = \case
  Same                  -> return ind
  Change δn δps δis δcs -> do
    n'  <- DA.patch n δn
    ps' <- DL.patch DA.patch ps δps
    is' <- DL.patch DA.patch is δis
    cs' <- DL.patch DC.patch cs δcs -- note: the constructors still refer to the old inductive!
    return $ fix $ \ind' -> Inductive n' ps' is' $ map (updateConstructorInd ind') cs'
      where
        updateConstructorInd ind' (Constructor _ n ps is) = Constructor ind' n ps is

-- so, the output type is:
-- Pi p0 (Pi p1 (Pi p2 (Pi n (Pi i0 (Pi i1 (Pi i2))))))
-- and becomes:
-- Pi p0' (Pi p1' (Pi n' (Pi i0' (Pi i1'))))
-- δps will tell us how to update the p-telescope
-- δn  will tell us how to update the n
-- δis will tell us how to update the i-telescope
δinductiveRawType ::
  (DA.Diff Variable) ->
  (DL.Diff (BoundTerm α) (DA.Diff (BoundTerm α))) ->
  (DL.Diff (Term α) (DA.Diff (Term α))) ->
  DT.Diff α
δinductiveRawType δn δps δis =
  foldr onIndex (DT.CpyPi _ (Binder . Just <$> δn) (foldr onParam DT.Same ps)) is
  where
    onIndex :: Raw.Type Variable -> DT.Diff α -> DT.Diff α
    onIndex i      t = _ -- Pi () i (abstractAnonymous t)
    onParam :: (Binder Variable, Raw.Type Variable) -> DT.Diff α -> DT.Diff α
    onParam (b, p) t = _ -- Pi () p (abstractBinder b t)
