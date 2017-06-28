{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Tactic
  ( Tactic(..)
  , runTactic
  ) where

import Control.Monad.Except
import Prelude
import Text.PrettyPrint.Annotated.WL

import Atomic
import Goal
import PrettyPrinting.PrettyPrintable
import Term.TypeChecked as TypeChecked
import Term.Variable
import Typing.GlobalEnvironment

data Tactic ν
  = Atomic (Atomic ν)
  | Semicolon (Tactic ν) (Tactic ν)
  deriving (Show)

instance PrettyPrintable ν => PrettyPrintable (Tactic ν) where
  prettyDoc = \case
    Atomic a -> prettyDoc a
    Semicolon a b ->
      fillCat
        [ prettyDoc a
        , text "; "
        , prettyDoc b
        ]

-- decomposeTactic :: Tactic ν -> (Atomic ν, [Tactic ν])
-- decomposeTactic (Atomic a)      = (a, [])
-- decomposeTactic (Semicolon a b) =
--   let (atomic, ts) = decomposeTactic a in
--   (atomic, ts ++ [b])

runTactic ::
  MonadError String m =>
  GlobalEnvironment (Checked Variable) Variable ->
  Tactic Variable -> Goal (Checked Variable) Variable -> m [Goal (Checked Variable) Variable]
runTactic ge t goal =
  case t of
    Atomic a -> runAtomic ge a goal
    Semicolon t1 t2 -> do
      gs <- runTactic ge t1 goal
      gs' <- concat <$> sequence (map (runTactic ge t2) gs)
      return gs'
