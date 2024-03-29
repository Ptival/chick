{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Diff.Motive.Test where

import Diff.Motive
import qualified Diff.Term as DT
import Inductive.Motive
import Polysemy
import Polysemy.Error
import Polysemy.Trace
import StandardLibrary
import StandardLibraryDiff
import Term.Term
import qualified Term.Universe as U

testListToVec :: IO Bool
testListToVec =
  let listMotive = mkMotiveType () indList (Type U.Type)
   in let vecMotive = mkMotiveType () indVec (Type U.Type)
       in let δlistMotive = δmkMotiveType indList δListToVec
           in -- do
              -- putStrLn $ prettyStr listMotive
              -- putStrLn $ prettyStr δlistMotive
              -- putStrLn $ prettyStr vecMotive
              (runM . ignoreTrace . runError $ DT.patch listMotive δlistMotive) >>= \case
                Left (_s :: String) ->
                  -- do
                  -- putStrLn s
                  return False
                Right motive' ->
                  -- do
                  -- putStrLn $ prettyStr motive'
                  return (motive' == vecMotive)
