module Diff.ConcatMap.Test where

import qualified Diff.Atom as DA
import Diff.ConcatMap
import qualified Diff.List as DL
import Polysemy
import Polysemy.Error
import Polysemy.Trace

-- import           PrettyPrinting.PrettyPrintable

input :: [Int]
input = [2, 3, 4, 5]

f :: Int -> [Int]
f n = replicate n n

δinput :: DL.Diff Int (DA.Diff Int)
δinput =
  DL.Permute [2, 0, 3, 1]
    . DL.Insert 1
    . DL.nKeeps (length input)
    . DL.Insert 6
    $ DL.Same

patchList :: [Int] -> DL.Diff Int (DA.Diff Int) -> IO (Either String [Int])
patchList i δi = runM . ignoreTrace . runError $ DL.patch DA.patch i δi

-- property:
-- concatMap f (patch p l) = permute p' (concatMap f l)

patchElem :: DA.Diff Int -> Int -> Maybe Int
patchElem δi i =
  case (run . ignoreTrace . runError) (DA.patch i δi) of
    Left _ -> Nothing
    Right r -> Just r

test :: IO Bool
test = do
  let output = concatMap f input
  patchList input δinput >>= \case
    Left e -> do
      putStrLn e
      return False
    Right input' -> do
      print input'
      let output' = concatMap f input'
      case δconcatMap f patchElem input δinput of
        Nothing -> do
          putStrLn "Nothing"
          return False
        Just δoutput ->
          patchList output δoutput >>= \case
            Left e -> do
              putStrLn e
              return False
            Right output'' ->
              if output' == output''
                then do
                  print output''
                  return True
                else do
                  putStrLn "Expected:"
                  print output'
                  putStrLn "Obtained:"
                  print output''
                  return False
