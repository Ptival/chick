module Examples.Utils
  ( distinctPairs,
  )
where

-- for now, assuming the input list has distinct elements
distinctPairs :: [a] -> [(a, a)]
distinctPairs l =
  let maxIndex = length l - 1
   in [ (l !! i1, l !! i2)
        | i1 <- [0 .. maxIndex],
          i2 <- [i1 + 1 .. maxIndex]
      ]
