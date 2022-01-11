module Lib
  ( someFunc,
  )
where

import Dimacs (parseDIMACSFromFile)
import Internal.Sat (solveDIMACS)
import Sat (solve)

someFunc :: IO ()
someFunc = do
  let solved =
        solve
          [ [4, 5],
            [-4, -5],
            [1, 6],
            [-1, 6],
            [-2, -3]
          ]
  print solved
  parsed <- parseDIMACSFromFile "resources/problems/prime4.cnf"
  case parsed of
    Right dimacs -> print $ show $ solveDIMACS dimacs
    left -> print "Error!"
