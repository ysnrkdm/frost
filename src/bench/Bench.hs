import Criterion.Main
import Criterion.Types
import Dimacs (parseDIMACSFromFile)
import Internal.Sat (solveDIMACS)
import Sat (solve)
import Statistics.Types
import Test.HUnit

solveFile :: String -> Bool -> IO ()
solveFile fileName isSat = do
  parsed <- parseDIMACSFromFile ("resources/problems/" ++ fileName)
  case parsed of
    Right dimacs -> do
      case solveDIMACS dimacs of
        Just _ -> assertBool "" isSat
        Nothing -> assertBool "" (not isSat)
      return ()
    left -> assertFailure $ "" ++ fileName ++ "cannot be parsed"

main :: IO ()
main =
  Criterion.Main.defaultMainWith
    (defaultConfig {timeLimit = 30})
    [ bgroup
        "sat tests"
        [ benchit "add4.cnf" False,
          benchit "diamond1.cnf" False,
          benchit "diamond2.cnf" False,
          benchit "diamond3.cnf" False,
          benchit "ph2.cnf" False,
          benchit "ph3.cnf" False,
          benchit "prime4.cnf" True,
          benchit "prime9.cnf" True,
          benchit "prime25.cnf" True,
          benchit "prime49.cnf" True,
          benchit "prime121.cnf" True
        ]
    ]
  where
    benchit fileName isSat = bench fileName $ whnfIO $ solveFile fileName isSat
