import Dimacs (parseDIMACSFromFile)
import Internal.Sat (solveDIMACS)
import Sat (solve)
import System.Timeout (timeout)
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit
  ( Test (TestLabel, TestList),
    Testable (test),
    assertBool,
    assertEqual,
    assertFailure,
    (~:),
  )
import TestData (benchData, testData)

solveFile :: String -> String -> Bool -> IO ()
solveFile resourcePath fileName isSat = do
  parsed <- parseDIMACSFromFile (resourcePath ++ fileName)
  case parsed of
    Right dimacs -> do
      case solveDIMACS dimacs of
        Just _ -> assertBool "Expected UNSAT but SAT" isSat
        Nothing -> assertBool "Expected SAT but UNSAT" (not isSat)
      return ()
    left -> assertFailure $ "Input file " ++ fileName ++ " cannot be parsed"

runTestBounded :: String -> (String, Bool) -> Test.HUnit.Test
runTestBounded resourcePath (testFileName, expected) =
  test
    [ testFileName ~: do
        runOk <- timeout (1000000 * 10) $ solveFile resourcePath testFileName expected
        assertEqual "test timed out (10 sec limit)" runOk (Just ())
        return ()
    ]

main :: IO ()
main =
  defaultMain $
    hUnitTestToTests (TestLabel "timeBoundTest" $ TestList $ map (runTestBounded "resources/problems/") testData)

-- ++ hUnitTestToTests (TestLabel "timeBoundTest Benchmark 2021" $ TestList $ map (runTestBounded "resources/benchmark_2021/main/") benchData)
