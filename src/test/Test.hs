import Dimacs (parseDIMACSFromFile)
import Internal.Cdcl (solveDIMACS)
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

runTestBounded :: String -> String -> Bool -> Test.HUnit.Test
runTestBounded resourcePath testFileName expected =
  test
    [ testFileName ~: do
        runOk <- timeout (1000000 * 2) $ solveFile resourcePath testFileName expected
        assertEqual "test timed out (10 sec limit)" runOk (Just ())
        return ()
    ]

testData :: [(String, Bool)]
testData =
  [ ("ph2.cnf", False),
    ("prime9.cnf", True),
    ("prime25.cnf", True),
    ("and2.cnf", True),
    ("bin1.cnf", True),
    ("bin2.cnf", True),
    ("bin3.cnf", True),
    ("eq1.cnf", True),
    ("eq2.cnf", False),
    ("eq3.cnf", True),
    ("full2.cnf", False),
    ("ite1.cnf", True),
    ("prime25.cnf", True),
    ("prime9.cnf", True),
    ("probe1.cnf", True),
    ("sqrt16129.cnf", True),
    ("tieshirt.cnf", True),
    ("twocores1.cnf", False),
    ("twocores2.cnf", False),
    ("unit1.cnf", False),
    ("unit2.cnf", False),
    ("unit3.cnf", False),
    ("unit4.cnf", False),
    ("unit5.cnf", False),
    ("unit6.cnf", False),
    ("xor1.cnf", True),
    ("xor2.cnf", True),
    ("xor3.cnf", True)
  ]

main :: IO ()
main =
  defaultMain $
    hUnitTestToTests $
      TestLabel "timeBoundTest" $
        TestList $ map (uncurry (runTestBounded "resources/problems/")) testData
