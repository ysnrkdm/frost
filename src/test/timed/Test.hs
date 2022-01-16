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

testData :: [(String, Bool)]
testData =
  [ ("add4.cnf", False),
    ("diamond1.cnf", False),
    ("diamond2.cnf", False),
    ("diamond3.cnf", False),
    ("ph2.cnf", False),
    ("ph3.cnf", False),
    ("prime4.cnf", True),
    ("prime9.cnf", True),
    ("prime25.cnf", True),
    ("prime49.cnf", True),
    ("prime121.cnf", True),
    --
    ("add128.cnf", True),
    ("add16.cnf", True),
    ("add32.cnf", True),
    ("add4.cnf", True),
    ("add64.cnf", False),
    ("add8.cnf", True),
    ("and1.cnf", True),
    ("and2.cnf", True),
    ("bin1.cnf", True),
    ("bin2.cnf", True),
    ("bin3.cnf", True),
    ("def1.cnf", False),
    ("eq1.cnf", True),
    ("eq2.cnf", False),
    ("eq3.cnf", True),
    -- ("false.cnf", True),
    ("full2.cnf", False),
    ("full3.cnf", False),
    ("full4.cnf", False),
    ("ite1.cnf", True),
    ("ph11.cnf", True),
    ("ph4.cnf", False),
    ("ph5.cnf", False),
    ("ph6.cnf", False),
    ("prime121.cnf", True),
    ("prime1369.cnf", True),
    ("prime1681.cnf", True),
    ("prime169.cnf", True),
    ("prime1849.cnf", True),
    ("prime2209.cnf", True),
    ("prime25.cnf", True),
    ("prime289.cnf", True),
    ("prime361.cnf", True),
    ("prime4.cnf", True),
    ("prime4294967297.cnf", True),
    ("prime49.cnf", True),
    ("prime529.cnf", True),
    ("prime65537.cnf", True),
    ("prime841.cnf", True),
    ("prime9.cnf", True),
    ("prime961.cnf", True),
    ("probe1.cnf", True),
    ("sqrt10201.cnf", True),
    ("sqrt1042441.cnf", True),
    ("sqrt10609.cnf", True),
    ("sqrt11449.cnf", True),
    ("sqrt11881.cnf", True),
    ("sqrt12769.cnf", True),
    ("sqrt16129.cnf", True),
    ("sqrt259081.cnf", True),
    ("sqrt2809.cnf", True),
    ("sqrt3481.cnf", True),
    ("sqrt3721.cnf", True),
    ("sqrt4489.cnf", True),
    ("sqrt5041.cnf", True),
    ("sqrt5329.cnf", True),
    ("sqrt6241.cnf", True),
    ("sqrt63001.cnf", True),
    ("sqrt6889.cnf", True),
    ("sqrt7921.cnf", True),
    ("sqrt9409.cnf", True),
    ("tieshirt.cnf", True),
    ("tph6.cnf", True),
    -- ("true.cnf", True),
    ("twocores1.cnf", False),
    ("twocores2.cnf", False),
    ("twocores3.cnf", False),
    ("unit1.cnf", False),
    ("unit2.cnf", False),
    ("unit3.cnf", False),
    ("unit4.cnf", False),
    ("unit5.cnf", False),
    ("unit6.cnf", False),
    ("xor1.cnf", True),
    ("xor2.cnf", True),
    ("xor3.cnf", True),
    ("xor4.cnf", True)
  ]

solveFile :: String -> Bool -> IO ()
solveFile fileName isSat = do
  parsed <- parseDIMACSFromFile ("resources/problems/" ++ fileName)
  case parsed of
    Right dimacs -> do
      case solveDIMACS dimacs of
        Just _ -> assertBool "Expected UNSAT but SAT" isSat
        Nothing -> assertBool "Expected SAT but UNSAT" (not isSat)
      return ()
    left -> assertFailure $ "Input file " ++ fileName ++ " cannot be parsed"

runTestBounded :: (String, Bool) -> Test.HUnit.Test
runTestBounded (testFileName, expected) =
  test
    [ testFileName ~: do
        runOk <- timeout (1000000 * 10) $ solveFile testFileName expected
        assertEqual "test timed out (30 sec limit)" runOk (Just ())
        return ()
    ]

main :: IO ()
main =
  defaultMain $
    hUnitTestToTests $
      TestLabel "timeBoundTest" $ TestList $ map runTestBounded testData
