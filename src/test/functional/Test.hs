import qualified Data.IntMap as IntMap
import Dimacs (parseDIMACSFromFile)
import Internal.Cdcl
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

evalTest :: Test
evalTest =
  test
    [ "" ~: assertEqual "" (Just False) $
        eval
          ( SolverState
              [ [1, -9],
                [-2, -9],
                [9, 2, -1],
                [-1, -10],
                [2, -10],
                [10, -2, 1],
                [-9, -11],
                [-10, -11],
                [11, 10, 9],
                [3, -12],
                [4, -12],
                [12, -4, -3],
                [5, -13],
                [6, -13],
                [13, -6, -5],
                [-3, -14],
                [-4, -14],
                [14, 4, 3],
                [13, -15],
                [-14, -15],
                [15, 14, -13],
                [-12, -16],
                [-15, -16],
                [16, 15, 12],
                [-11, -17],
                [-16, -17],
                [17, 16, 11],
                [11, -18],
                [16, -18],
                [18, -16, -11],
                [-17, -19],
                [-18, -19],
                [19, 18, 17],
                [3, -20],
                [-4, -20],
                [20, 4, -3],
                [-3, -21],
                [4, -21],
                [21, -4, 3],
                [-20, -22],
                [-21, -22],
                [22, 21, 20],
                [13, -23],
                [-22, -23],
                [23, 22, -13],
                [-12, -24],
                [-23, -24],
                [24, 23, 12],
                [-11, -25],
                [-24, -25],
                [25, 24, 11],
                [11, -26],
                [24, -26],
                [26, -24, -11],
                [-25, -27],
                [-26, -27],
                [27, 26, 25],
                [19, -28],
                [-27, -28],
                [28, 27, -19],
                [-19, -29],
                [27, -29],
                [29, -27, 19],
                [-28, -30],
                [-29, -30],
                [30, 29, 28],
                [7, -31],
                [-8, -31],
                [31, 8, -7],
                [-7, -32],
                [8, -32],
                [32, -8, 7],
                [-31, -33],
                [-32, -33],
                [33, 32, 31],
                [1, -34],
                [2, -34],
                [34, -2, -1],
                [-1, -35],
                [-2, -35],
                [35, 2, 1],
                [-35, -36],
                [-16, -36],
                [36, 16, 35],
                [-34, -37],
                [-36, -37],
                [37, 36, 34],
                [-33, -38],
                [-37, -38],
                [38, 37, 33],
                [33, -39],
                [37, -39],
                [39, -37, -33],
                [-38, -40],
                [-39, -40],
                [40, 39, 38],
                [-34, -41],
                [-25, -41],
                [41, 25, 34],
                [-33, -42],
                [-41, -42],
                [42, 41, 33],
                [33, -43],
                [41, -43],
                [43, -41, -33],
                [-42, -44],
                [-43, -44],
                [44, 43, 42],
                [40, -45],
                [-44, -45],
                [45, 44, -40],
                [-40, -46],
                [44, -46],
                [46, -44, 40],
                [-45, -47],
                [-46, -47],
                [47, 46, 45],
                [30, -48],
                [47, -48],
                [48, -47, -30],
                [7, -49],
                [8, -49],
                [49, -8, -7],
                [-7, -50],
                [-8, -50],
                [50, 8, 7],
                [34, -51],
                [-50, -51],
                [51, 50, -34],
                [-49, -52],
                [-51, -52],
                [52, 51, 49],
                [-35, -53],
                [-50, -53],
                [53, 50, 35],
                [-16, -54],
                [53, -54],
                [54, -53, 16],
                [52, -55],
                [-54, -55],
                [55, 54, -52],
                [-49, -56],
                [-42, -56],
                [56, 42, 49],
                [-55, -57],
                [56, -57],
                [57, -56, 55],
                [55, -58],
                [-56, -58],
                [58, 56, -55],
                [-57, -59],
                [-58, -59],
                [59, 58, 57],
                [48, -60],
                [59, -60],
                [60, -59, -48],
                [-60]
              ]
              ( IntMap.fromList
                  [ (0, [1, -60]),
                    (1, [-9, -60, -35, -10]),
                    (2, [2, -60]),
                    (3, [11, -60, -35, -9]),
                    (4, [3, -60, -25, -17, -10, -9]),
                    (5, [-12, -60, -21, -14]),
                    (6, [-4, -60]),
                    (7, [5, -60, -21, -12]),
                    (8, [-13, -60]),
                    (9, [-6, -60, -23, -15]),
                    (10, [16, -60, -23, -15, -13]),
                    (11, [18, -60, -54, -36, -17, -15, -12]),
                    (12, [20, -60, -54, -36, -28, -25, -19, -15, -12, 16, -17, -10, -9, 11]),
                    (13, [24, -60, -22, -21, -12, -4, -14, 3]),
                    (14, [26, -60, -25, -23, -12]),
                    (15, [30, -60, -29, -27, -23, -12, 24, -25, -17, -10, -9, 11]),
                    (16, [7, -60, -29, -28]),
                    (17, [-31, -60, -50, -32]),
                    (18, [8, -60]),
                    (19, [33, -60, -50, -31]),
                    (20, [34, -60, -42, -38, -32, -31]),
                    (21, [40, -60, -43, -41, -39, -37, -35, -25, -17, 11, -9, 2, -10, 1]),
                    (22, [44, -60, -46, -39, -38]),
                    (23, [47, -60, -45, -43, -42]),
                    (24, [48, -60, -46, -45]),
                    (25, [49, -59, -60, -46, -45, 47, -29, -28, 30]),
                    (26, [51, -48, -60, 59, -58, -57, -56, -55, -52, -50, -42, -38, 33, -31, 8, -32, 7]),
                    (27, [-60, -58, -55, 53, -52, -50, -43, -41, -39, -37, -35, -25, -17, 11, -9, 2, -10, 1, 34])
                  ]
              )
              []
          )
    ]

main :: IO ()
main =
  defaultMain $
    hUnitTestToTests $
      TestLabel "timeBoundTest" $
        TestList
          [evalTest]
