import Test.HUnit
import System.Environment
import Lib

main = getArgs >>= runTestTT . tests

tests names =
  TestList $ map snd $ filter ((null names ||) . (`elem` names) . fst)
    [ ("task1", task1Test)
    , ("task2", task2Test)
    , ("task3", task3Test)
    , ("task4", task4Test)
    , ("task5", task5Test)
    , ("task6", task6Test)
    , ("task7", task7Test)
    , ("task8", task8Test)
    , ("task9", task9Test)
    , ("task10", task10Test)
    , ("task11", task11Test)
    ]

task1Test = TestList
  [ TestCase $ assertEqual "to 0" 0 $ bin2int zero
  , TestCase $ assertEqual "to 4" 4 $ bin2int four
  , TestCase $ assertEqual "from 0" zero $ int2bin 0
  , TestCase $ assertEqual "from 4" four $ int2bin 4
  ]
  where
    zero = In emptyCtor
    four = In $ zeroCtor $ In $ zeroCtor $ In $ oneCtor $ In emptyCtor

task2Test = TestList
  [ TestCase $ assertEqual "show 3" "3" $ showExpr e3
  , TestCase $ assertEqual "show (3+5)" "(3+5)" $ showExpr ep35
  , TestCase $ assertEqual "show ((3+5)*7)" "((3+5)*7)" $ showExpr emp357
  , TestCase $ assertEqual "show (7*(3+5))" "(7*(3+5))" $ showExpr em7p35
  , TestCase $ assertEqual "eval 3" 3 $ evalExpr e3
  , TestCase $ assertEqual "eval (3+5)" 8 $ evalExpr ep35
  , TestCase $ assertEqual "eval ((3+5)*7)" 56 $ evalExpr emp357
  , TestCase $ assertEqual "eval (7*(3+5))" 56 $ evalExpr em7p35
  ]

task3Test = TestList
  [ TestCase $ assertEqual "compile 3" "3" $ cs e3
  , TestCase $ assertEqual "compile (3+5)" "+ 3 5" $ cs ep35
  , TestCase $ assertEqual "compile ((3+5)*7)" "* + 3 5 7" $ cs emp357
  , TestCase $ assertEqual "compile (7*(3+5))" "* 7 + 3 5" $ cs em7p35
  , TestCase $ assertEqual "evalSM 3" [3] $ ce e3
  , TestCase $ assertEqual "evalSM (3+5)" [8] $ ce ep35
  , TestCase $ assertEqual "evalSM ((3+5)*7)" [56] $ ce emp357
  , TestCase $ assertEqual "evalSM (7*(3+5))" [56] $ ce em7p35
  ]
  where
    cs = showByteCode . compile
    ce = evalSM . compile

task4Test = TestList []
task5Test = TestList []
task6Test = TestList []
task7Test = TestList []
task8Test = TestList []
task9Test = TestList []
task10Test = TestList []
task11Test = TestList []
