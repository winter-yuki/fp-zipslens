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
  [ TestCase $ assertEqual "to 0" 0 $ bin2int empty
  , TestCase $ assertEqual "to 4" 4 $ bin2int four
  , TestCase $ assertEqual "from 0" empty $ int2bin 0
  , TestCase $ assertEqual "from 4" four $ int2bin 4
  ]
  where
    four = In $ zero $ In $ zero $ In $ one $ In empty

task2Test = TestList []
task3Test = TestList []
task4Test = TestList []
task5Test = TestList []
task6Test = TestList []
task7Test = TestList []
task8Test = TestList []
task9Test = TestList []
task10Test = TestList []
task11Test = TestList []
