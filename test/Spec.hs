import Test.HUnit
import System.Environment

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

task1Test = undefined
task2Test = undefined
task3Test = undefined
task4Test = undefined
task5Test = undefined
task6Test = undefined
task7Test = undefined
task8Test = undefined
task9Test = undefined
task10Test = undefined
task11Test = undefined
