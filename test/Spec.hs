import Test.HUnit
import System.Environment
import Lib
import qualified Data.List as List

main = getArgs >>= runTestTT . tests

tests names =
  TestList $ map snd $ filter ((null names ||) . (`elem` names) . fst)
    []
