module Main (main) where

import Test.HUnit
import Lists hiding (main)

expected :: [(Integer, Integer)]
expected = [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3)]

listsTests :: Test
listsTests = test [
    "testV1" ~: "" ~: expected ~=? (v1 [1..2] [1..3] [10])
  , "testV2" ~: "" ~: expected ~=? (v2 [1..2] [1..3] [10])
  , "testV3" ~: "" ~: expected ~=? (v3 [1..2] [1..3] [10])
  , "testV4" ~: "" ~: expected ~=? (v4 [1..2] [1..3] [10])
  ]

main :: IO ()
main = do
  _ <- runTestTT listsTests
  return ()
