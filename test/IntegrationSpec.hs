module IntegrationSpec
  ( testIntegration
  ) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Interpreter
import           Lex
import           Parse

import           AST                            ( Query(AST) )
import           Data.Map

evalIO :: String -> IO (Result [[String]])
evalIO s = do
  table <- (evalRoot . parseJCQL . alexScanTokens) s
  return $ singleTable table

eval :: TableMap -> String -> Result [[String]]
eval tables s = singleTable $ evalTable tables query
  where (AST _ query) = (parseJCQL . alexScanTokens) s

singleTable :: Result Table -> Result [[String]]
singleTable res = do
  Prelude.map (head . elems) <$> res


readF :: Int -> Int -> String -> IO String
readF p e f = readFile ("./testCsv/p" ++ show p ++ "/e" ++ show e ++ "/" ++ f)

example :: Int -> Int -> Assertion
example p e = do
  query    <- readF p e "query.cql"
  out      <- evalIO query
  expected <- readF p e "query.csv"
  out @?= Ok (unparseCsv expected)

testIntegration :: TestTree
testIntegration = testGroup
  "Integration"
  [ testCase "reads csv file and produces list of pro developers" $ do
    out <-
      evalIO
        "import a './testCsv/user.csv' take a where not (a.2 = 'Aleksei') select a.2, a.3"
    out @?= Ok [["Ryan", "GB"], ["Dom", "PL"]]
  , testGroup "Example Problems"
              [testGroup "Problem 1" [testCase "Example 1" $ example 1 1]]
  , testGroup
    "Problems from Coursework"
    [ testGroup
        "Problem 1 (unsorted)"
        [ testCase "Example 1" $ example 1 1
        , testCase "Example 2" $ example 1 2
        , testCase "Example 3" $ example 1 3
        ]
    ]
  ]
