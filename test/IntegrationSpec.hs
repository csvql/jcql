module IntegrationSpec
  ( testIntegration
  ) where

import           AST                            ( Query(AST) )
import           Data.Map
import           Interpreter
import           Lex
import           Parse
import           Test.Tasty
import           Test.Tasty.HUnit

evalIO :: String -> IO (Result [[String]])
evalIO = evalRoot . parseJCQL . alexScanTokens

eval :: TableMap -> String -> Result [[String]]
eval tables s = evalTable tables query
  where (AST _ query) = (parseJCQL . alexScanTokens) s

problem :: FilePath -> Assertion
problem path = do
  (expected, query) <- readProblem' path
  out               <- evalIO query
  out @?= Ok (unparseCsv expected)

problem' :: FilePath -> TestTree
problem' f = testCase f $ problem $ "./testCsv/problems/" ++ f ++ "/query"

example :: FilePath -> TestTree
example f = testCase f $ problem $ "./testCsv/examples/" ++ f

testIntegration :: TestTree
testIntegration = testGroup
  "Valid tests"
  [ testCase "reads csv file and produces list of pro developers" $ do
    out <-
      evalIO
        "import a './testCsv/user.csv' take a where not (a.2 = 'Aleksei') select a.2, a.3"
    out @?= Ok [["Ryan", "GB"], ["Dom", "PL"]]
  , testGroup
    "Provided Problems"
    [ problem' "1.1"
    , problem' "1.2"
    , problem' "1.3"
    , problem' "2.1"
    , problem' "2.2"
    , problem' "2.3"
    , problem' "3.1"
    , problem' "3.2"
    , problem' "3.3"
    , problem' "4.1"
    , problem' "4.2"
    , problem' "5.1"
    , problem' "6.1"
    , problem' "7.1"
    , problem' "8.1"
    ]
  , testGroup "Example tests" [example "multiple joins", example "alias", example "left join", example "left join empty"]
  , testGroup
    "Errors"
    [ testCase "Table not found" $ eval empty "take a" @?= Error
      "table 'a' not found"
    , testCase "Column not found" $ eval tableA "take a select a.3" @?= Error
      "could not find column 3 in table 'a' (of length 1)"
    , testCase "Invalid select type"
    $   eval tableA "take a select a.1 = ''"
    @?= Error "Error in Expression: (a.1 = '') should be of type 'string', but got 'boolean'"
    ]
  ]
tableA = singleton "a" [singleton "a" ["1", "2"]]

readProblem' :: FilePath -> IO (String, String)
readProblem' dir = do
  expected <- readFile $ dir ++ ".csv"
  query    <- readFile $ dir ++ ".cql"
  return (expected, query)
