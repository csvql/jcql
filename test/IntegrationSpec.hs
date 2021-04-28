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
  "Integration"
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
    ]
  , testGroup "Example tests" [
    example "multiple joins"
  ]
  ]


readProblem' :: FilePath -> IO (String, String)
readProblem' dir = do
  expected <- readFile $ dir ++ ".csv"
  query    <- readFile $ dir ++ ".cql"
  return (expected, query)

