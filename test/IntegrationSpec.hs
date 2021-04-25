module IntegrationSpec
  ( testIntegration
  ) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Interpreter
import           Lex
import           Parse

import           Data.Map

eval :: String -> IO (Result [[String]])
eval s = do
  table <- (evalRoot . parseJCQL . alexScanTokens) s
  return $ singleTable table

singleTable :: Result Table -> Result [[String]]
singleTable res = do
  Prelude.map (head . elems) <$> res

testIntegration :: TestTree
testIntegration = testGroup
  "Integration"
  [ testCase "pro developers" $ do
      out <-
        eval
          "import a './testCsv/user.csv' take a where not (a.2 = 'Aleksei') select a.2, a.3"
      out @?= Ok [["Ryan", "GB"], ["Dom", "PL"]]
  ]

