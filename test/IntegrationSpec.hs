module IntegrationSpec
  ( testIntegration
  ) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Interpreter
import           Lex
import           Parse

import           Data.Map

eval :: String -> IO (Result Table)
eval = evalRoot . parseJCQL . alexScanTokens

testIntegration :: TestTree
testIntegration = testGroup
  "Integration"
  [ testCase "pro developers" $ do
      out <- eval "import a './testCsv/user.csv' take a where not (a.2 = 'Aleksei') select a.2"
      out @?= Ok
        [ fromList [("a", ["Ryan"])]
        , fromList [("a", ["Dom"])]
        ]
  ]
