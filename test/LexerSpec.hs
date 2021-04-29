module LexerSpec
  ( testLexer
  ) where

import           Data.Char                      ( toLower
                                                , toUpper
                                                )
import           Lex                            ( AlexPosn(AlexPn)
                                                , Token(..)
                                                , alexScanTokens
                                                , str
                                                )
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( (@?=)
                                                , testCase
                                                )
import qualified Test.Tasty.QuickCheck         as QC

testLexer :: TestTree
testLexer = testGroup
  "Lexer"
  [ testCase "ignores whitespace"           (l " " @?= [])
  , testCase "ignores multiple whitespaces" (l "     " @?= [])
  , testGroup "lexes keywords" $ map
    (\input -> testCase input (l input @?= [TKeyword input startPn]))
    lexKeywords
  ,
      -- (can be assumed to work as it's handelled by alex) calculates column, position and line number correctly
      -- what about SELECTFROM? (no space). This needs to be figured out and fixed
    testGroup "lexes basic strings" $ map
    (\q ->
      testCase q
        $   l (q ++ "hello world" ++ q)
        @?= [TString "hello world" startPn]
    )
    lexStringQuotes
  , testGroup "string quotes can be escaped within string" $ map
    (\q ->
      testCase (q ++ " can be escaped with \\" ++ q)
        $   l (q ++ "hello \\" ++ q ++ "world" ++ q)
        @?= [TString ("hello \\" ++ q ++ "world") startPn]
    )
    lexStringQuotes
  , testGroup "lexes operators" $ map
    (\input -> testCase input (l input @?= [TOperator input startPn]))
    lexOperators
  , testGroup "lexes brackets" $ map
    (\input -> testCase input (l input @?= [TBracket input startPn]))
    lexBrackets
  , testGroup
    "lexes identifiers"
    [
        -- TODO throws error on invalid identifier name, e.g. `SELECT %.1`
        -- TODO throws error when starts with a number or underscore
        -- TODO throws error when contains a:
        --  - dot
        --  - dash
      testCase "alphabetic identifier"
    $   l "quickBrownFox"
    @?= [TIdentifier "quickBrownFox" startPn]
    , testCase "alphanumeric identifier"
    $   l "a1960b"
    @?= [TIdentifier "a1960b" startPn]
    , testCase "lexes methods"
    $   l ".a1960b()"
    @?= [ TDot startPn
        , TIdentifier "a1960b" (AlexPn 1 1 2)
        , TBracket "(" (AlexPn 7 1 8)
        , TBracket ")" (AlexPn 8 1 9)
        ]
    ]
  , testCase "lexes dot" $ l "." @?= [TDot startPn]
  , testCase "lexes a comma" $ l "," @?= [TComma startPn]
  , testCase "lexes asterisk" (l "*" @?= [TAsterisk startPn])
  , testCase "comments" (l "import // should import" @?= [TKeyword "import" startPn])
  ]

-- random capitalization per character is applied to create a mixed-case string
testRandomWordWithRandomCapitalization :: String -> Int -> Bool
testRandomWordWithRandomCapitalization w a = l w == l randomlyCapitalized
 where
  n = a `mod` length w
  randomlyCapitalized =
    [ if i == n then toLower (w !! i) else toUpper (w !! i)
    | i <- [0 .. length w - 1]
    ]
  expected = map toUpper w


lexKeywords =
  [ "import"
  , "take"
  , "join"
  , "inner"
  , "cross"
  , "on"
  , "case"
  , "when"
  , "then"
  , "else"
  , "end"
  , "where"
  , "select"
  , "order"
  ]


lexOperators =
  ["=", "!=", ">=", "<=", ">", "<", "not", "and", "or", "-", "+", "/"]

lexBrackets = ["(", ")", "[", "]"]

lexStringQuotes = ["\"", "\'"]

startPn = AlexPn 0 1 1

l = alexScanTokens

lexToString :: String -> String
lexToString = concatMap str . alexScanTokens
