module ParserSpec
    ( testParser
    ) where
import           AST
import           AST                            ( TableValue(TableRef) )
import           Data.Char
import           Data.Maybe                     ( fromJust )
import           Lex
import           Parse
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck         as QC

testParser :: TestTree
testParser = testGroup
    "Parser"
    [ testGroup
        "imports"
        [ testCase
            "unaliased import"
            (parse "import 'a.csv' take x select a.1" @?= AST
                [UnaliasedImport "a.csv"]
                ("x", [], Nothing, [SelectExpr (TableColumn "a" 0)])
            )
        , testCase
            "aliased import"
            (parse "import A 'a.csv' take x select a.1" @?= AST
                [AliasedImport "A" "a.csv"]
                ("x", [], Nothing, [SelectExpr (TableColumn "a" 0)])
            )
        , testCase
            "multiple unaliased imports"
            (parse "import 'a.csv', 'b.csv' take x select a.1" @?= AST
                [UnaliasedImport "a.csv", UnaliasedImport "b.csv"]
                ("x", [], Nothing, [SelectExpr (TableColumn "a" 0)])
            )
        , testCase
            "multiple aliased imports"
            (parse "import B 'b.csv', A 'a.csv' take x select a.1" @?= AST
                [AliasedImport "B" "b.csv", AliasedImport "A" "a.csv"]
                ("x", [], Nothing, [SelectExpr (TableColumn "a" 0)])
            )
        ]
    , testGroup
        "take"
        [ testCase
              "take identifier"
              (parse "import 'a.csv' take A select a.1" @?= AST
                  [UnaliasedImport "a.csv"]
                  ("A", [], Nothing, [SelectExpr (TableColumn "a" 0)])
              )
        ]
    , testGroup
        "join"
        [ testCase
            "no join"
            (parse "import A 'a.csv', B 'b.csv' take A select a.1" @?= AST
                [AliasedImport "A" "a.csv", AliasedImport "B" "b.csv"]
                ("A", [], Nothing, [SelectExpr (TableColumn "a" 0)])
            )
        , testCase
            "cross join"
            (parse "import A 'a.csv', B 'b.csv' take A cross join B select a.1"
            @?= AST
                    [AliasedImport "A" "a.csv", AliasedImport "B" "b.csv"]
                    ( "A"
                    , [Cross (TableRef "B")]
                    , Nothing
                    , [SelectExpr (TableColumn "a" 0)]
                    )
            )
        , testCase
            "inner join with equals"
            (   parse
                    "import A 'a.csv', B 'b.csv' take A inner join B on A.1 = B.1 select a.1"
            @?= AST
                    [AliasedImport "A" "a.csv", AliasedImport "B" "b.csv"]
                    ( "A"
                    , [ Inner
                            (TableRef "B")
                            (BinaryOpExpr (TableColumn "A" 0)
                                          AST.EQ
                                          (TableColumn "B" 0)
                            )
                      ]
                    , Nothing
                    , [SelectExpr (TableColumn "a" 0)]
                    )
            )
        , testCase
            "inner join with equals"
            (   parse
                    "import A 'a.csv', B 'b.csv', C 'c.csv' take A inner join B on A.1 = B.1, cross join C select a.1"
            @?= AST
                    [ AliasedImport "A" "a.csv"
                    , AliasedImport "B" "b.csv"
                    , AliasedImport "C" "c.csv"
                    ]
                    ( "A"
                    , [ Cross (TableRef "C")
                      , Inner
                          (TableRef "B")
                          (BinaryOpExpr (TableColumn "A" 0)
                                        AST.EQ
                                        (TableColumn "B" 0)
                          )
                      ]
                    , Nothing
                    , [SelectExpr (TableColumn "a" 0)]
                    )
            )
        ]
    , testGroup
        "filter"
        [ testCase
            "unary operator"
            (   parse
                    "import B 'b.csv', A 'a.csv' take A cross join B where not 0 select a.1"
            @?= AST
                    [AliasedImport "B" "b.csv", AliasedImport "A" "a.csv"]
                    ( "A"
                    , [Cross (TableRef "B")]
                    , (Just (UnaryOpExpr NOT (ValueExpr (ValueInt 0))))
                    , [SelectExpr (TableColumn "a" 0)]
                    )
            )
        , testCase
            "binary operator"
            (   parse
                    "import B 'b.csv', A 'a.csv' take A cross join B where a.1 = 0 select a.1"
            @?= AST
                    [AliasedImport "B" "b.csv", AliasedImport "A" "a.csv"]
                    ( "A"
                    , [Cross (TableRef "B")]
                    , (Just
                          (BinaryOpExpr (TableColumn "a" 0)
                                        AST.EQ
                                        (ValueExpr (ValueInt 0))
                          )
                      )
                    , [SelectExpr (TableColumn "a" 0)]
                    )
            )
        , testCase
            "function"
            (   parse
                    "import B 'b.csv', A 'a.csv' take A cross join B where Coalesce(a.1,a.2) select a.1"
            @?= AST
                    [AliasedImport "B" "b.csv", AliasedImport "A" "a.csv"]
                    ( "A"
                    , [Cross (TableRef "B")]
                    , (Just
                          (Function
                              "Coalesce"
                              [TableColumn "a" 0, TableColumn "a" 1]
                          )
                      )
                    , [SelectExpr (TableColumn "a" 0)]
                    )
            )
        , testCase
            "function and unary operator"
            (   parse
                    "import B 'b.csv', A 'a.csv' take A cross join B where Len(a.1) = 0 select a.1"
            @?= AST
                    [AliasedImport "B" "b.csv", AliasedImport "A" "a.csv"]
                    ( "A"
                    , [Cross (TableRef "B")]
                    , (Just
                          (BinaryOpExpr
                              (Function "Len" [TableColumn "a" 0])
                              AST.EQ
                              (ValueExpr (ValueInt 0))
                          )
                      )
                    , [SelectExpr (TableColumn "a" 0)]
                    )
            )
        ]
    , testGroup
        "expression"
        [ testCase "arithmetic precedence"
        $   parseExpr "0+1=1+2*5"
        @?= "(0 + 1) = (1 + (2 * 5))"
        , testCase "boolean precedence"
        $   parseExpr "not true or false and true"
        @?= "(not true) or (false and true)"
        , testCase "boolean comparison precedence"
        $   parseExpr "true or false = true"
        @?= "true or (false = true)"
        , testCase "not is right associative"
        $   parseExpr "not not true"
        @?= "not (not true)"
        , testCase "division and multiplication"
        $   parseExpr "1 * 2 / 3"
        @?= "(1 * 2) / 3"
        ]
    , testGroup
        "select"
        [ testCase
            "select one column"
            (parse "import 'a.csv' take x select a.1" @?= AST
                [UnaliasedImport "a.csv"]
                ("x", [], Nothing, [SelectExpr (TableColumn "a" 0)])
            )
        , testCase
            "select multiple columns"
            (parse "import 'a.csv' take x select a.1,a.2" @?= AST
                [UnaliasedImport "a.csv"]
                ( "x"
                , []
                , Nothing
                , [ SelectExpr (TableColumn "a" 0)
                  , SelectExpr (TableColumn "a" 1)
                  ]
                )
            )
        , testCase
            "select value"
            (parse "import 'a.csv' take x select 0" @?= AST
                [UnaliasedImport "a.csv"]
                ("x", [], Nothing, [SelectExpr (ValueExpr (ValueInt 0))])
            )
        , testCase
            "select wildcard"
            (parse "import 'a.csv' take x select *" @?= AST
                [UnaliasedImport "a.csv"]
                ("x", [], Nothing, [Wildcard])
            )
        , testCase
            "select qualified wildcard"
            (parse "import 'a.csv' take x select a.*" @?= AST
                [UnaliasedImport "a.csv"]
                ("x", [], Nothing, [QualifiedWildcard "a"])
            )
        ]
    , testGroup
        "case"
        [ testCase "basic"
        $   parseExpr "case when true then 1 else 2 end"
        @?= "case when (true) then (1) else (2) end"
        , testCase "multiple branches"
        $   parseExpr "case when true then 1 when false then 3 else 2 end"
        @?= "case when (true) then (1 when (false) then (3) else (2) end"
        , testCase "nested"
        $ parseExpr "case when 1 then case when 2 then 2 else 3 end else 69 end"
        @?= "case when (1) then (case when (2) then (2) else (3) end) else (69) end"
        ]
    ]

parse :: String -> Query
parse = parseJCQL . alexScanTokens

parseExpr :: String -> String
parseExpr s = trimBrackets $ printExpr $ getExpr ast
  where
    ast = parse ("import 'a.csv' take a where " ++ s ++ " select a.1")
    getExpr (AST _ (_, _, f, _)) = fromJust f
    trimBrackets (fst : s) = if fst == '(' then init s else fst : s
