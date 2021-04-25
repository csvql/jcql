module InterpreterSpec
  ( testInterpreter
  ) where
import           Data.Map                       ( Map
                                                , empty
                                                , fromList
                                                , lookup
                                                , singleton
                                                , toList
                                                )
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck         as QC

import           AST
import           Data.Maybe
import           Interpreter
import AST (TableValue(TableRef))

testInterpreter :: TestTree
testInterpreter = testGroup
  "Interpreter"
  [ testGroup
    "importing CSV"
    [ testGroup
      "valid CSV"
      [ testCase "Simple Aliased Import" $ do
        csv <- importCSV [AliasedImport "user" "./testCsv/user.csv"]
        csv @?= Ok (singleton "user" users)
      , testCase "Simple Unaliased Import" $ do
        csv <- importCSV [UnaliasedImport "./testCsv/user.csv"]
        csv @?= Ok (singleton "user" users)
      ]
    , testGroup
      "multipleCSV's"
      [ testCase "Two Aliased Imports" $ do
        csv <- importCSV
          [ AliasedImport "user"    "./testCsv/user.csv"
          , AliasedImport "country" "./testCsv/country.csv"
          ]
        csv @?= Ok (fromList [("user", users), ("country", countries)])
      , testCase "Two Unaliased Imports" $ do
        csv <- importCSV
          [ UnaliasedImport "./testCsv/user.csv"
          , UnaliasedImport "./testCsv/country.csv"
          ]
        csv @?= Ok (fromList [("user", users), ("country", countries)])
      ]
    , testGroup
      "Invalid CSV"
      [ testCase "Import with a weird name" $ do
          csv <- importCSV [UnaliasedImport "./testCsv/b$d.csv"]
          let throwsError = case csv of
                Ok    _ -> False
                Error _ -> True
          throwsError @?= True
      ]
    ]
  , testGroup
    "evalExpr"
    [ testGroup
      "TableColumn"
      [ testCase "existing table column"
        $   evalExpr (TableColumn "A" 0) (singleton "A" ["den", "is"])
        @?= Ok (ValueString "den")
      -- , testCase "missing table column"
      -- $   evalExpr (TableColumn "A" 2) (singleton "A" ["den", "is"])
      -- @?= 
      -- , testCase "missing table"
      -- $   evalExpr (TableColumn "B" 0) (singleton "A" ["den", "is"])
      -- @?= 
      ]
    , testGroup
      "BinaryOpExpr"
      [ testGroup
        "EQ"
        [ testCase "String EQ String (same value) returns true"
        $   evalExpr
              (BinaryOpExpr (ValueExpr (ValueString "a"))
                            AST.EQ
                            (ValueExpr (ValueString "a"))
              )
              empty
        @?= Ok (ValueBool True)
        , testCase "String EQ String (different values) returns false"
        $   evalExpr
              (BinaryOpExpr (ValueExpr (ValueString "a"))
                            AST.EQ
                            (ValueExpr (ValueString "b"))
              )
              empty
        @?= Ok (ValueBool False)

          -- The part for type testing made by Aleksei
        , testCase "Bool EQ Bool (same value) returns true"
        $   evalExpr
              (BinaryOpExpr (ValueExpr (ValueBool True))
                            AST.EQ
                            (ValueExpr (ValueBool True))
              )
              empty
        @?= Ok (ValueBool True)
        , testCase "Bool EQ Bool (different values) returns false"
        $   evalExpr
              (BinaryOpExpr (ValueExpr (ValueBool True))
                            AST.EQ
                            (ValueExpr (ValueBool False))
              )
              empty
        @?= Ok (ValueBool False)
        , testCase "Int EQ Int (same value) returns true"
        $   evalExpr
              (BinaryOpExpr (ValueExpr (ValueInt 1))
                            AST.EQ
                            (ValueExpr (ValueInt 1))
              )
              empty
        @?= Ok (ValueBool True)
        , testCase "Int EQ Int (different values) returns false"
        $   evalExpr
              (BinaryOpExpr (ValueExpr (ValueInt 1))
                            AST.EQ
                            (ValueExpr (ValueInt 2))
              )
              empty
        @?= Ok (ValueBool False)
        , testCase "String EQ Boolean returns an error"
        $   evalErr
              (BinaryOpExpr (ValueExpr (ValueString "a"))
                            AST.EQ
                            (ValueExpr (ValueBool True))
              )
              empty
        @?= True
        , testCase "String EQ Integer returns an error"
        $   evalErr
              (BinaryOpExpr (ValueExpr (ValueString "a"))
                            AST.EQ
                            (ValueExpr (ValueInt 3))
              )
              empty
        @?= True
        , testCase "Boolean EQ Integer returns an error"
        $   evalErr
              (BinaryOpExpr (ValueExpr (ValueBool True))
                            AST.EQ
                            (ValueExpr (ValueInt 3))
              )
              empty
        @?= True
        ]
      , testGroup
        "LT"
        [ testCase "String LT String (same value) returns false"
        $   evalExpr
              (BinaryOpExpr (ValueExpr (ValueString "a"))
                            AST.LT
                            (ValueExpr (ValueString "a"))
              )
              empty
        @?= Ok (ValueBool False)
        , testCase "String LT String (different values) returns true"
        $   evalExpr
              (BinaryOpExpr (ValueExpr (ValueString "a"))
                            AST.LT
                            (ValueExpr (ValueString "b"))
              )
              empty
        @?= Ok (ValueBool True)

      -- The part for type testing made by Aleksei
        , testCase "Bool GT Bool returns error"
        $   evalErr
              (BinaryOpExpr (ValueExpr (ValueBool True))
                            AST.LT
                            (ValueExpr (ValueBool True))
              )
              empty
        @?= True
        , testCase "Int GT Int (same value) returns false"
        $   evalExpr
              (BinaryOpExpr (ValueExpr (ValueInt 1))
                            AST.LT
                            (ValueExpr (ValueInt 1))
              )
              empty
        @?= Ok (ValueBool False)
        , testCase "Int LT Int (different values) returns true"
        $   evalExpr
              (BinaryOpExpr (ValueExpr (ValueInt 1))
                            AST.LT
                            (ValueExpr (ValueInt 2))
              )
              empty
        @?= Ok (ValueBool True)
        , testCase "String LT Boolean returns an error"
        $   evalErr
              (BinaryOpExpr (ValueExpr (ValueString "a"))
                            AST.LT
                            (ValueExpr (ValueBool True))
              )
              empty
        @?= True
        , testCase "String LT Integer returns an error"
        $   evalErr
              (BinaryOpExpr (ValueExpr (ValueString "a"))
                            AST.LT
                            (ValueExpr (ValueInt 3))
              )
              empty
        @?= True
        , testCase "Boolean LT Integer returns an error"
        $   evalErr
              (BinaryOpExpr (ValueExpr (ValueBool True))
                            AST.LT
                            (ValueExpr (ValueInt 3))
              )
              empty
        @?= True
        ]
      , testGroup
        "LEQ"
        [ testCase "String LEQ String (same value) returns true"
        $   evalExpr
              (BinaryOpExpr (ValueExpr (ValueString "a"))
                            AST.LEQ
                            (ValueExpr (ValueString "a"))
              )
              empty
        @?= Ok (ValueBool True)
        , testCase "String LEQ String (different values) returns true"
        $   evalExpr
              (BinaryOpExpr (ValueExpr (ValueString "a"))
                            AST.LEQ
                            (ValueExpr (ValueString "b"))
              )
              empty
        @?= Ok (ValueBool True)
        , testCase "Bool LEQ Bool returns error"
        $   evalErr
              (BinaryOpExpr (ValueExpr (ValueBool True))
                            AST.LEQ
                            (ValueExpr (ValueBool True))
              )
              empty
        @?= True
        , testCase "Int LEQ Int (same value) returns true"
        $   evalExpr
              (BinaryOpExpr (ValueExpr (ValueInt 1))
                            AST.LEQ
                            (ValueExpr (ValueInt 1))
              )
              empty
        @?= Ok (ValueBool True)
        , testCase "Int LEQ Int (different values) returns true"
        $   evalExpr
              (BinaryOpExpr (ValueExpr (ValueInt 1))
                            AST.LEQ
                            (ValueExpr (ValueInt 2))
              )
              empty
        @?= Ok (ValueBool True)
        , testCase "String LEQ Boolean returns an error"
        $   evalErr
              (BinaryOpExpr (ValueExpr (ValueString "a"))
                            AST.LEQ
                            (ValueExpr (ValueBool True))
              )
              empty
        @?= True
        , testCase "String LEQ Integer returns an error"
        $   evalErr
              (BinaryOpExpr (ValueExpr (ValueString "a"))
                            AST.LEQ
                            (ValueExpr (ValueInt 3))
              )
              empty
        @?= True
        , testCase "Boolean LEQ Integer returns an error"
        $   evalErr
              (BinaryOpExpr (ValueExpr (ValueBool True))
                            AST.LEQ
                            (ValueExpr (ValueInt 3))
              )
              empty
        @?= True
        ]
      , testGroup
        "GT"
        [ testCase "String GT String (same value) returns false"
        $   evalExpr
              (BinaryOpExpr (ValueExpr (ValueString "a"))
                            AST.GT
                            (ValueExpr (ValueString "a"))
              )
              empty
        @?= Ok (ValueBool False)
        , testCase "String GT String (different values) returns true"
        $   evalExpr
              (BinaryOpExpr (ValueExpr (ValueString "b"))
                            AST.GT
                            (ValueExpr (ValueString "a"))
              )
              empty
        @?= Ok (ValueBool True)
        , testCase "Bool GT Bool returns error"
        $   evalErr
              (BinaryOpExpr (ValueExpr (ValueBool True))
                            AST.GT
                            (ValueExpr (ValueBool True))
              )
              empty
        @?= True
        , testCase "Int GT Int (same value) returns false"
        $   evalExpr
              (BinaryOpExpr (ValueExpr (ValueInt 1))
                            AST.GT
                            (ValueExpr (ValueInt 1))
              )
              empty
        @?= Ok (ValueBool False)
        , testCase "Int GT Int (different values) returns true"
        $   evalExpr
              (BinaryOpExpr (ValueExpr (ValueInt 2))
                            AST.GT
                            (ValueExpr (ValueInt 1))
              )
              empty
        @?= Ok (ValueBool True)
        , testCase "String GT Boolean returns an error"
        $   evalErr
              (BinaryOpExpr (ValueExpr (ValueString "a"))
                            AST.GT
                            (ValueExpr (ValueBool True))
              )
              empty
        @?= True
        , testCase "String GT Integer returns an error"
        $   evalErr
              (BinaryOpExpr (ValueExpr (ValueString "a"))
                            AST.GT
                            (ValueExpr (ValueInt 3))
              )
              empty
        @?= True
        , testCase "Boolean GT Integer returns an error"
        $   evalErr
              (BinaryOpExpr (ValueExpr (ValueBool True))
                            AST.GT
                            (ValueExpr (ValueInt 3))
              )
              empty
        @?= True
        ]
      , testGroup
        "GEQ"
        [ testCase "String GEQ String (same value) returns true"
        $   evalExpr
              (BinaryOpExpr (ValueExpr (ValueString "a"))
                            AST.GEQ
                            (ValueExpr (ValueString "a"))
              )
              empty
        @?= Ok (ValueBool True)
        , testCase "String GEQ String (different values) returns true"
        $   evalExpr
              (BinaryOpExpr (ValueExpr (ValueString "b"))
                            AST.GEQ
                            (ValueExpr (ValueString "a"))
              )
              empty
        @?= Ok (ValueBool True)
        , testCase "Bool GEQ Bool returns error"
        $   evalErr
              (BinaryOpExpr (ValueExpr (ValueBool True))
                            AST.GEQ
                            (ValueExpr (ValueBool True))
              )
              empty
        @?= True
        , testCase "Int GEQ Int (same value) returns true"
        $   evalExpr
              (BinaryOpExpr (ValueExpr (ValueInt 1))
                            AST.GEQ
                            (ValueExpr (ValueInt 1))
              )
              empty
        @?= Ok (ValueBool True)
        , testCase "Int GEQ Int (different values) returns true"
        $   evalExpr
              (BinaryOpExpr (ValueExpr (ValueInt 2))
                            AST.GEQ
                            (ValueExpr (ValueInt 1))
              )
              empty
        @?= Ok (ValueBool True)
        , testCase "String GEQ Boolean returns an error"
        $   evalErr
              (BinaryOpExpr (ValueExpr (ValueString "a"))
                            AST.GEQ
                            (ValueExpr (ValueBool True))
              )
              empty
        @?= True
        , testCase "String GEQ Integer returns an error"
        $   evalErr
              (BinaryOpExpr (ValueExpr (ValueString "a"))
                            AST.GEQ
                            (ValueExpr (ValueInt 3))
              )
              empty
        @?= True
        , testCase "Boolean GEQ Integer returns an error"
        $   evalErr
              (BinaryOpExpr (ValueExpr (ValueBool True))
                            AST.GEQ
                            (ValueExpr (ValueInt 3))
              )
              empty
        @?= True
        ]
      , testGroup
        "AND"
        [ testCase "String AND Int same value returns error"
        $   evalErr
              (BinaryOpExpr (ValueExpr (ValueString "a"))
                            AST.AND
                            (ValueExpr (ValueInt 1))
              )
              empty
        @?= True
        , testCase "Int AND Bool same value returns error"
        $   evalErr
              (BinaryOpExpr (ValueExpr (ValueInt 1))
                            AST.AND
                            (ValueExpr (ValueBool True))
              )
              empty
        @?= True
        , testCase "True and True return True"
        $   evalExpr
              (BinaryOpExpr (ValueExpr (ValueBool True))
                            AST.AND
                            (ValueExpr (ValueBool True))
              )
              empty
        @?= Ok (ValueBool True)
        , testCase "True and False return False"
        $   evalExpr
              (BinaryOpExpr (ValueExpr (ValueBool True))
                            AST.AND
                            (ValueExpr (ValueBool False))
              )
              empty
        @?= Ok (ValueBool False)
        , testCase "False and True return False"
        $   evalExpr
              (BinaryOpExpr (ValueExpr (ValueBool False))
                            AST.AND
                            (ValueExpr (ValueBool True))
              )
              empty
        @?= Ok (ValueBool False)
        , testCase "False and False return False"
        $   evalExpr
              (BinaryOpExpr (ValueExpr (ValueBool False))
                            AST.AND
                            (ValueExpr (ValueBool False))
              )
              empty
        @?= Ok (ValueBool False)
        ]
      , testGroup
        "OR"
        [ testCase "String OR Int same value returns error"
        $   evalErr
              (BinaryOpExpr (ValueExpr (ValueString "a"))
                            AST.OR
                            (ValueExpr (ValueInt 1))
              )
              empty
        @?= True
        , testCase "Int OR Bool same value returns error"
        $   evalErr
              (BinaryOpExpr (ValueExpr (ValueInt 1))
                            AST.OR
                            (ValueExpr (ValueBool True))
              )
              empty
        @?= True
        , testCase "True OR True return True"
        $   evalExpr
              (BinaryOpExpr (ValueExpr (ValueBool True))
                            AST.OR
                            (ValueExpr (ValueBool True))
              )
              empty
        @?= Ok (ValueBool True)
        , testCase "True OR False return True"
        $   evalExpr
              (BinaryOpExpr (ValueExpr (ValueBool True))
                            AST.OR
                            (ValueExpr (ValueBool False))
              )
              empty
        @?= Ok (ValueBool True)
        , testCase "False OR True return True"
        $   evalExpr
              (BinaryOpExpr (ValueExpr (ValueBool False))
                            AST.OR
                            (ValueExpr (ValueBool True))
              )
              empty
        @?= Ok (ValueBool True)
        , testCase "False OR False return False"
        $   evalExpr
              (BinaryOpExpr (ValueExpr (ValueBool False))
                            AST.OR
                            (ValueExpr (ValueBool False))
              )
              empty
        @?= Ok (ValueBool False)
        ]
      ]
    , testGroup
      "UnaryOpExpr"
      [ testCase "Using String reutrns an error"
      $   evalErr (UnaryOpExpr AST.NOT (ValueExpr (ValueString ""))) empty
      @?= True
      , testCase "Using Int returns an error"
      $   evalErr (UnaryOpExpr AST.NOT (ValueExpr (ValueInt 1))) empty
      @?= True
      , testCase "Using True returns an False"
      $   evalExpr (UnaryOpExpr AST.NOT (ValueExpr (ValueBool True))) empty
      @?= Ok (ValueBool False)
      , testCase "Using True returns an True"
      $   evalExpr (UnaryOpExpr AST.NOT (ValueExpr (ValueBool False))) empty
      @?= Ok (ValueBool True)
      ]
    , testGroup
      "Function"
      [ testCase "coalesce returns first non null value"
      $   evalExpr
            (Function
              "COALESCE"
              [ ValueExpr (ValueString "")
              , ValueExpr (ValueString "")
              , ValueExpr (ValueString "wassup")
              ]
            )
            empty
      @?= Ok (ValueString "wassup")
      , testCase "coalesce with all null values returns null"
      $   evalExpr
            (Function "COALESCE"
                      [ValueExpr (ValueString ""), ValueExpr (ValueString "")]
            )
            empty
      @?= Ok (ValueString "")
      ]
    , testGroup
      "Case"
      [ testCase "case statement with multiple types returns an error"
      $   evalErr
            ( Case
                [ (ValueExpr $ ValueBool True , ValueExpr $ ValueString "Den")
                , (ValueExpr $ ValueBool False, ValueExpr $ ValueInt 3)
                ]
            $ ValueExpr (ValueString "is")
            )
            empty
      @?= True
      , testCase
        "case statement with multiple types returns an error (the different one is the last one)"
      $   evalErr
            ( Case
                [ (ValueExpr $ ValueBool True , ValueExpr $ ValueString "Den")
                , (ValueExpr $ ValueBool False, ValueExpr $ ValueString "is")
                , (ValueExpr $ ValueBool False, ValueExpr $ ValueString "Great")
                , (ValueExpr $ ValueBool False, ValueExpr $ ValueString "Awesome")
                , (ValueExpr $ ValueBool False, ValueExpr $ ValueString "Yappy")
                ]
            $ ValueExpr (ValueInt 0)
            )
            empty
      @?= True
      , testCase "case statement returns true statement"
      $   evalExpr
            ( Case
                [ (ValueExpr $ ValueBool False, ValueExpr $ ValueString "Den")
                , (ValueExpr $ ValueBool False, ValueExpr $ ValueString "is")
                , (ValueExpr $ ValueBool True , ValueExpr $ ValueString "Great")
                , (ValueExpr $ ValueBool False, ValueExpr $ ValueString "Awesome")
                , (ValueExpr $ ValueBool False, ValueExpr $ ValueString "Yappy")
                ]
            $ ValueExpr (ValueString "Positive")
            )
            empty
      @?= Ok (ValueString "Great")
      , testCase "case statement returns first true statement"
      $   evalExpr
            ( Case
                [ (ValueExpr $ ValueBool False, ValueExpr $ ValueString "Den")
                , (ValueExpr $ ValueBool False, ValueExpr $ ValueString "is")
                , (ValueExpr $ ValueBool True , ValueExpr $ ValueString "Great")
                , (ValueExpr $ ValueBool False, ValueExpr $ ValueString "Awesome")
                , (ValueExpr $ ValueBool True , ValueExpr $ ValueString "Yappy")
                ]
            $ ValueExpr (ValueString "Positive")
            )
            empty
      @?= Ok (ValueString "Great")
      , testCase "case statement returns else statement"
      $   evalExpr
            ( Case
                [ (ValueExpr $ ValueBool False, ValueExpr $ ValueString "Den")
                , (ValueExpr $ ValueBool False, ValueExpr $ ValueString "is")
                , (ValueExpr $ ValueBool False, ValueExpr $ ValueString "Great")
                , (ValueExpr $ ValueBool False, ValueExpr $ ValueString "Awesome")
                , (ValueExpr $ ValueBool False, ValueExpr $ ValueString "Yappy")
                ]
            $ ValueExpr (ValueString "Positive")
            )
            empty
      @?= Ok (ValueString "Positive")
      , testCase "case statement with some required evaluation returns false"
      $   evalErr
            ( Case
                [ (ValueExpr $ ValueBool False, ValueExpr $ ValueString "Den")
                , (ValueExpr $ ValueBool False, ValueExpr $ ValueString "is")
                , ( ValueExpr $ ValueBool False
                  , BinaryOpExpr (ValueExpr $ ValueInt 1)
                                 AST.LT
                                 (ValueExpr $ ValueInt 3)
                  )
                , (ValueExpr $ ValueBool False, ValueExpr $ ValueString "Awesome")
                , (ValueExpr $ ValueBool False, ValueExpr $ ValueString "Yappy")
                ]
            $ ValueExpr (ValueString "Positive")
            )
            empty
      @?= True
      , testCase
        "case statement with some required evaluation returns correct value"
      $   evalExpr
            ( Case
                [ ( BinaryOpExpr (ValueExpr $ ValueBool True)
                                 AST.AND
                                 (ValueExpr $ ValueBool False)
                  , BinaryOpExpr (ValueExpr $ ValueInt 1)
                                 AST.LT
                                 (ValueExpr $ ValueInt 3)
                  )
                , ( BinaryOpExpr (ValueExpr $ ValueBool True)
                                 AST.AND
                                 (ValueExpr $ ValueBool False)
                  , BinaryOpExpr (ValueExpr $ ValueInt 4)
                                 AST.LT
                                 (ValueExpr $ ValueInt 5)
                  )
                , ( BinaryOpExpr (ValueExpr $ ValueBool True)
                                 AST.AND
                                 (ValueExpr $ ValueBool False)
                  , BinaryOpExpr (ValueExpr $ ValueInt 6)
                                 AST.LT
                                 (ValueExpr $ ValueInt 7)
                  )
                , ( BinaryOpExpr (ValueExpr $ ValueBool True)
                                 AST.AND
                                 (ValueExpr $ ValueBool True)
                  , BinaryOpExpr (ValueExpr $ ValueInt 9)
                                 AST.LT
                                 (ValueExpr $ ValueInt 8)
                  )
                , ( ValueExpr $ ValueBool False
                  , BinaryOpExpr (ValueExpr $ ValueInt 1)
                                 AST.LT
                                 (ValueExpr $ ValueInt 2)
                  )
                ]
            $ ValueExpr (ValueBool True)
            )
            empty
      @?= Ok (ValueBool False)
      ]
    ]
  , testGroup
    "Internals"
    [ testGroup
        "join"
        [ testCase "cross join"
        $   crossJoin
              [ fromList [("A", ["Hi", "Dear"])]
              , fromList [("A", ["Welcome", "Kind"])]
              , fromList [("A", ["Greetings", "My"])]
              ]
              [fromList [("B", ["Earth"])], fromList [("B", ["World"])]]
        @?= [ fromList [("A", ["Hi", "Dear"]), ("B", ["Earth"])]
            , fromList [("A", ["Hi", "Dear"]), ("B", ["World"])]
            , fromList [("A", ["Welcome", "Kind"]), ("B", ["Earth"])]
            , fromList [("A", ["Welcome", "Kind"]), ("B", ["World"])]
            , fromList [("A", ["Greetings", "My"]), ("B", ["Earth"])]
            , fromList [("A", ["Greetings", "My"]), ("B", ["World"])]
            ]
        -- , testCase "left join"
        -- $   leftJoin
        --       users
        --       countries
        --       (BinaryOpExpr (TableColumn "user" 2)
        --                     AST.EQ
        --                     (TableColumn "country" 0)
        --       )
        -- @?= [ fromList
        --       [("country", ["GB", "66"]), ("user", ["1", "Ryan", "GB"])]
        --     , fromList [("country", ["PL", "38"]), ("user", ["2", "Dom", "PL"])]
        --     , fromList [("user", ["3", "Aleksei", "RU"])]
        --     ]
        , testCase "inner join"
        $   innerJoin
              users
              countries
              (BinaryOpExpr (TableColumn "user" 2)
                            AST.EQ
                            (TableColumn "country" 0)
              )
        @?= [ fromList
              [("country", ["GB", "66"]), ("user", ["1", "Ryan", "GB"])]
            , fromList [("country", ["PL", "38"]), ("user", ["2", "Dom", "PL"])]
            ]
        , testCase "Multiple Joins"
        $   performJoins
              tableMap
              users
              [ Inner
                (TableRef "user")
                (BinaryOpExpr (TableColumn "user" 1) AST.EQ (TableColumn "user" 1)
                )
              , Inner
                (TableRef "country")
                (BinaryOpExpr (TableColumn "user" 2)
                              AST.EQ
                              (TableColumn "country" 0)
                )
              ]
        @?= [ fromList
              [ ("user"   , ["1", "Ryan", "GB"])
              , ("user"   , ["1", "Ryan", "GB"])
              , ("country", ["GB", "66"])
              ]
            , fromList
              [ ("user"   , ["2", "Dom", "PL"])
              , ("user"   , ["2", "Dom", "PL"])
              , ("country", ["PL", "38"])
              ]
            ]
        , testCase "findRow valid query"
        $   findRow
              users
              (BinaryOpExpr (TableColumn "user" 0)
                            AST.EQ
                            (ValueExpr (ValueString "2"))
              )
        @?= Just (users !! 1)
        , testCase "findRow not found"
        $   findRow
              users
              (BinaryOpExpr (TableColumn "user" 0)
                            AST.EQ
                            (ValueExpr (ValueString "69"))
              )
        @?= Nothing
        ]
        -- testCase "nested table" $ 
    ]
  , testGroup
    "Filter"
    [ testGroup
      "Simple Filters"
      [ testCase "Simply filter everything"
        $ case filterTable joinedTable (ValueExpr (ValueBool True)) of
            Ok    table -> table @?= joinedTable
            Error _     -> assertFailure "Failed to return table"
      , testCase "Simply remove everything"
        $ case filterTable joinedTable (ValueExpr (ValueBool False)) of
            Ok    table -> table @?= []
            Error _     -> assertFailure "Failed to return table"
      ]
    , testGroup
      "More Complicated Filters" -- TODO: Finish this test, the result will be wrong
      [ testCase
            "Filtering rows that have same 0th column matching in 2 joined tables"
          $ case
              filterTable
                aJoinC
                (BinaryOpExpr (TableColumn "A" 0) AST.EQ (TableColumn "C" 0))
            of
              Ok table ->
                table
                  @?= [ fromList [("A", ["Hi", "Dear"]), ("C", ["Hi", "Bye"])]
                      , fromList
                        [("A", ["Welcome", "Kind"]), ("C", ["Welcome", "Not"])]
                      ]
              Error a ->
                assertFailure $ "Failed to return table, got error: " ++ a
      ]
    , testGroup
      "Wrong Type Testing"
      [ testCase "Trying to filter with string value for condition"
        $ case filterTable joinedTable (ValueExpr (ValueString "Test")) of
            Ok    _ -> assertFailure "There should be no resulting table"
            Error _ -> True @?= True
      , testCase "Trying to filter with integer value for condition"
        $ case filterTable joinedTable (ValueExpr (ValueInt 3)) of
            Ok    _ -> assertFailure "There should be no resulting table"
            Error _ -> True @?= True
      ]
    ]
  , testGroup
    "Select"
    [ testGroup
      "Selecting Single Selections"
      [ testCase "Wildcard" $ case select [Wildcard] joinedTable of
        Ok    result -> result @?= convert joinedTable
        Error _      -> assertFailure "Failed to return selection"
      , testCase "QualifiedWildcard for table A"
        $ case select [QualifiedWildcard "A"] joinedTable of
            Ok    result -> result @?= convert tableA
            Error _      -> assertFailure "Failed to return selection"
      , testCase "QualifiedWildcard for table B"
        $ case select [QualifiedWildcard "B"] joinedTable of
            Ok    result -> result @?= convert tableB
            Error _      -> assertFailure "Failed to return selection"
      , testGroup
        "Testing different expressions (correct)"
        [ testCase "Picking a particular column"
          $ case select [SelectExpr (TableColumn "A" 0)] joinedTable of
              Ok result -> result @?= convert
                [ fromList [("A", ["Hi"])]
                , fromList [("A", ["Hi"])]
                , fromList [("A", ["Welcome"])]
                , fromList [("A", ["Welcome"])]
                , fromList [("A", ["Greetings"])]
                , fromList [("A", ["Greetings"])]
                ]
              Error _ -> assertFailure "Failed to return selection"
        , testCase "Just an arbitrary string column"
          $ case
              select [SelectExpr (ValueExpr (ValueString "0"))] joinedTable
            of
              Ok result ->
                result
                  @?= [ ["0"]
                      , ["0"]
                      , ["0"]
                      , ["0"]
                      , ["0"]
                      , ["0"]
                      ]
              Error _ -> assertFailure "Failed to return selection"
        , testCase "COALESCE function" -- Note that I'm not testing the actual COALESCE function, just in this context
          $ case
              select
                [ SelectExpr
                    (Function "COALESCE" [TableColumn "A" 0, TableColumn "B" 0])
                ]
                joinedTable
            of
              Ok result -> result @?= convert
                [ fromList [("A", ["Hi"])]
                , fromList [("A", ["Hi"])]
                , fromList [("A", ["Welcome"])]
                , fromList [("A", ["Welcome"])]
                , fromList [("A", ["Greetings"])]
                , fromList [("A", ["Greetings"])]
                ]
        ]
      , testGroup
        "Testing different expressions (invalid Type)"
        [ testCase "Boolean Value used in expression"
          $ case select [SelectExpr (ValueExpr (ValueBool True))] joinedTable of
              Ok    _ -> assertFailure "Can't allow boolean type"
              Error _ -> True @?= True
        , testCase "Integer Value used in expression"
          $ case select [SelectExpr (ValueExpr (ValueInt 3))] joinedTable of
              Ok    _ -> assertFailure "Can't allow integer type"
              Error _ -> True @?= True
        ]
      ]
    , testGroup
      "Multiple Selections"
      [ testCase "Multiple Wildcards"
        $ case select [Wildcard, Wildcard] joinedTable of
            Ok    result -> result @?= convert' joinedTable
            Error _      -> assertFailure "Failed to return selection"
      , testCase "Multiple Qualified Wildcards"
        $ case
            select [QualifiedWildcard "A", QualifiedWildcard "B"] joinedTable
          of
            Ok    result -> result @?= convert'' tableA tableB
            Error _      -> assertFailure "Failed to return selection"
      , testGroup
        "Multiple Expressions Correct"
        [ testCase "Multiple Column selections"
          $ case
              select
                [SelectExpr (TableColumn "A" 0), SelectExpr (TableColumn "B" 0)]
                joinedTable
            of
              Ok result ->
                result
                  @?= convert''
                        [ fromList [("A", ["Hi"])]
                        , fromList [("A", ["Hi"])]
                        , fromList [("A", ["Welcome"])]
                        , fromList [("A", ["Welcome"])]
                        , fromList [("A", ["Greetings"])]
                        , fromList [("A", ["Greetings"])]
                        ]
                        tableB
              Error _ -> assertFailure "Failed to return selection"
        , testCase "Multiple arbitrary Strings"
          $ case
              select
                [ SelectExpr (ValueExpr (ValueString "OOO"))
                , SelectExpr (ValueExpr (ValueString "NOO"))
                ]
                joinedTable
            of
              Ok result ->
                result
                  @?= [ ["OOO", "NOO"]
                      , ["OOO", "NOO"]
                      , ["OOO", "NOO"]
                      , ["OOO", "NOO"]
                      , ["OOO", "NOO"]
                      , ["OOO", "NOO"]
                      ]
              Error _ -> assertFailure "Failed to return selection"
        ]
      ]
    ]
  ]

-- TODO: innerjoin has to be based on equating both expressions values!
users :: [Row]
users =
  [ fromList [("user", ["1", "Ryan", "GB"])]
  , fromList [("user", ["2", "Dom", "PL"])]
  , fromList [("user", ["3", "Aleksei", "RU"])]
  ]

tableMap = fromList [("user", users), ("country", countries)]

countries =
  [fromList [("country", ["GB", "66"])], fromList [("country", ["PL", "38"])]]

convert :: [Row] -> [[String]]
convert [] = []
convert (row : rows) =
  [ value | value <- (concatMap snd . toList) row ] : convert rows

convert' :: [Row] -> [[String]]
convert' []           = []
convert' (row : rows) = (conversion ++ conversion) : convert' rows
 where
  conversion = [ value | value <- (concatMap snd . toList) row ]

convert'' :: [Row] -> [Row] -> [[String]]
convert'' [] _ = []
convert'' (row' : rows') (row'' : rows'') =
  (conversion' ++ conversion'') : convert'' rows' rows''
 where
  conversion' = [ value | value <- (concatMap snd . toList) row' ]
  conversion'' =
    [ value | value <- (concatMap snd . toList) row'' ]


tableA =
  [ fromList [("A", ["Hi", "Dear"])]
  , fromList [("A", ["Hi", "Dear"])]
  , fromList [("A", ["Welcome", "Kind"])]
  , fromList [("A", ["Welcome", "Kind"])]
  , fromList [("A", ["Greetings", "My"])]
  , fromList [("A", ["Greetings", "My"])]
  ]
tableC =
  [fromList [("C", ["Hi", "Bye"])], fromList [("C", ["Welcome", "Not"])]]

aJoinC =
  [ fromList [("A", ["Hi", "Dear"]), ("C", ["Hi", "Bye"])]
  , fromList [("A", ["Hi", "Dear"]), ("C", ["Welcome", "Not"])]
  , fromList [("A", ["Welcome", "Kind"]), ("C", ["Hi", "Bye"])]
  , fromList [("A", ["Welcome", "Kind"]), ("C", ["Welcome", "Not"])]
  , fromList [("A", ["Greetings", "My"]), ("C", ["Hi", "Bye"])]
  , fromList [("A", ["Greetings", "My"]), ("C", ["Welcome", "Not"])]
  ]

tableB =
  [ fromList [("B", ["Earth"])]
  , fromList [("B", ["World"])]
  , fromList [("B", ["Earth"])]
  , fromList [("B", ["World"])]
  , fromList [("B", ["Earth"])]
  , fromList [("B", ["World"])]
  ]


joinedTable =
  [ fromList [("A", ["Hi", "Dear"]), ("B", ["Earth"])]
  , fromList [("A", ["Hi", "Dear"]), ("B", ["World"])]
  , fromList [("A", ["Welcome", "Kind"]), ("B", ["Earth"])]
  , fromList [("A", ["Welcome", "Kind"]), ("B", ["World"])]
  , fromList [("A", ["Greetings", "My"]), ("B", ["Earth"])]
  , fromList [("A", ["Greetings", "My"]), ("B", ["World"])]
  ]


evalErr :: Expr -> Row -> Bool
evalErr expr row = case evalExpr expr row of
  Ok    _ -> False
  Error _ -> True

-- evalExprErr :: Expr -> Row -> String
-- evalExprErr expr row = case evalExpr expr row of
--   Ok _ -> ""
--   Error e -> ""
