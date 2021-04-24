{-# LANGUAGE LambdaCase #-}
module Interpreter where

import           AST
import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Map                       ( Map
                                                , empty
                                                , findWithDefault
                                                , fromList
                                                , keys
                                                , lookup
                                                , singleton
                                                , toList
                                                , unionWith
                                                )
import           Data.Maybe

-- Row defines a single row
type Row = Map Identifier [String]
type Table = [Row]
type TableMap = Map Identifier [Row]
type Column = (Int, [String])
-- type DecodedSelect = [TableContents] -> [String]

-- Result Monad Implementation
data Result v =
  Ok v
  | Error String
  deriving (Eq, Show)

-- Making Result instance of a Monad
----------------------------------------------------
instance  Functor Result  where
  fmap _ (Error e) = Error e
  fmap f (Ok    a) = Ok (f a)

instance Applicative Result where
  pure v = Error ""

  Ok    f <*> m  = fmap f m
  Error s <*> _m = Error s


instance Monad Result where
  (Error s) >>= f = Error s
  (Ok    v) >>= f = f v
  return v = Ok v

----------------------------------------------------
-- TODO: this is an obsolete because importCSV has changes since
-- I'll leave it here in case there is something useful that can be used 

-- evalQuery :: Query -> IO [[String]]
-- -- TODO: convert imports to a Map
-- evalQuery (AST imports table joins (Just filter) select) = do
--   tables <- importCSV imports
--   let tableMap      = fromList tables
--       chosenTable   = Data.Map.lookup table tableMap
--       joinedTable   = performJoins tableMap joinedTable joins
--       filteredTable = filterTable joinedTable filter >>= \x -> return x
--   return [[]]

-- Parsing CSV's
----------------------------------------------------
-- Importing the given csv files into list of pairs containing the name of the file 
-- and the unparsed csv. The name is either derived or used if explicitly stated
importCSV :: [Import] -> IO [Result (Identifier, [Row])]
importCSV []                              = return []
importCSV ((AliasedImport id loc) : rest) = do
  file <- readFile loc
  let rows = createRows (unparseCsv file) id
  rest <- importCSV rest
  return $ Ok (id, rows) : rest
importCSV ((UnaliasedImport loc) : rest) = do
  file <- readFile loc
  let id   = takeWhile (/= '.') (last $ splitOn "/" loc)
      rows = createRows (unparseCsv file) id
  rest <- importCSV rest
  let verifiedPair =
        checkCharSet id >>= \verifiedId -> return (verifiedId, rows)
  return $ verifiedPair : rest

-- Checking that the name of the file doesn't contain any illegal characters
-- Note that the name won't have any . or / since these are used to get the name in the importCSV
checkCharSet :: String -> Result String
checkCharSet [] = return []
checkCharSet (c : cs)
  | isAlpha c || c == '_' || isNumber c = do
    rest <- checkCharSet cs
    return (c : rest)
  | otherwise = Error "illegal characters used in the import"

-- Simple parsing of read CSV into 2D array
unparseCsv :: String -> [[String]]
unparseCsv l =
  let line = lines l
  in  let lists = map (splitOn ",") line in map (map trim) lists

-- Covert 2D array into a list of rows
createRows :: [[String]] -> Identifier -> [Row]
createRows table name = [ fromList [(name, row)] | row <- table ]

-- Remove any trailing space
trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace
----------------------------------------------------

-- Joins
-- TODO: update it to use the Result Monad
----------------------------------------------------
performJoins :: TableMap -> Table -> [Join] -> Table
performJoins _      final []                     = final
performJoins tables table (Inner id exp : joins) = performJoins tables
                                                                join
                                                                joins
  where join = innerJoin table (fromJust $ Data.Map.lookup id tables) exp
performJoins tables table (Cross id : joins) = performJoins tables join joins
  where join = crossJoin table (fromJust $ Data.Map.lookup id tables)

-- crossJoin joins two tables together
crossJoin :: [Row] -> [Row] -> [Row]
crossJoin xs ys = [ x `mergeMap` y | x <- xs, y <- ys ]

-- inner join joins two tables only if Expr evaluates to True
innerJoin :: [Row] -> [Row] -> Expr -> [Row]
innerJoin t1 t2 e =
  [ fromJust row
  | r <- t1
  , let row = findRow [ a `mergeMap` r | a <- t2 ] e
  , isJust row
  ]
----------------------------------------------------

-- Filter
----------------------------------------------------
-- TODO, important that only certain types of filters should be allowed
filterTable :: Table -> Expr -> Result Table
filterTable table expr = do
  evaledExpr <- unwrap $ map (evalExpr expr) table
  zipped     <- unwrap $ zipWith (curry validityCheck) evaledExpr table
  return $ [ row | (validity, row) <- zipped, validity ]

-- Basically unwraps a pair for simplicity sake
validityCheck :: (Value, Row) -> Result (Bool, Row)
validityCheck (val, row) = do
  valid <- isValid val
  return (valid, row)

-- Only allows the condition in filtering to be of boolean value after evaluation
isValid :: Value -> Result Bool
isValid (ValueBool True ) = Ok True
isValid (ValueBool False) = Ok False
isValid _                 = Error "Typing Error"
----------------------------------------------------

-- Selection
----------------------------------------------------
select :: Select -> Table -> Result [[Value]]
select items []           = Ok []
select items (row : rows) = do
  let exprs = map (unwrapSelect row) items
  newRow <- unwrap $ map (performSelect row) exprs
  rest   <- select items rows
  return (concat newRow : rest)

-- Convert the select statements to expressions for simplicity
unwrapSelect :: Row -> SelectItem -> [Expr]
unwrapSelect row (QualifiedWildcard name) =
  [ TableColumn name index
  | index <- [0 .. length (fromJust $ Data.Map.lookup name row) - 1]
  ]
unwrapSelect _   (SelectExpr e) = [e]
unwrapSelect row Wildcard       = concat
  [ unwrapSelect row (QualifiedWildcard table) | table <- listTables ]
  where listTables = keys row

-- Reconstruct the row based on the results of the expressions, which are the converted select Item
performSelect :: Row -> [Expr] -> Result [Value]
performSelect _   []       = Ok []
performSelect row (e : es) = do
  value        <- evalExpr e row
  checkedValue <- checkType value
  rest         <- performSelect row es
  return (checkedValue : rest)
 where
  checkType v = case v of
    ValueString v -> Ok $ ValueString v
    _             -> Error "Selection expressions can only have string"

----------------------------------------------------

-- Evaluating expression
----------------------------------------------------
-- evalExpr evaluates an expression into a result
-- if the evaluation was successful, it returns `Ok Value`
-- invalid evalution will return `Error String`
evalExpr :: Expr -> Row -> Result Value
evalExpr expr row = case expr of
  ValueExpr v         -> Ok v
  TableColumn tbl col -> case getColumn (tbl, col) row of
    Just v  -> Ok (ValueString v)
    Nothing -> Error "table not found"
  BinaryOpExpr l op r -> do
    left  <- evalExpr l row
    right <- evalExpr r row
    evalBinaryOp op left right
  UnaryOpExpr op e -> do
    value <- evalExpr e row
    evalUnaryOp op value
  Function name args -> do
    vals <- unwrap $ map (`evalExpr` row) args
    evalFn name vals
  Case exprs def -> do
    evaled <- unwrap
      [ evalExpr a row >>= \v1 -> evalExpr b row >>= \v2 -> return (v1, v2)
      | (a, b) <- exprs
      ]
      -- [ unwrapPair (evalExpr a row, evalExpr b row) | (a, b) <- exprs ]
    evaledDef <- evalExpr def row
    _         <- typeCheckCase evaled evaledDef
    evalCase evaled evaledDef
  -- Case        cases else' -> evalCase (map (`evalExpr` row) cases) else'
----------------------------------------------------

-- Additional load of functions used for evaluating the expressions
----------------------------------------------------
-- evalBinaryOp takes a binary operator alongside two values it is applied to and evaluates it
evalBinaryOp :: BinaryOpType -> Value -> Value -> Result Value
evalBinaryOp op = case op of
  AST.EQ         -> evalEQ
  AST.LT         -> evalLT
  AST.LEQ        -> evalLEQ
  AST.GEQ        -> evalGEQ
  AST.GT         -> evalGT
  AST.AND        -> evalAND
  AST.OR         -> evalOR
  AST.Sum        -> evalSum
  AST.Difference -> evalDifference
  AST.Product    -> evalProduct

-- TODO: Further extend type error messages
evalEQ :: Value -> Value -> Result Value
evalEQ (ValueString a) (ValueString b) = return $ ValueBool (a == b)
evalEQ (ValueInt    a) (ValueInt    b) = return $ ValueBool (a == b)
evalEQ (ValueBool   a) (ValueBool   b) = return $ ValueBool (a == b)
evalEQ _               _               = Error "Type Error"

evalLEQ :: Value -> Value -> Result Value
evalLEQ (ValueString a) (ValueString b) = return $ ValueBool (a <= b)
evalLEQ (ValueInt    a) (ValueInt    b) = return $ ValueBool (a <= b)
evalLEQ _               _               = Error "Type Error"

evalLT :: Value -> Value -> Result Value
evalLT (ValueString a) (ValueString b) = return $ ValueBool (a < b)
evalLT (ValueInt    a) (ValueInt    b) = return $ ValueBool (a < b)
evalLT _               _               = Error "Type Error"

evalGT :: Value -> Value -> Result Value
evalGT (ValueString a) (ValueString b) = return $ ValueBool (a > b)
evalGT (ValueInt    a) (ValueInt    b) = return $ ValueBool (a > b)
evalGT _               _               = Error "Type Error"

evalGEQ :: Value -> Value -> Result Value
evalGEQ (ValueString a) (ValueString b) = return $ ValueBool (a >= b)
evalGEQ (ValueInt    a) (ValueInt    b) = return $ ValueBool (a >= b)
evalGEQ _               _               = Error "Type Error"

evalAND :: Value -> Value -> Result Value
evalAND (ValueBool a) (ValueBool b) = return $ ValueBool (a && b)
evalAND _             _             = Error "Type Error"

evalOR :: Value -> Value -> Result Value
evalOR (ValueBool a) (ValueBool b) = return $ ValueBool (a || b)
evalOR _             _             = Error "Type Error"

evalSum :: Value -> Value -> Result Value
evalSum (ValueInt a) (ValueInt b) = return $ ValueInt (a + b)
evalSum _            _            = Error "Type Error"

evalDifference :: Value -> Value -> Result Value
evalDifference (ValueInt a) (ValueInt b) = return $ ValueInt (a - b)
evalDifference _            _            = Error "Type Error"

evalProduct :: Value -> Value -> Result Value
evalProduct (ValueInt a) (ValueInt b) = return $ ValueInt (a * b)
evalProduct _            _            = Error "Type Error"

-- evalFn takes a unary operator and a value it is applied to and evaluates it
evalUnaryOp :: UnaryOpType -> Value -> Result Value
evalUnaryOp op = case op of
  NOT -> evalNOT

evalNOT :: Value -> Result Value
evalNOT (ValueBool a) = return $ ValueBool (not a)
evalNOT _             = Error "Type Error"
----------------------------------------------------

-- evalFn takes a function name and its arguments and evaluates it
evalFn :: String -> [Value] -> Result Value
evalFn fn args = case fn of
  "COALESCE" -> do
    let nonnull = filter (not . isNull) args
    if null nonnull then Ok (ValueString "") else Ok (head nonnull)


-- Specific type check depending on the type of the statement
typeCheckCase :: [(Value, Value)] -> Value -> Result ()
typeCheckCase ((ValueBool cond, ValueString stmt) : rest) def = typeCheckCase'
  rest
  def
 where
  typeCheckCase' [] (ValueString _) = return ()
  typeCheckCase' ((ValueBool cond, ValueString stmt) : rest) def =
    typeCheckCase' rest def
  typeCheckCase' _ _ = Error "Type Error"

typeCheckCase ((ValueBool cond, ValueBool stmt) : rest) def = typeCheckCase'
  rest
  def
 where
  typeCheckCase' [] (ValueBool _) = return ()
  typeCheckCase' ((ValueBool cond, ValueBool stmt) : rest) def =
    typeCheckCase' rest def
  typeCheckCase' _ _ = Error "Type Error"

typeCheckCase ((ValueBool cond, ValueInt stmt) : rest) def = typeCheckCase'
  rest
  def
 where
  typeCheckCase' [] (ValueBool _) = return ()
  typeCheckCase' ((ValueBool cond, ValueInt stmt) : rest) def =
    typeCheckCase' rest def
  typeCheckCase' _ _ = Error "Type Error"

typeCheckCase _ _ = Error "Type Error"


-- The function responsible for actual evaluation of the case statement
evalCase :: [(Value, Value)] -> Value -> Result Value
evalCase [] def = return def
evalCase ((ValueBool cond, stmt) : rest) def | cond      = return stmt
                                             | otherwise = evalCase rest def

-- Utility Functions

-- Simple unwrap of a list of Result values into a result list of values
unwrap :: [Result a] -> Result [a]
unwrap []       = Ok []
unwrap (r : rs) = do
  r'  <- r
  rs' <- unwrap rs
  return (r' : rs')

-- findRow finds the first row that makes the expr evalute to true
findRow :: [Row] -> Expr -> Maybe Row
findRow rows expr = do
  let filtered = filter (isTrue . evalExpr expr) rows
  if null filtered then Nothing else Just $ head filtered

-- getColumn gets a column by table name and column index (starting from 0)
getColumn :: (String, Int) -> Row -> Maybe String
getColumn (key, i) row = case Data.Map.lookup key row of
  Nothing      -> Nothing -- table name not found
  Just columns -> if i < length columns then Just (columns !! i) else Nothing

-- mergeMap merges two rows together
mergeMap :: Row -> Row -> Row
a `mergeMap` b = unionWith (++) a b

-- utility function used by COALESCE
isNull :: Value -> Bool
isNull = \case
  ValueString s -> null s
  _             -> error "isNull can only be used on strings"

-- utility used by JOIN and WHERE
isTrue :: Result Value -> Bool
isTrue r = do
  case r of
    Ok v -> case v of
      ValueBool b -> b
      _           -> False
    Error e -> False
