module Main where

import           AST
import           Control.Exception
import           GHC.IO.Exception
import           Interpreter
import           Lex
import           Parse
import           System.Console.ANSI
import           System.Environment             ( getArgs )
import Data.List (intercalate)
import Eval
import System.IO

parse :: [FilePath] -> IO (Result String)
parse [f] = do
  f <- readFile f
  return $ Ok f
parse fs = return $ Error ("expected 1 argument, given " ++ show (length fs))

main :: IO ()
main = catch run (\(ErrorCall e) -> printErr e)

-- TODO: what if there is no Just but nothing instead?
argReadHandler :: IOError -> IO (Result a)
argReadHandler (IOError _ _ _ expl _ (Just loc)) =
  return $ Error ("Argument error: " ++ expl ++ " - " ++ loc)

run :: IO ()
run = do
  hSetSGR stdout [SetColor Foreground Vivid Green]
  hPutStr stdout "> "
  hSetSGR stdout [Reset]
  hFlush stdout
  line <- getLine
  evaled <- evalString line
  case evaled of
    Ok content -> do
      putStrLn $ printTable content
      run
    Error e -> do
      printErr e
      run

evalString c = catch ((evalRoot . parseJCQL . alexScanTokens) c) (\(ErrorCall e) -> return $ Error e)