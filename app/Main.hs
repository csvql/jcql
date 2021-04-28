module Main where

import Parse
import Lex
import AST
import System.Environment ( getArgs )
import Interpreter
import Control.Exception
import System.Console.ANSI

parse :: [FilePath] -> IO (Result String)
parse [f] = do
  f <- readFile f
  return $ Ok f
parse fs = return $ Error ("expected 1 argument, given "++show (length fs))

main :: IO ()
main = do
  args <- getArgs
  argsParsed <- parse args
  case argsParsed of
    Ok content -> do
      catch (eval content) (\(ErrorCall e) -> printErr e)
    Error e -> printErr e
  

eval :: String -> IO ()
eval c = do
  parsed <- (evalRoot . parseJCQL . alexScanTokens) c
  case parsed of
    Error s -> error s
    Ok v -> print v

printErr e = do
  setSGR [SetColor Foreground Vivid Red]
  putStrLn e
  setSGR [Reset]