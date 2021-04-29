module Eval where

import Control.Exception
import Interpreter 
import Parse
import Lex
import System.Console.ANSI
import System.IO
import Data.List
eval :: String -> IO ()
eval c = catch (eval' c) (\(ErrorCall e) -> printErr e)
 where
  eval' c = do
    parsed <- (evalRoot . parseJCQL . alexScanTokens) c
    case parsed of
      Error s -> printErr s
      Ok    v -> putStrLn $ printTable v

printErr e = do
  hSetSGR stderr [SetColor Foreground Vivid Red]
  hPutStrLn stderr e
  hSetSGR stderr [Reset]

printTable = intercalate "\n" . map (intercalate ",")
