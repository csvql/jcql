module Main where

import           AST
import           Control.Exception
import           GHC.IO.Exception
import           Interpreter
import           Lex
import           Parse
import           System.Console.ANSI
import           System.Environment             ( getArgs )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )

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
  args       <- getArgs
  argsParsed <- handle argReadHandler $ parse args
  case argsParsed of
    Ok content -> do
      catch (eval content) (\(ErrorCall e) -> printErr e)
    Error e -> printErr e

eval :: String -> IO ()
eval c = catch (eval' c) (\(ErrorCall e) -> printErr e)
 where
  eval' c = do
    parsed <- (evalRoot . parseJCQL . alexScanTokens) c
    case parsed of
      Error s -> printErr s
      Ok    v -> print v

printErr e = do
  hSetSGR stderr [SetColor Foreground Vivid Red]
  hPutStrLn stderr e
  hSetSGR stderr [Reset]
