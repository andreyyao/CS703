module Main where

import Parser(parseType, parseExpr)
import Lexer(scanMany)

main :: IO ()
main = do
  -- putStrLn "TEST\n"
  (print . parseType . scanMany) "Int"
  (print . parseExpr . scanMany) "1 + 2 * 3"
  return ()
