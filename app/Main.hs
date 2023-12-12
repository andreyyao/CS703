{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.FilePath
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import System.Console.CmdArgs
import Ast
import Synthesizer(synthesize)
import Typing(Context, typecheck)
import Interp(interp, Value)
import Parser(parseType, parseExpr)
import Lexer(scanMany)
import Debug.Trace

data Modal = Check | Synth | Eval
                deriving (Show, Data, Typeable)

data Nice = Nice {file :: FilePath, mode :: Modal} deriving (Show, Data, Typeable)


nice = Nice
  { file = def,
    mode = enum
    [ Check &= help "Type check",
      Synth &= help "Synthesis",
      Eval &= help "Evaluate" ]
  } &= help "synthcc" &= auto

cmd = cmdArgs $ nice &= help "Synthesizer" &= program "Synthcc" &= summary "CS703 Synthesizer for first class continuations v1.0"

parseFile :: FilePath -> IO Expr
parseFile filePath = do
    fileContent <- TIO.readFile filePath
    return (parseExpr . scanMany $ T.unpack fileContent)

main :: IO ()
main = do
  c <- cmd
  e <- parseFile $ file c
  putStrLn $ case mode c of
    Check -> case typecheck e of
      Just t -> "Type: " ++ show t
      Nothing -> "Type error"
    Synth -> maybe "Synthesis error" show (synthesize e)
    Eval -> show $ interp e