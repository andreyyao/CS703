{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.FilePath
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Console.CmdArgs
import Ast(Expr)
import Synthesizer(synthesize)
import Typing(Context, typecheck)
import Interp(interp, Value)
import Parser(parseType, parseExpr)
import Lexer(scanMany)
import Debug.Trace

data Modal = Check | Synth | Eval
                deriving (Show, Data, Typeable)

data Nice = Nice {file :: Maybe FilePath, mode :: Modal} deriving (Show, Data, Typeable)


nice = Nice
  { file = def,
    mode = enum
    [ Check &= help "Type check",
      Synth &= help "Synthesis",
      Eval &= help "Evaluate" ]
  } &= help "synthcc" &= auto

cmd = cmdArgs $ nice &= help "Synthesizer" &= program "Synthcc" &= summary "CS703 Synthesizer for first class continuations v1.0"

parseInput :: Maybe FilePath -> IO Expr
parseInput fpo =
  fmap (parseExpr . scanMany) prog where
  prog = case fpo of
    Just fp -> do
      fileContent <- TIO.readFile fp
      return $ T.unpack fileContent
    Nothing -> getContents

main :: IO ()
main = do
  c <- cmd
  e <- parseInput $ file c
  putStrLn $ case mode c of
    Check -> case typecheck e of
      Just t -> "Type: " ++ show t
      Nothing -> "Type error"
    Synth -> maybe "Synthesis error" show (synthesize e)
    Eval -> show $ interp e