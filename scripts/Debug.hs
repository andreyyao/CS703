module Main where
import System.Directory (listDirectory)
import System.FilePath
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit
import Typing
import Interp(interp, Value)
import Parser(parseType, parseExpr)
import Lexer(scanMany)
import Ast(Tipe, sizeof )
import Logic()
import Synthesizer(synthesize)
import Control.Exception
import Formatting
import Formatting.Clock
import System.Clock


run :: String -> IO ()
run prog = do
    putStrLn ("=========================================")
    start <- getTime Monotonic
    putStrLn ( "SIZE_ORIG:" ++ show ((sizeof. parseExpr . scanMany) prog));
    case (synthesize . parseExpr . scanMany) prog of
      Just t -> putStrLn ( "IN:"++prog ++ "\nSYNTH: " ++ show t ++ "\nSIZE_SYNTH:" ++  show (sizeof t));
      Nothing -> putStrLn ( "IN:"++prog ++ "\nSYNTH:Error checking program");
    end <- getTime Monotonic
    fprint (timeSpecs % "\n") start end


mainLoop :: IO()
mainLoop = do
    putStrLn("\n-------------------------------")
    putStrLn("Enter Program('exit' to quit):");
    prog <- getLine;
    if prog == "exit" then exitWith (ExitSuccess)
    else
        run prog;
        mainLoop
main :: IO ()
main = do  
    putStrLn ("Debugging Script");
    mainLoop;