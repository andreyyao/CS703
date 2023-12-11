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
import Ast(Tipe)
import Logic()
import Synthesizer(synthesize)

checkValidCode :: String -> IO ()
checkValidCode prog = do
    case (typecheck . parseExpr . scanMany) prog of
        Just t -> putStrLn ("Type: " ++ show t)
        Nothing -> error "Error checking program"

checkScanTokens :: String -> IO ()
checkScanTokens prog = do
    putStrLn ("Tokens: " ++ show (scanMany prog))
        

checkSynthesize :: String -> IO ()
checkSynthesize prog = do
    case (synthesize . parseExpr . scanMany) prog of
      Just t -> putStrLn ("Synthesized: " ++ show t)
      Nothing -> print "Error checking program"

checkSynthInterp :: String -> IO ()
checkSynthInterp prog = do
    case (synthesize . parseExpr . scanMany) prog of
      Just e -> putStrLn ("Interpreted: " ++ show (interp e))
      Nothing -> print "Error interpreting program"

run :: String -> IO ()
run prog = do
    putStrLn ("\nProg: " ++ prog);
    checkScanTokens prog;
    putStrLn("Parses: Success\n");
    checkValidCode prog;
    putStrLn("Typechecks: Success\n");
    checkSynthesize prog;
    putStrLn("Synhesizes: Success\n");
    checkSynthInterp prog;
    putStrLn("Interprets: Success\n");


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