import System.Directory (listDirectory)
import System.FilePath
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Typing
import Interp(interp, Value)
import Parser(parseType, parseExpr)
import Lexer(scanMany)
import Ast(Tipe)
import Logic()
import Synthesizer(synthesize)

checkSourceCode :: String -> IO ()
checkSourceCode prog =
  let divider = "\n------------------------" in do
    putStrLn (divider ++ "\nProg: " ++ prog);
    case (typecheck . parseExpr . scanMany) prog of
      Just t -> putStrLn ("Type: " ++ show t ++ divider)
      Nothing -> putStrLn "Error checking program"

checkSynthesize :: String -> IO ()
checkSynthesize prog =
  let divider = "\n------------------------" in do
    putStrLn (divider ++ "\nProg: " ++ prog);
    case (synthesize . parseExpr . scanMany) prog of
      Just e -> putStrLn ("Synthesized: " ++ show e ++ divider)
      Nothing -> putStrLn "Error synthesizing program"

checkSynthInterp :: String -> IO ()
checkSynthInterp prog =
  let divider = "\n------------------------" in do
    putStrLn (divider ++ "\nProg: " ++ prog);
    case (synthesize . parseExpr . scanMany) prog of
      Just e -> putStrLn ("Interpreted: " ++ show (interp e) ++ divider)
      Nothing -> putStrLn "Error interpreting program"

readFilesInDirectory :: IO [String]
readFilesInDirectory =
  let dir = "examples" in do
    files <- listDirectory dir
    let filePaths = map (dir </>) files
    mapM readFileAsStrings filePaths

readFileAsStrings :: FilePath -> IO String
readFileAsStrings filePath = do
    fileContent <- TIO.readFile filePath
    return (T.unpack fileContent)

main :: IO ()
main = do
  programs <- readFilesInDirectory
  mapM_ checkSourceCode programs
  mapM_ checkSynthesize programs
  mapM_ checkSynthInterp programsimport System.Directory (listDirectory)
import System.FilePath
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Typing
import Interp(interp, Value)
import Parser(parseType, parseExpr)
import Lexer(scanMany)
import Ast(Tipe)
import Logic()
import Synthesizer(synthesize)

checkSourceCode :: (String, String) -> IO ()
checkSourceCode (file, prog) =
  let divider = "\n---------------" ++ file ++ "----------------" in do
    putStrLn (divider ++ "\nProg: " ++ prog);
    case (typecheck . parseExpr . scanMany) prog of
      Just t -> putStrLn ("Type: " ++ show t)
      Nothing -> putStrLn "Error checking program"

checkSynthesize :: (String, String) -> IO ()
checkSynthesize (file, prog) =
  let divider = "\n---------------" ++ file ++ "----------------" in do
    putStrLn (divider ++ "\nProg: " ++ prog);
    case (synthesize . parseExpr . scanMany) prog of
      Just e -> putStrLn ("Synthesized: " ++ show e)
      Nothing -> putStrLn "Error synthesizing program"

checkSynthInterp :: (String, String) -> IO ()
checkSynthInterp (file, prog) =
  let divider = "\n---------------" ++ file ++ "----------------" in do
    putStrLn (divider ++ "\nProg: " ++ prog);
    case (synthesize . parseExpr . scanMany) prog of
      Just e -> putStrLn ("Interpreted: " ++ show (interp e))
      Nothing -> putStrLn "Error interpreting program"

readFilesInDirectory :: IO [(String, String)]
readFilesInDirectory =
  let dir = "examples" in do
    files <- listDirectory dir
    let filePaths = map (dir </>) files
    mapM readFileAsStrings filePaths

readFileAsStrings :: FilePath -> IO (String, String)
readFileAsStrings filePath = do
    fileContent <- TIO.readFile filePath
    return (filePath, T.unpack fileContent)

main :: IO ()
main = do
  programs <- readFilesInDirectory
  mapM_ checkSynthesize programs
  mapM_ checkSynthInterp programs