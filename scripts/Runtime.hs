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
import Ast(Tipe)
import Logic()
import Synthesizer(synthesize)
import Control.Exception
import Formatting
import Formatting.Clock
import System.Clock

checkSynthesize :: String -> IO ()
checkSynthesize prog = do
    case (synthesize . parseExpr . scanMany) prog of
      Just t -> putStrLn ( prog ++ " -> " ++ show t)
      Nothing -> putStrLn ( prog ++ " -> " ++ "Error checking program")

run :: String -> IO ()
run prog = do
    start <- getTime Monotonic
    checkSynthesize prog;
    end <- getTime Monotonic
    fprint (timeSpecs % "\n") start end
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
    mapM_ run programs