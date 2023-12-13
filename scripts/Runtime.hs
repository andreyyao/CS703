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


checkSynth:: String->String
checkSynth prog = do
    let origString =  "SIZE_ORIG:" ++ show ((sizeof. parseExpr . scanMany) prog) ++ "\nIN:"++prog in do
        case (synthesize . parseExpr . scanMany) prog of
            Just t -> origString++ "\nSYNTH:" ++ show t ++ "\nSIZE_SYNTH:" ++  show (sizeof t);
            Nothing ->  origString ++ "\nSYNTH:ERROR\nSIZE_SYNTH:None"
run :: String -> IO ()
run prog = do
    putStrLn ("__NEW_EXAMPLE__")
    start <- getTime Monotonic
    putStrLn (checkSynth prog)
    end <- getTime Monotonic
    fprint "TIME:"
    fprint (timeSpecs % "\n") start end


readFilesInDirectory :: IO [String]
readFilesInDirectory =
  let dir = "examples" in do
    files <- listDirectory dir
    let filePaths = map (dir </>) files
    putStrLn ("Reading files: " ++ show filePaths)
    mapM readFileAsStrings filePaths

readFileAsStrings :: FilePath -> IO String
readFileAsStrings filePath = do
    fileContent <- TIO.readFile filePath
    return (T.unpack fileContent)

main :: IO ()
main = do  
    putStrLn("__START__")
    programs <- readFilesInDirectory
    mapM_ run programs
    putStrLn("__END__")