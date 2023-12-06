import System.Directory (listDirectory)
import System.FilePath
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
      Nothing -> print "Error checking program"

checkSynthesize :: String -> IO ()
checkSynthesize prog =
  let divider = "\n------------------------" in do
    putStrLn (divider ++ "\nProg: " ++ prog);
    case (synthesize . parseExpr . scanMany) prog of
      Just t -> putStrLn ("Synthesized: " ++ show t ++ divider)
      Nothing -> print "Error checking program"

checkSynthInterp :: String -> IO ()
checkSynthInterp prog =
  let divider = "\n------------------------" in do
    putStrLn (divider ++ "\nProg: " ++ prog);
    let v = (interp . parseExpr . scanMany) prog in
      putStrLn ("Interpreted: " ++ show v ++ divider)


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
  mapM_ checkSynthInterp programs