import System.Directory (listDirectory)
import System.FilePath
import Data.Maybe
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Typing
import Parser(parseType, parseExpr)
import Lexer(scanMany)
import Ast(Tipe)
import Logic(generateTree,Prop(..),Inference(..),Derivation(..), Conclusion)


checkSourceCode :: String -> IO ()
checkSourceCode prog =
  let divider = "\n------------------------" in do
    putStrLn (divider ++ "\nProg: " ++ prog);
    case (typecheck . parseExpr . scanMany) prog of
      Just t -> putStrLn ("Type: " ++ show t ++ divider)
      Nothing -> print "Error checking program"

checkGenerateTreeIdentity :: IO ()
checkGenerateTreeIdentity =
  let divider = "\n------------------------" in do
    putStrLn (divider ++ "\nGenerate Tree Identity:");
    let input = (Axiom, (Atom "A")) in do
      let expected = input in do
        case [expected] == (generateTree [input]) of
          True -> putStrLn ("Passed")
          False -> putStrLn ("Failed")

checkGenerateTreeDoubleNeg :: IO ()
checkGenerateTreeDoubleNeg =
  let divider = "\n------------------------" in do
    putStrLn (divider ++ "\nGenerate Tree Double Neg:");
    let input = (Axiom, (Neg (Neg (Atom "A")))) in do
      let expected = (Unary DoubleNegation input, (Atom "A")) in do
        case [expected] == (generateTree [input]) of
          True -> putStrLn ("Passed")
          False -> putStrLn ("Failed")

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
  checkGenerateTreeIdentity
  checkGenerateTreeDoubleNeg