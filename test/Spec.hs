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
import Logic(unaryHelper,Prop(..),Inference(..),Derivation(..), Conclusion)


checkSourceCode :: String -> IO ()
checkSourceCode prog =
  let divider = "\n------------------------" in do
    putStrLn (divider ++ "\nProg: " ++ prog);
    case (typecheck . parseExpr . scanMany) prog of
      Just t -> putStrLn ("Type: " ++ show t ++ divider)
      Nothing -> print "Error checking program"


unaryTestHelper :: Conclusion -> Conclusion -> String
unaryTestHelper c c1 = case unaryHelper c of
  c1 -> "Passed"
  _ -> "Failed"

checkUnaryHelper :: IO ()
checkUnaryHelper = do
  putStrLn "\nLogic Tests\n------------------------"
  let c=(Axiom, (Neg (Neg (Atom "Int")))) in do
    putStrLn (unaryTestHelper c (Unary DoubleNegation c, (Atom "Int")))

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
  checkUnaryHelper