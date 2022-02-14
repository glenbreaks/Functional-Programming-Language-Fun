import Datatypes
import Tokenizer
import Parser
import Compiler
import Emulator
import Control.Exception (SomeException, try)
import Control.Monad (unless)

welcome :: IO ()
welcome = do
  putStr "\n\nWelcome to Fun, our implementation of the functional programming language F.\n"
  putStr "We hope you have fun with Fun  : )     - Chrissi, Daniel, Jonny, Kathy and Lara\n\n"
  putStr "\nPlease enter your F Program:\n"
  programInput

programInput = do
  code <- getLine
  input code

input :: String -> IO ()
input code = do
  putStr "\nPlease select an option:\n"
  putStr "[1] Tokenize  [2] Parse\n[3] Compile   [4] Emulate\n[5] Result    [6] New Program\n[7] Syntax    [0] Exit\n\n"
  choice <- getLine
  case choice of
    "1" -> do
      showTokens code
      input code
    "2" -> do
      showParse code
      input code
    "3" -> do
      showCompile code
      input code
    "4" -> do
      showEmulate code
      input code
    "5" -> do
      emulate code
      input code
    "6" -> do
      putStr "\nPlease enter your F Program:\n"
      programInput
      input code
    "7" -> do
      syntax <- readFile "Syntax.txt"
      putStr "\n\n"
      putStr syntax
      putStr "\n\n"
      input code
    "0" -> goodbye
    _   -> do
      input code

goodbye :: IO ()
goodbye = do
  putStr "Goodbye\n\n"

readUserFile :: IO String
readUserFile = do
  print "Please enter filename of your F Program: "
  filename <- getLine
  contents <- try (readFile filename) :: IO(Either SomeException String)
  case contents of
    Left exception -> do
      print "File not found."
      readUserFile
    Right wert -> return wert