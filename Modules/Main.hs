import Datatypes
import Tokenizer
import Parser
import Compiler
import Emulator
import Control.Exception (SomeException, try)

letsGo :: IO ()
letsGo = do
  putStr ("\n\n               Welcome to\n"
       ++ "     ______   __   __   __   __   __\n"
       ++ "    |   ___| |  | |  | |  \\ |  | |  |\n"
       ++ "    |  |__   |  | |  | |   \\|  | |__|\n"
       ++ "    |   __|  |  |_|  | |  |\\   |  __\n"
       ++ "    |__|      \\_____/  |__| \\__| |__|\n\n\n"
       ++ "This is our implementation of the functional"
       ++ "\nprogramming language F.\n\n"
       ++ "We hope you have fun with Fun  : )"
       ++ "\n    - Chrissi, Daniel, Jonny, Kathy and Lara\n\n\n"
       ++ "Please enter your Fun program or press [l] to load a file:\n")
  choice <- getLine 
  case choice of
    "l" -> do
      putStr "\nPlease enter the name of your fun file:\n"
      filename <- getLine
      eitherProgram <- try (readFile filename) :: IO (Either SomeException String)
      case eitherProgram of
        Left  _       -> do 
          putStrLn ("Couldn't find any file with the name " ++ filename)
          input ""
        Right program -> input program
    x   -> input x 

input :: String -> IO ()
input code = do
  if code == "" then 
    putStr "\n"
  else
    putStr ("\nYour program: \n" ++ code)
  putStr ("\n\nPlease select an option:\n"
       ++ "[1] Tokenize  [6] New Program\n"
       ++ "[2] Parse     [7] Load File\n"
       ++ "[3] Compile   [8] ReadMe\n"
       ++ "[4] Emulate   [9] Syntax\n"
       ++ "[5] Result    [0] Exit\n\n")
  choice <- getLine
  case choice of
    "1" -> do
      showTokenize code
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
      putStr "\nPlease enter your Fun Program:\n"
      code <- getLine
      input code
    "7" -> do
      putStr "\nPlease enter the name of your fun file:\n"
      filename <- getLine
      eitherProgram <- try (readFile filename) :: IO (Either SomeException String)
      case eitherProgram of
        Left  _       -> do 
          putStrLn ("Couldn't find any file with the name " ++ filename)
          input code
        Right program -> input program
    "8" -> do
      readMe <- readFile "../README.md"
      putStr "\n\n"
      putStr readMe
      putStr "\n\nThis README is a markdown file. It looks better when opened in a markdown viewer.\n"
      input code
    "9" -> do
      syntax <- readFile "../Syntax.txt"
      putStr "\n\n"
      putStr syntax
      putStr "\n\n"
      input code
    "0" -> goodbye
    _   -> do
      putStr "\nSorry, that's not an option."
      input code

goodbye :: IO ()
goodbye = do
  putStr "\nGoodbye!\n\n"

ex1 = "main = quadratwurzel 25; quadratwurzel x = 1 + qw x 20; qw a b = if b == 0 then a else (a-1)/(2+qw a (b-1));"
ex2 = "main = 5-2-1 + 8/2/3;" -- left associativities of - and /
ex3 = "main = let x = 3, y = 2 in x*y;" -- commas instead of semicolons in loacl definitions
ex4 = "main = (2^3 + 2^(-3) / 2^0 + 2^2^2)^2;" -- exponential functions allowed