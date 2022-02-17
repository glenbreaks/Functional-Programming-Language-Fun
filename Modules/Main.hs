import Datatypes
import Tokenizer
import Parser
import Compiler
import Emulator

letsGo :: IO ()
letsGo = do
  putStr ("\n\n               Welcome to\n"
       ++ "     ______   __   __   __   __   __\n"
       ++ "    |   ___| |  | |  | |  \\ |  | |  |\n"
       ++ "    |  |__   |  | |  | |   \\|  | |__|\n"
       ++ "    |   __|  |  |_|  | |  |\\   |  __\n"
       ++ "    |__|      \\_____/  |__| \\__| |__|\n\n\n"
       ++ "This is our implementation of the functional programming language F.\n"
       ++ "We hope you have fun with Fun  : )     - Chrissi, Daniel, Jonny, Kathy and Lara\n\n"
       ++ "\nPlease enter your Fun program:\n")
  programInput

programInput = do
  code <- getLine
  input code

input :: String -> IO ()
input code = do
  putStr ("\nYour program: " ++ code
       ++ "\nPlease select an option:\n"
       ++ "[1] Tokenize  [5] Result\n"
       ++ "[2] Parse     [6] New Program\n"
       ++ "[3] Compile   [7] Syntax\n"
       ++ "[4] Emulate   [0] Exit\n\n")
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
      programInput
    "7" -> do
      syntax <- readFile "Syntax.txt"
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

ex = "main = quadratwurzel 25; quadratwurzel x = 1 + qw x 20; qw a b = if b == 0 then a else (a-1)/(2+qw a (b-1));"