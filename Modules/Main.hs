import Datatypes
import Tokenizer
import Parser
import Compiler
import Emulator
import Control.Exception (SomeException, try)
import Control.Monad (unless)

letsGo :: IO ()
letsGo = do
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
      input code

goodbye :: IO ()
goodbye = do
  putStr "\nGoodbye!\n\n"

e1 = "bool x = ((x == true) | (x == false)); f x = if bool x | x < 1 then 1 else x * f (x - 1); main = f 6;"
e2 = "main = quadrat (quadrat (3 * 1)); quadrat x = x * x;"
e3 = "main = f 3; f x = let y = 5, z = false in if (y < x) == z then x / (y / 3) else f (x - 1); f = 1;"
e4 = "main = 1;"
e5 = "main = 1+2;"
e6 = "f x y = 1;main = e;" --passt -- VariableNotInScope e
e7 = "f x y = x + y;" --passt -- MissingMain
e8 = "f x y = c; main = f 3 4;" --passt -- VariableNotInScope c
e9 = "vector x y = (x y); main = vector 3 2;" -- Prelude.!!: negative index 
e10 = "id x = x; main = id 1;"
e11 = "vektorpr x y z w = x*z + y*w; main = vektorpr 1 2 3 4;"
e12 = "main = #;"
e13 = " main = 3; +" -- passt -- "parse error on input: +" 
e14 = "main = quadratwurzel 25; quadratwurzel x = 1 + qw x 20; qw a b = if b == 0 then a else (a-1)/(2+qw a (b-1));"