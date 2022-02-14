import Datatypes
import Tokenizer
import Parser
import Compiler
import Emulator
import Control.Exception (SomeException, try)
import Control.Monad (unless)

playGround :: IO ()
playGround = do
  print "Willkommen bei unserem "

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