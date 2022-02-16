module Tokenizer (showTokenize, tokenize) where
import Datatypes (Token(..))
import Show
import Data.Char(isDigit, isAlpha, isAlphaNum)
import Control.Exception (Exception, throw)

---------- Tokenizer:

-- emulate bsp
bsp = "main = quadratwurzel 25; quadratwurzel x = 1 + qw x 20; qw a b = if b == 0 then a else (a-1)/(2+qw a (b-1));"

showTokenize :: String -> IO() -- just for output
showTokenize xs =
    case tokenize xs of
        Right x -> putStr $ showBoxed "Tokens" ++ "\n\n" ++ hshowTokenize x ++ "\n"
                    where hshowTokenize (x:xs) = show x ++ "\n" ++ hshowTokenize xs
                          hshowTokenize []     = ""
        Left x  -> putStrLn x

tokenize :: String -> Either String [Token]
tokenize xs = tokenizer $ words $ spaceyfier xs


spaceyfier :: String -> String
spaceyfier x =
   case x of
       ';' : xs       -> " ; "  ++ spaceyfier xs
       ',' : xs       -> " , "  ++ spaceyfier xs
       '|' : xs       -> " | "  ++ spaceyfier xs
       '&' : xs       -> " & "  ++ spaceyfier xs
       '<' : xs       -> " < "  ++ spaceyfier xs
       '=' : '=' : xs -> " == " ++ spaceyfier xs
       '=' : xs       -> " = "  ++ spaceyfier xs
       '+' : xs       -> " + "  ++ spaceyfier xs
       '-' : xs       -> " - "  ++ spaceyfier xs
       '*' : xs       -> " * "  ++ spaceyfier xs
       '/' : xs       -> " / "  ++ spaceyfier xs
       '(' : xs       -> " ( "  ++ spaceyfier xs
       ')' : xs       -> " ) "  ++ spaceyfier xs
       []             -> []
       _   : xs       -> head x : spaceyfier xs

space :: String -> [Token]
space = aspace chars tokens
    where chars = ";,|&<+-*/()="
          tokens = [Semicolon, Comma, Or, And, LessThan, Plus, Minus, Times, DivBy, OpenPar, ClosePar, Equals]
          aspace (x:xs) (y:ys) (z:zs) =
              if x == z then 
                  if z == '=' && head zs == '=' then
                          Is : space(tail zs)
                  else y : space zs
              else aspace xs ys (z:zs)
--          aspace [] _ x = bspace x
--                where bspace (x:xs) = 
          aspace _ _ _ = []

tok :: [String] -> [Token] 
tok (x:xs) = htok strings tokens (x:xs)
    where strings = ["|","&","not","<","==","-","+","/","*","(",")","let","in","if","then","else",";",",","=","true",",false"]
          tokens = [Or,And,Not,LessThan,Is,Minus,Plus,DivBy,Times,OpenPar,ClosePar,Let,In,If,Then,Else,Semicolon,Comma,Equals,Boolean True, Boolean False]
          htok (x:xs) (y:ys) (z:zs) =
              if x == z then
                  y : space zs
              else htok xs ys (z:zs)
          htok _ _ (z:zs) =
              case tok zs of
                  rest -> case z of
                    _       | checkNumber z                   -> (Number (read z) : rest
                            | isAlpha (head z) && checkName z -> Name z          : rest
                            | otherwise                       -> throw userError "InvalidName"
tok [] = []

tokenizer :: [String] -> Either String [Token]
tokenizer (x:xs) =
    case tokenizer xs of
        Right rest -> case x of
            "|"     -> return (Or            : rest)
            "&"     -> return (And           : rest)       
            "not"   -> return (Not           : rest)
            "<"     -> return (LessThan      : rest)
            "=="    -> return (Is            : rest)
            "-"     -> return (Minus         : rest)
            "+"     -> return (Plus          : rest)
            "/"     -> return (DivBy         : rest)
            "*"     -> return (Times         : rest)
            "("     -> return (OpenPar       : rest)
            ")"     -> return (ClosePar      : rest)
            "let"   -> return (Let           : rest)
            "in"    -> return (In            : rest)
            "if"    -> return (If            : rest)
            "then"  -> return (Then          : rest)
            "else"  -> return (Else          : rest)
            ";"     -> return (Semicolon     : rest)
            ","     -> return (Comma         : rest)
            "="     -> return (Equals        : rest)
            "true"  -> return (Boolean True  : rest)
            "false" -> return (Boolean False : rest)
            _       | checkNumber x                   -> return (Number (read x) : rest)
                    | isAlpha (head x) && checkName x -> return (Name x          : rest)
                    | otherwise                       -> Left ("Invalid name: " ++ show x)
        error      -> error
tokenizer []             = return[]

checkNumber :: String -> Bool
checkNumber = foldr ((&&) . isDigit) True

checkName :: String -> Bool
checkName = foldr (\ x -> (&&) (isAlphaNum x || x == '_' || x == '\'')) True

-- Beispiele: spaceifyer, Variablennamen mit _, ' und Zahlen (InvalidName auslösen?)

