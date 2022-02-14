module Tokenizer (showTokens, tokenize) where
import Datatypes (FunException(..), Token(..))
import Show
import Data.Char(isDigit, isAlpha, isAlphaNum)
import Control.Exception (Exception, throw)

---------- Tokenizer:

-- emulate bsp
bsp = "main = quadratwurzel 25; quadratwurzel x = 1 + qw x 20; qw a b = if b == 0 then a else (a-1)/(2+qw a (b-1));"

showTokens :: String -> IO() -- just for output
showTokens xs = let ys = tokenize xs in
    putStr $ showBoxed "Tokens" ++ "\n\n" ++ hshowTokens ys ++ "\n"
        where hshowTokens (x:xs) = show x ++ "\n" ++ hshowTokens xs
              hshowTokens []     = ""

tokenize :: String -> Either String [Token]
tokenize xs = case (tokenizer $ words $ spaceyfier xs) of
    Right x -> x
    error   -> error

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
tokenizer []             = return[]

checkNumber :: String -> Bool
checkNumber = foldr ((&&) . isDigit) True

checkName :: String -> Bool
checkName = foldr (\ x -> (&&) (isAlphaNum x || x == '_' || x == '\'')) True

-- Beispiele: spaceifyer, Variablennamen mit _, ' und Zahlen (InvalidName auslösen?)

