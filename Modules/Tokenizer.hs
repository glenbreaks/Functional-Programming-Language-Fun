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

