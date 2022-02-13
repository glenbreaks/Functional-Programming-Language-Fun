module Tokenizer (showTokens, tokenize) where
import Datatypes (CompilerException(..), Token(..))
import Show (showDef, showStack, showHeap, showBoxed)
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

tokenize :: String -> [Token]
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

tokenizer :: [String] -> [Token]
tokenizer (x:xs) =
    case x of
        "|"     -> Or            : tokenizer xs
        "&"     -> And           : tokenizer xs
        "not"   -> Not           : tokenizer xs
        "<"     -> LessThan      : tokenizer xs
        "=="    -> Is            : tokenizer xs
        "-"     -> Minus         : tokenizer xs
        "+"     -> Plus          : tokenizer xs
        "/"     -> DivBy         : tokenizer xs
        "*"     -> Times         : tokenizer xs
        "("     -> OpenPar       : tokenizer xs
        ")"     -> ClosePar      : tokenizer xs
        "let"   -> Let           : tokenizer xs
        "in"    -> In            : tokenizer xs
        "if"    -> If            : tokenizer xs
        "then"  -> Then          : tokenizer xs
        "else"  -> Else          : tokenizer xs
        ";"     -> Semicolon     : tokenizer xs
        ","     -> Comma         : tokenizer xs
        "="     -> Equals        : tokenizer xs
        "true"  -> Boolean True  : tokenizer xs
        "false" -> Boolean False : tokenizer xs
        _        | checkNumber x                   -> Number (read x) : tokenizer xs
                 | isAlpha (head x) && checkName x -> Name x          : tokenizer xs
                 | otherwise                       -> throw (InvalidName x)
tokenizer []             = []

checkNumber :: String -> Bool
checkNumber = foldr ((&&) . isDigit) True

checkName :: String -> Bool
checkName = foldr (\ x -> (&&) (isAlphaNum x || x == '_' || x == '\'')) True

-- Beispiele: spaceifyer, Variablennamen mit _, ' und Zahlen (InvalidName auslösen?)

