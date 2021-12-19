import Data.Char(isDigit, isAlpha, isAlphaNum)
import Control.Exception (Exception, throw)

data CompilerException 
    = InvalidName !String
    -- |
    -- |
    deriving Show

instance Exception CompilerException

data Token
    = Number Integer
    | Name String
    | Boolean Bool
    | Or
    | And
    | Not
    | LessThan
    | Is
    | Minus
    | Plus
    | DivBy
    | Times
    | OpenPar
    | ClosePar
    | Let
    | In
    | If
    | Then
    | Else
    | Semicolon
    | Equals
    deriving (Eq, Show)

spaceyfier :: String -> String
spaceyfier x = do
   case x of
       ';' : xs       -> " ;"   ++ spaceyfier xs -- hier kein Leerzeichen danach, da Strichpunkte in Namen eh nicht erlaubt sind!
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
tokenizer ("|"     : xs) = Or            : tokenizer xs
tokenizer ("&"     : xs) = And           : tokenizer xs
tokenizer ("not"   : xs) = Not           : tokenizer xs
tokenizer ("<"     : xs) = LessThan      : tokenizer xs
tokenizer ("=="    : xs) = Is            : tokenizer xs
tokenizer ("-"     : xs) = Minus         : tokenizer xs
tokenizer ("+"     : xs) = Plus          : tokenizer xs
tokenizer ("/"     : xs) = DivBy         : tokenizer xs
tokenizer ("*"     : xs) = Times         : tokenizer xs
tokenizer ("("     : xs) = OpenPar       : tokenizer xs
tokenizer (")"     : xs) = ClosePar      : tokenizer xs
tokenizer ("let"   : xs) = Let           : tokenizer xs
tokenizer ("in"    : xs) = In            : tokenizer xs
tokenizer ("if"    : xs) = If            : tokenizer xs
tokenizer ("then"  : xs) = Then          : tokenizer xs
tokenizer ("else"  : xs) = Else          : tokenizer xs
tokenizer (";"     : xs) = Semicolon     : tokenizer xs
tokenizer ("="     : xs) = Equals        : tokenizer xs
tokenizer ("True"  : xs) = Boolean True  : tokenizer xs -- klein wär schöner und ala Skript aber input soll output matchen! (was ist output lol)
tokenizer ("False" : xs) = Boolean False : tokenizer xs
tokenizer []             = []
tokenizer (x:xs)
    | checkNumber x                   = Number (read x) : tokenizer xs
    | isAlpha (head x) && checkName x = Name x           : tokenizer xs
    | otherwise                       = throw (InvalidName x)

checkNumber :: String -> Bool
checkNumber (x:xs) = isDigit x && checkNumber xs
checkNumber []     = True

-- extraName :: Char -> Bool
-- extraName '_'  = True
-- extraName '\'' = True
-- extraName xs   = False

checkName :: String -> Bool
-- checkName (x:xs) = (isAlphaNum x || extraName x) && checkName xs
checkName (x:xs) = (isAlphaNum x || x == '_' || x == '\'') && checkName xs
checkName []     = True