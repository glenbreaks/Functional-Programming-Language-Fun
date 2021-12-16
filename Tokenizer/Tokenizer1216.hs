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
spaceyfier xs = do
   case xs of
       ';' : _          -> " ;" ++ spaceyfier (tail xs) -- leerzeichen danach?
       '<' : _          -> " < " ++ spaceyfier (tail xs)
       '=' : '=' : rest -> " == " ++ spaceyfier (tail rest)
       '+' : _          -> " + " ++ spaceyfier (tail xs)
       '-' : _          -> " - " ++ spaceyfier (tail xs)
       '*' : _          -> " * " ++ spaceyfier (tail xs)
       '/' : _          -> " / " ++ spaceyfier (tail xs)
       []               -> []
       _                -> head xs : spaceyfier (tail xs)

example = spaceyfier "f x == 3<4;" -- passt 
       
wordyfier :: String -> [String]
wordyfier xs = words $ spaceyfier xs

example2 = wordyfier "f x == 3<4;"

tokenizer :: [String] -> [Token]
tokenizer ("|"     : xs) = Or            : tokenizer xs
tokenizer ("&"     : xs) = And           : tokenizer xs
tokenizer ("not"   : xs) = Not           : tokenizer xs
tokenizer ("<"     : xs) = LessThan      : tokenizer xs
tokenizer ("=="    : xs) = Is            : tokenizer xs
tokenizer ("-"     : xs) = Minus         : tokenizer xs
tokenizer ("+"     : xs) = Plus          : tokenizer xs
tokenizer ("/"     : xs) = DivBy           : tokenizer xs
tokenizer ("*"     : xs) = Times         : tokenizer xs
tokenizer ("("     : xs) = OpenPar       : tokenizer xs
tokenizer (")"     : xs) = ClosePar      : tokenizer xs
tokenizer ("Let"   : xs) = Let           : tokenizer xs
tokenizer ("In"    : xs) = In            : tokenizer xs
tokenizer ("If"    : xs) = If            : tokenizer xs
tokenizer ("Then"  : xs) = Then          : tokenizer xs
tokenizer ("Else"  : xs) = Else          : tokenizer xs
tokenizer (";"     : xs) = Semicolon     : tokenizer xs
tokenizer ("="     : xs) = Equals        : tokenizer xs
tokenizer ("True"  : xs) = Boolean True  : tokenizer xs
tokenizer ("False" : xs) = Boolean False : tokenizer xs
tokenizer []             = []
tokenizer (x:xs)         = do
    if checkNumber x then Number (read x) : tokenizer xs else Name x : tokenizer xs

example4 = tokenizerFinal "f x = 3*4+25"

checkInt :: Char -> Bool
checkInt x = do
    case x of
        '0' -> True
        '1' -> True
        '2' -> True
        '3' -> True
        '4' -> True
        '5' -> True
        '6' -> True
        '7' -> True
        '8' -> True
        '9' -> True
        _   -> False

checkNumber :: String -> Bool
checkNumber (x:xs) = checkInt x && checkNumber xs
checkNumber [] = True



tokenizerFinal xs = tokenizer $ wordyfier xs

example3 = tokenizerFinal "f x = 3<4;" -- passt 


-- read "123" :: Int
-- 123
-- main :: IO()
-- main = tokenizer (wordyfier (spaceyfier x))
