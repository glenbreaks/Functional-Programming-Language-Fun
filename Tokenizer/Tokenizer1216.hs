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
    | Div
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
       ';' : _  -> " ;" ++ spaceyfier (tail xs) -- leerzeichen danach?
       '<' : _  -> " < " ++ spaceyfier (tail xs)
       '=' : '=' : rest  -> " == " ++ spaceyfier (tail rest)
       '+' : _  -> " + " ++ spaceyfier (tail xs)
       '-' : _  -> " - " ++ spaceyfier (tail xs)
       '*' : _  -> " * " ++ spaceyfier (tail xs)
       '/' : _  -> " / " ++ spaceyfier (tail xs)
       []        -> []
       _         -> head xs : spaceyfier (tail xs)

example = spaceyfier "f x == 3<4;" -- passt 
       
wordyfier :: String -> [String]
wordyfier xs = words $ spaceyfier xs

example2 = wordyfier "f x == 3<4;"

tokenizer :: [String] -> [Token]
tokenizer x = do
    case x of
        "|"     : xs -> Or            : tokenizer xs
        "&"     : xs -> And           : tokenizer xs
        "not"   : xs -> Not           : tokenizer xs
        "<"     : xs -> LessThan      : tokenizer xs
        "=="    : xs -> Is            : tokenizer xs
        "-"     : xs -> Minus         : tokenizer xs
        "+"     : xs -> Plus          : tokenizer xs
        "/"     : xs -> Div           : tokenizer xs
        "*"     : xs -> Times         : tokenizer xs
        "("     : xs -> OpenPar       : tokenizer xs
        ")"     : xs -> ClosePar      : tokenizer xs
        "Let"   : xs -> Let           : tokenizer xs
        "In"    : xs -> In            : tokenizer xs
        "If"    : xs -> If            : tokenizer xs
        "Then"  : xs -> Then          : tokenizer xs
        "Else"  : xs -> Else          : tokenizer xs
        ";"     : xs -> Semicolon     : tokenizer xs
        "="     : xs -> Equals        : tokenizer xs
        "True"  : xs -> Boolean True  : tokenizer xs
        "False" : xs -> Boolean False : tokenizer xs

-- read "123" :: Int
-- 123
-- main :: IO()
-- main = tokenizer (wordyfier (spaceyfier x))
