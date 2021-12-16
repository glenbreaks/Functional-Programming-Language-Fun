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

preTokenizer :: String -> [String]
preTokenizer xs = do
   case xs of
       ';' : _  -> " ;" : preTokenizer (tail xs) -- leerzeichen danach?
       '<' : _ -> " < " : preTokenizer (tail xs)
    --    "==" -> 
    --    "+"  -> 
    --    "-"  -> 
    --    "*"  -> 
    --    "/"  -> 
       []    -> []
       xss    -> xss : preTokenizer (tail xs)
       

-- Leerzeichen einfügen bei Semicolons, Plus, Minus, Times, DivBy, LessThan, Is

tokenizer :: [String] -> [Token]
tokenizer ("|" : xs)   = Or       : tokenizer xs
tokenizer ("&" : xs)   = And      : tokenizer xs
tokenizer ("not" : xs) = Not      : tokenizer xs
tokenizer ("<" : xs)   = LessThan : tokenizer xs
tokenizer ("==" : xs)  = Is       : tokenizer xs
tokenizer ("-" : xs)   = Minus    : tokenizer xs
tokenizer ("+" : xs)   = Plus     : tokenizer xs
            -- "/"     -> Div      : tokenizer xs
            -- "*"     -> Times    : tokenizer xs
            -- "("     -> OpenPar  : tokenizer xs
            -- ")"     -> ClosePar : tokenizer xs
            -- "Let"   -> Let      : tokenizer xs
            -- "In"    -> In       : tokenizer xs
            -- "If"    -> If       : tokenizer xs
            -- "Then"  -> Then     : tokenizer xs
            -- "Else"  -> Else     : tokenizer xs
            -- ";"     -> Semicolon: tokenizer xs
            -- "="     -> Equals   : tokenizer xs
-- tokenizer ("True" : xs) = Boolean True : tokenizer xs
-- tokenizer

-- read "123" :: Int
-- 123

