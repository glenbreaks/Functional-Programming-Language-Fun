{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.Char(isDigit, isAlpha, isAlphaNum)
import Control.Exception (Exception, throw)
import GHC.Float (int2Float)

data CompilerException
    = InvalidName !String
    | WrongAddress
    | NoValueFound !String
    | MissingMain
    | TypeCheck !String
    | VariableNotInScope !String
    deriving Show

instance Exception CompilerException

data Token
    = Number Int
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
    | Comma
    | Equals
    deriving Eq

newtype Program = Program [Definition] deriving Show

data Definition = Definition [Expression] Expression

newtype LocDefs = LocDefs [LocDef] deriving (Eq, Show)

data LocDef     = LocDef Expression Expression deriving (Eq, Show)

data Expression
    = LetX      LocDefs Expression
    | IfX       Expression Expression Expression
    | OrX       Expression Expression
    | AndX      Expression Expression
    | NotX      Expression
    | LessThanX Expression Expression
    | IsX       Expression Expression
    | Sum       Expression Expression
    | NegExpo   Expression -- x hoch minus 1 = 1/x
    | Mult      Expression Expression
    | Neg       Expression
    | Function  Expression Expression
    | Val       Int
    | BoolVal   Bool
    | Variable  String
    deriving (Eq, Show)

type Parser a = [Token] -> Either String (a, [Token])

data Instruction
    = Reset
    | Pushfun String
    | Pushval Type Value
    | Pushparam Int
    | Makeapp
    | Slide Int
    | Return
    | Halt
    | Unwind
    | Call
    | Pushpre Token
    | Updatefun Int
    | Updateop
    | Operator Op
    | Alloc
    | Updatelet Int
    | Slidelet Int
    deriving Eq

data Type  = Float | Bool deriving (Eq, Show)
type Value = Float

data State = State
    {
        pc     :: Int,
        code   :: [Instruction],
        stack  :: Stack,
        heap   :: Heap,
        global :: Global
    }

type Stack  = [String]
type Heap   = [HeapCell]
type Global = [(String, Int)]

data HeapCell
    = APP Int Int
    | DEF String Int Int
    | VAL Type Value
    | IND Int
    | PRE Token Op
    | UNINITIALIZED

data Op
    = UnaryOp
    | BinaryOp
    | IfOp
    deriving Eq

newtype Result = Result HeapCell

newtype EmulatorState = EmulatorState [State]

---------- Show-Funktionen:

instance Show Token
    where
        show Or              = "|"
        show And             = "&"
        show Not             = "not"
        show LessThan        = "<"
        show Is              = "=="
        show Minus           = "-"
        show Plus            = "+"
        show DivBy           = "/"
        show Times           = "*"
        show OpenPar         = "("
        show ClosePar        = ")"
        show Let             = "let"
        show In              = "in"
        show If              = "if"
        show Then            = "then"
        show Else            = "else"
        show Semicolon       = ";"
        show Comma           = ","
        show Equals          = "="
        show (Boolean True)  = "true"
        show (Boolean False) = "false"
        show (Number x)      = show x
        show (Name   x)      = x

instance Show Definition
    where show (Definition ((Variable fun):args) expr) = "Definition " ++ fun ++ indent (15-length fun) ++ show (hshowArgs args) ++ indent (10-length (show (hshowArgs args))) ++ "(" ++ show expr ++ ")"
           where hshowArgs ((Variable x):xs) = x:hshowArgs xs
                 hshowArgs []                = []

instance Show Instruction
    where show Reset              = "Reset"
          show (Pushfun fun)      = "Pushfun " ++ fun
          show (Pushval Bool v)
                      | v == 0    = "Pushval Bool false"
                      | otherwise = "Pushval Bool true"
          show (Pushval t v)      = "Pushval " ++ show t ++ " " ++ show v
          show (Pushparam i)      = "Pushparam " ++ show i
          show Makeapp            = "Makeapp"
          show (Slide i)          = "Slide " ++ show i
          show Return             = "Return"
          show Halt               = "Halt"
          show Unwind             = "Unwind"
          show Call               = "Call"
          show (Pushpre If)       = "Pushpre if"
          show (Pushpre Plus)     = "Pushpre +"
          show (Pushpre Minus)    = "Pushpre -"
          show (Pushpre Times)    = "Pushpre *"
          show (Pushpre DivBy)    = "Pushpre 1/"
          show (Pushpre Or)       = "Pushpre |"
          show (Pushpre And)      = "Pushpre &"
          show (Pushpre Not)      = "Pushpre not"
          show (Pushpre LessThan) = "Pushpre <"
          show (Pushpre Is)       = "Pushpre =="
          show (Pushpre OpenPar)  = "Pushpre ("
          show (Pushpre ClosePar) = "Pushpre )"
          show (Pushpre x       ) = "Pushpre " ++ show x    -- x kann Number, Bool oder Name sein
          show (Updatefun i)      = "Update " ++ show i
          show Updateop           = "Update op"
          show (Operator o)       = "Operator " ++ show o
          show Alloc              = "Alloc"
          show (Updatelet i)      = "Updatelet " ++ show i
          show (Slidelet i)       = "Slidelet " ++ show i

instance Show State
    where show (State _ code stack heap global) = "\n\n················\n: Instructions :\n················\n" ++ showCode code 0
                                               ++ "\n········\n: Heap :\n········\n"                           ++ showHeap heap 4
                                               ++ "\n··········\n: Global :\n··········\n"                     ++ showGlobal global
                                               ++ "\n\n"
            where
                showCode (x:xs) 4             = "\n· binary operation ·\nc4:   " ++ show x ++ "\n" ++ showCode xs 5
                showCode (x:xs) 13            = "\n· if operation ·\nc13:  "     ++ show x ++ "\n" ++ showCode xs 14
                showCode (x:xs) 21            = "\n· unary operation ·\nc21:  "  ++ show x ++ "\n" ++ showCode xs 22
                showCode [Return] akk         = "c" ++ show akk ++ ":" ++ indent (4 - length (show akk)) ++ "Return\n"
                showCode (Return:xs) akk
                    | akk /= 12 && akk /=20   = "c" ++ show akk ++ ":" ++ indent (4 - length (show akk)) ++ "Return\n\n· " ++ name (akk+1) heap ++ " ·\n" ++ showCode xs (akk+1)
                    | otherwise               = "c" ++ show akk ++ ":" ++ indent (4 - length (show akk)) ++ "Return\n" ++ showCode xs (akk+1)
                showCode (x:xs) akk           = "c" ++ show akk ++ ":" ++ indent (4 - length (show akk)) ++ show x ++ "\n" ++ showCode xs (akk+1)
                showCode [] _                 = ""
                name n (x:xs)                 =
                    case x of
                        DEF f _ adr -> if n == adr then f else name n xs
                        _           -> ""
                name _ []                     = ""
                showGlobal ((x, y):xs)        = "h" ++ show y ++ ":" ++ indent (4 - length (show y)) ++ x ++ "\n" ++ showGlobal xs
                showGlobal []                 = ""

instance Show HeapCell
    where show (APP a b  )   = "APP h" ++ show a ++ " h" ++ show b
          show (DEF f x y)   = "DEF " ++ f ++ " " ++ show x ++ " c" ++ show y
          show (VAL t v  )   = "VAL " ++ show t ++ " " ++ show v
          show (IND x    )   = "IND h" ++ show x
          show (PRE t op )   = "PRE " ++ show t ++ " " ++ show op
          show UNINITIALIZED = ""

instance Show Op
    where show UnaryOp  = "1"
          show BinaryOp = "2"
          show IfOp     = "3"

instance Show Result
    where show (Result x) = "\n>>> Result: " ++
            case x of
                VAL Bool 1 -> "true\n\n"
                VAL Bool _ -> "false\n\n"
                VAL _    a -> show a     ++ "\n\n"
                _          -> show x     ++ "\n\n"

instance Show EmulatorState
    where show (EmulatorState (s1@State{pc=pc1, code=code}:s2@State{pc=pc2, stack=stack, heap=heap, global=global}:xs)) =
                showBoxed (show (code!!pc1)) ++ "\n\n" ++ columns pc2 stack heap ++ show (EmulatorState (s2:xs))
          show (EmulatorState [s@State{pc=pc, code=code, stack=stack, heap=heap, global=global}]) =
                showBoxed (show (code!!pc))  ++ "\n\n" ++ show (result s)
          show (EmulatorState _) = ""

showStack :: [String] -> Int -> String
showStack [] _ = ""
showStack xs n = hshowStack xs n 0
    where hshowStack (x:xs) n akk = "s" ++ show akk ++ ":" ++ indent (n - length (show akk)) ++ x ++ "\n" ++ hshowStack xs n (akk+1)
          hshowStack []     _ _   = ""

showHeap :: [HeapCell] -> Int -> String
showHeap [] _ = ""
showHeap xs n = hshowHeap xs n 0
    where hshowHeap (x:xs) n akk = "h" ++ show akk ++ ":" ++ indent (n - length (show akk)) ++ show x ++ "\n" ++ hshowHeap xs n (akk+1)
          hshowHeap []     _ _   = ""

showBoxed :: String -> String
showBoxed xs = "\n" ++ dots (length xs+4) ++ "\n: " ++ xs ++ " :\n" ++ dots (length xs+4)
    where dots 0 = ""
          dots n = "·" ++ dots (n-1)

indent :: Int -> String
indent 0                      = ""
indent n                      = " " ++ indent (n-1)

columns :: Int -> [String] -> [HeapCell] -> String
columns pc (s:stack) (h:heap) = "T:  " ++ show (length stack-1) ++ indent (15-length (show (length stack-1)))
                                       ++ "s0: " ++ indent 2 ++ s ++ indent (15-length s)
                                       ++ "h0: "++ indent 2 ++ show h ++ indent (15-length (show h))
                             ++ "\n" ++ hcolumns 1 [pc] stack heap
    where hcolumns akk (x:xs) (y:ys) (z:zs) = "PC: " ++ show x ++ indent (15-length (show x))
                                           ++ "s" ++ show akk ++ ":" ++ indent (4-length (show akk)) ++ y ++ indent (15-length y)
                                           ++ "h" ++ show akk ++ ":" ++ indent (4-length (show akk)) ++ show z
                                           ++ "\n" ++ hcolumns (akk+1) xs ys zs
          hcolumns akk []     (y:ys) (z:zs) = indent 19 
                                           ++ "s" ++ show akk ++ ":" ++ indent (4-length (show akk)) ++ y ++ indent (15-length y)
                                           ++ "h" ++ show akk ++ ":" ++ indent (4-length (show akk)) ++ show z
                                           ++ "\n" ++ hcolumns (akk+1) [] ys zs
          hcolumns akk []     (y:ys) []     = indent 19 
                                           ++ "s" ++ show akk ++ ":" ++ indent (4-length (show akk)) ++ y
                                           ++ "\n" ++ hcolumns (akk+1) [] ys []
          hcolumns akk []     []     (z:zs) = indent 40
                                           ++ "h" ++ show akk ++ ":" ++ indent (4-length (show akk)) ++ show z
                                           ++ "\n" ++ hcolumns (akk+1) [] [] zs
          hcolumns akk []     []     []     = ""
          hcolumns akk (x:xs) []     (z:zs) = "PC: " ++ show x ++ indent (15-length (show x))
                                           ++ indent 21
                                           ++ "h" ++ show akk ++ ":" ++ indent (4-length (show akk)) ++ show z
                                           ++ "\n" ++ hcolumns (akk+1) xs [] zs
columns _ _ _ = ""


---------- Tokenizer:

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
       ';' : xs       -> " ; "  ++ spaceyfier xs    -- nach dem Strichpunkt kein Leerzeichen, weil als Eingabe danach ein Leerzeichen oder Zeilenumbruch kommen MUSS!
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


---------- Parser:

showParseResult :: String -> IO()
showParseResult xs =
    case parse xs of
        Left x -> putStr x
        Right (Program xs, _) -> putStr (showBoxed "Parser" ++ "\n\n" ++ hshowParseResult xs ++ "\n")
            where
                hshowParseResult (x:xs) = show x ++ "\n" ++ hshowParseResult xs
                hshowParseResult []     = ""

parse :: String -> Either String (Program, [Token])
parse xs = parseProgram $ tokenize xs

parseProgram :: Parser Program
parseProgram xs1 = do
    (e, xs2)  <- parseDef xs1
    case xs2 of
        Semicolon : xs3 -> do
            (es, xs4) <- parseRestProgram xs3
            case xs4 of
                []    -> return (Program (e:es), xs4)
                (x:_) -> Left ("Parse error on input: " ++ show x)   -- immer wenn die Restliste nicht leer ist (wenn Code nicht vollständig geparst werden konnte) -> Fehler  
        _               -> Left ("Semicolon expected after definition " ++ showDef e)


parseRestProgram :: Parser [Definition]
parseRestProgram xs1 = do
    case xs1 of
        Name i : _ -> do
            (e, xs2) <- parseDef xs1
            case xs2 of
                Semicolon : xs3 -> do
                    (es, xs4) <- parseRestProgram xs3
                    return (e:es, xs4)
                _               -> Left ("Semicolon expected after definition " ++ showDef e)
        _          -> return ([], xs1)

parseDef :: Parser Definition
parseDef xs1 = do
    (e, xs2)  <- parseVariable xs1
    (es, xs3) <- parseRestDef xs2
    (f, xs4)  <- parseExpr xs3
    return (Definition (e:es) f, xs4)

parseRestDef :: Parser [Expression]
parseRestDef (Name i : xs1) = do
    (is, xs2) <- parseRestDef xs1
    return (Variable i:is, xs2)
parseRestDef (Equals : xs1) = return ([], xs1)
parseRestDef _              = Left "Definition incomplete"

parseLocDefs :: Parser LocDefs
parseLocDefs xs1 = do
    (e, xs2)  <- parseLocDef xs1
    (es, xs3) <- parseRestLocDefs xs2
    return (LocDefs (e:es), xs3)

parseRestLocDefs :: Parser [LocDef]
parseRestLocDefs (Comma : xs1) = do
    (e, xs2)  <- parseLocDef xs1
    (es, xs3) <- parseRestLocDefs xs2
    return (e:es, xs3)
parseRestLocDefs xs                = return ([], xs)

parseLocDef :: Parser LocDef
parseLocDef xs1 = do
    (e, xs2)  <- parseVariable xs1
    case xs2 of
        Equals : xs3 -> do
            (es, xs4) <- parseExpr xs3
            return (LocDef e es, xs4)
        _            -> Left "Local definition incomplete"

parseExpr :: Parser Expression
parseExpr (Let : xs1) = do
    (e, xs2)  <- parseLocDefs xs1
    case xs2 of
        In : xs3 -> do
            (es, xs4) <- parseExpr xs3
            return (LetX e es, xs4)
        _        -> Left "Expected 'in' after local definition"
parseExpr (If : xs1)  = do
    (e1, xs2) <- parseExpr xs1
    case xs2 of
        Then : xs3 -> do
            (e2, xs4) <- parseExpr xs3
            case xs4 of
                Else : xs5 -> do
                    (es, xs6) <- parseExpr xs5
                    return (IfX e1 e2 es, xs6)
                _          -> Left "Expected 'else' after 'then' block"
        _          -> Left "Expected 'then' after 'if' block"
parseExpr xs          = parseOrExpr xs

parseOrExpr :: Parser Expression
parseOrExpr xs1 = do
    (e, xs2)  <- parseAndExpr xs1
    case xs2 of
        Or : xs3 -> do
            (es, xs4) <- parseOrExpr xs3
            return (OrX e es, xs4)
        _        -> return (e, xs2)

parseAndExpr :: Parser Expression
parseAndExpr xs1 = do
    (e, xs2)  <- parseNotExpr xs1
    case xs2 of
        And : xs3 -> do
            (es, xs4) <- parseAndExpr xs3
            return (AndX e es, xs4)
        _         -> return (e, xs2)

parseNotExpr :: Parser Expression
parseNotExpr (Not : xs1) = do
    (e, xs2)  <- parseCompareExpr xs1
    return (NotX e, xs2)
parseNotExpr xs          = parseCompareExpr xs

parseCompareExpr :: Parser Expression
parseCompareExpr xs1 = do
    (e, xs2)  <- parseAddExpr  xs1
    case xs2 of
        LessThan : xs3 -> do
            (es, xs4) <- parseAddExpr  xs3
            return (LessThanX e es, xs4)
        Is : xs3       -> do
            (es, xs4) <- parseAddExpr  xs3
            return (IsX e es, xs4)
        _              -> return (e, xs2)

parseAddExpr  :: Parser Expression
parseAddExpr  xs1 = do
    (e, xs2)  <- parseMultExpr  xs1
    (es, xs3) <- parseRestAddExpr xs2
    return (foldl Sum e es, xs3)

parseRestAddExpr :: Parser [Expression]
parseRestAddExpr (Plus : xs1)  = do
    (e, xs2)  <- parsePositiveMultExpr xs1
    (es, xs3) <- parseRestAddExpr xs2
    return (e:es, xs3)
parseRestAddExpr (Minus : xs1) = do
    (e, xs2)  <- parsePositiveMultExpr xs1
    (es, xs3) <- parseRestAddExpr xs2
    return (Neg e:es, xs3)
parseRestAddExpr xs            = return ([], xs)

parseMultExpr  :: Parser Expression
parseMultExpr  xs1 = do
    (e, xs2)  <- parseNegExpr xs1
    (es, xs3) <- parseRestMultExpr xs2
    return (foldl Mult e es, xs3)

parsePositiveMultExpr :: Parser Expression
parsePositiveMultExpr xs1 = do
    (e, xs2)  <- parseAtomicExpr  xs1
    (es, xs3) <- parseRestMultExpr xs2
    return (foldl Mult e es, xs3)

parseRestMultExpr :: Parser [Expression]
parseRestMultExpr (Times : xs1) = do
    (e, xs2)  <- parseAtomicExpr  xs1
    (es, xs3) <- parseRestMultExpr xs2
    return (e:es, xs3)
parseRestMultExpr (DivBy : xs1) = do
    (e, xs2)  <- parseAtomicExpr  xs1
    (es, xs3) <- parseRestMultExpr xs2
    return (NegExpo e:es, xs3)
parseRestMultExpr xs            = return ([], xs)

parseNegExpr :: Parser Expression
parseNegExpr (Minus : xs1) = do
    (e, xs2)  <- parseAtomicExpr  xs1
    return (Neg e, xs2)
parseNegExpr xs            = parseAtomicExpr  xs

parseAtomicExpr  :: Parser Expression
parseAtomicExpr  (Number i : xs1)  = return (Val i, xs1)
parseAtomicExpr  (Boolean i : xs1) = return (BoolVal i, xs1)
parseAtomicExpr  (OpenPar : xs1)   = do
    (e, xs2)  <- parseExpr xs1
    case xs2 of
        ClosePar : xs3 -> return (e, xs3)
        _              -> Left "Missing ')'"
parseAtomicExpr  (Name i : xs1)    = do
    (is, xs2) <- parseRestAtomicExpr xs1
    return (foldl Function (Variable i) is, xs2)
parseAtomicExpr  _                 = Left "Expected: number, boolean or parseVariable"

parseRestAtomicExpr :: Parser [Expression]
parseRestAtomicExpr (Number i : xs1)  = do
    (is, xs2) <- parseRestAtomicExpr xs1
    return (Val i:is, xs2)
parseRestAtomicExpr (Boolean i : xs1) = do
    (is, xs2) <- parseRestAtomicExpr xs1
    return (BoolVal i:is, xs2)
parseRestAtomicExpr (OpenPar : xs1)   = do
    (e, xs2)  <- parseExpr xs1
    case xs2 of
        ClosePar : xs3 -> do
            (es, xs4) <- parseRestAtomicExpr xs3
            return (e:es, xs4)
        _              -> Left "Missing ')'"
parseRestAtomicExpr (Name i : xs1)    = do
    (is, xs2) <- parseRestAtomicExpr xs1
    return (Variable i:is, xs2)
parseRestAtomicExpr xs                = return ([], xs)

parseVariable :: Parser Expression
parseVariable (Name i : xs) = return (Variable i, xs)
parseVariable _             = Left "parseVariable expected"

--- Hilfsfunktion Parser:

showDef :: Definition -> String
showDef (Definition (Variable a : _)_) = a



---------- Compiler:

compile :: String -> Either String State
compile xs =
    case parse xs of
        (Right (Program xs, [])) -> return (compileProgram xs)
        Left string              -> Left string

compileProgram :: [Definition] -> State
compileProgram = foldl compileDef State{pc=0, code=initCode, stack=[], heap=[], global=[]}
  where
    compileDef s@State{code = code, heap = heap, global = global} def@(Definition (Variable fun:args) body) =
      s { code   = code ++ codeDef def
        , heap   = heap ++ [DEF fun (length args) (length code)]
        , global = global ++ [(fun, length heap)] }

codeDef :: Definition -> [Instruction]
codeDef (Definition (Variable fun:args) body) = codeExpr body (buildEnv args) ++ [Updatefun n, Slide (n + 1), Unwind, Call, Return]
    where
        n = length args

codeExpr :: Expression -> [(Expression, Int)] -> [Instruction]
codeExpr (LetX      (LocDefs a) b)   env = codeLocDefs a envLet ++ codeExpr b envLet ++ [Slidelet n]
    where
        n = length a
        envLet = [(v, pos-1) | (v, pos) <- buildEnvLet a] ++ [(v, pos+n) | (v, pos) <- env]
codeExpr (IfX                a  b c) env = codeExpr c env ++ codeExpr b [(v, pos+1) | (v, pos) <- env] ++ codeExpr a [(v, pos+2) | (v, pos) <- env] ++ [Pushpre If, Makeapp, Makeapp, Makeapp]
codeExpr (NotX               a)      env = codeExpr a env ++ [Pushpre Not, Makeapp]
codeExpr (Neg                a)      env = codeExpr a env ++ [Pushpre Minus, Makeapp]
codeExpr (NegExpo            a)      env = codeExpr a env ++ [Pushpre DivBy, Makeapp] -- Token DivBy hier 1/x da kein NegExpo Token existiert
codeExpr (OrX                a  b)   env = codeExpr (IfX a   (BoolVal True) b)  env
codeExpr (AndX               a  b)   env = codeExpr (IfX a b (BoolVal False))   env
codeExpr (LessThanX          a  b)   env = codeExpr b env ++ codeExpr a  [(v, pos+1) | (v, pos) <- env] ++ [Pushpre LessThan, Makeapp, Makeapp]
codeExpr (IsX                a  b)   env = codeExpr b env ++ codeExpr a  [(v, pos+1) | (v, pos) <- env] ++ [Pushpre Is, Makeapp, Makeapp]
codeExpr (Sum                a  b)   env = codeExpr b env ++ codeExpr a  [(v, pos+1) | (v, pos) <- env] ++ [Pushpre Plus, Makeapp, Makeapp]
codeExpr (Mult               a  b)   env = codeExpr b env ++ codeExpr a  [(v, pos+1) | (v, pos) <- env] ++ [Pushpre Times, Makeapp, Makeapp]
codeExpr (Function           a  b)   env =
    case pos b env of
        Nothing -> codeExpr b env ++ codeExpr a env ++ [Makeapp]
        _       -> codeExpr b env ++ codeExpr a [(v, pos+1) | (v, pos) <- env] ++ [Makeapp]
codeExpr (Val                a)      env = [Pushval Float (int2Float a)]
codeExpr (BoolVal            a)      env = [Pushval Bool x]
    where x | a         = 1
            | not a     = 0
codeExpr (Variable           a)      env =
    case pos (Variable a) env of
        Nothing -> [Pushfun a]
        Just x  -> [Pushparam x]

codeLocDefs :: [LocDef] -> [(Expression, Int)] -> [Instruction]
codeLocDefs x env = alloc n ++ cLocDefs x env n
    where
        n                                     = length x
        alloc 0                               = []
        alloc x                               = [Alloc, Alloc, Makeapp] ++ alloc (x-1)
        cLocDefs ((LocDef var expr):xs) env n = codeExpr expr env ++ [Updatelet (n-1)] ++ cLocDefs xs env (n-1)
        cLocDefs []                     _   _ = []

--- Hilfsfunktionen Compiler:

initCode :: [Instruction]
initCode = [Reset, Pushfun "main", Call, Halt]
    ++ [Pushparam 1, Unwind, Call, Pushparam 3, Unwind, Call, Operator BinaryOp, Updateop, Return] --BinärOp
    ++ [Pushparam 1, Unwind, Call, Operator IfOp, Updateop, Unwind, Call, Return]                  --If
    ++ [Pushparam 1, Unwind, Call, Operator UnaryOp, Updateop, Return]                             --UnärOp

buildEnv :: [Expression] -> [(Expression, Int)]
buildEnv xs = pos xs 1
    where
        pos [] _ = []
        pos (x:xs) akk = (x, akk) : pos xs (akk + 1)

buildEnvLet :: [LocDef] -> [(Expression, Int)]
buildEnvLet xs = posL xs n
    where
        n = length xs
        posL ((LocDef var _):xs) n = (var, n-1) : posL xs (n-1)
        posL []                  n = []

pos :: Expression -> [(Expression, Int)] -> Maybe Int
pos _ []                      = Nothing
pos s ((x, i):xs) | s == x    = return i
                  | otherwise = pos s xs


----------------- functions for nice outputs of emulator:


showEmulate :: String -> IO()
showEmulate xs = case compile xs of
                    Left x -> putStr x
                    Right x -> putStr $ show (EmulatorState (showRun x))

showRun :: State -> [State]
showRun s@State{pc = pc, code = code, stack = stack, heap = heap, global = global} =
    if not (mainInGlobal global) then throw MissingMain else
        let i = code!!pc in
            if i /= Halt then
                    s:showRun(s { pc    = runPC i pc stack heap
                                , stack = runStack i pc stack heap global
                                , heap  = runHeap i stack heap })
                         else [s]

----------------- Emulator:

emulate :: String -> IO()
emulate xs =
    case compile xs of
        Right x -> putStr $ show (result (run x))
        Left x  -> putStr x

result :: State -> Result
result (State _ _ s h _) = Result (val (h!!getAddress (last s)) h)

run :: State -> State
run s@State{pc = pc, code = code, stack = stack, heap = heap, global = global} =
    if not (mainInGlobal global) then throw MissingMain else
        let i = code!!pc in
            if i /= Halt then
                           run s { pc    = runPC i pc stack heap
                                 , stack = runStack i pc stack heap global
                                 , heap  = runHeap i stack heap }
                         else s

-- p = pc
-- s = stack
-- h = heap
-- g = global

runPC :: Instruction -> Int -> [String] -> [HeapCell] -> Int
runPC Unwind p s h =
    case val (h!!getAddress (last s)) h of
        APP _ _ -> p
        _       -> p+1
runPC Call   p s h =
    case val (h!!getAddress (last s)) h of
        DEF _ _ adr    -> adr
        PRE _ BinaryOp -> 4
        PRE _ IfOp     -> 13
        PRE _ UnaryOp  -> 21
        _              -> p+1
runPC Return p s _ = getAddress (s!!(length s-2))
runPC _      p _ _ = p+1

runStack :: Instruction -> Int -> [String] -> [HeapCell] -> [(String, Int)] -> [String]
runStack (Pushfun arg)   _ s _ g = s ++ ["h" ++ show (address arg g)]
runStack (Pushval t v)   _ s h _ = s ++ ["h" ++ show (length h)] -- new
runStack (Pushparam arg) _ s h _ = s ++ ["h" ++ show (add2arg (s!!(length s-arg-2)) h)]
runStack Makeapp         _ s h _ = init (init s) ++ ["h" ++ show (length h)]
runStack (Slide arg)     _ s _ _ = slider (length s-arg-2) s 0 ++ [s!!(length s-2), s!!(length s-1)]
runStack Unwind          _ s h _ =
    case val (h!!getAddress (last s)) h of
        APP x _ -> s ++ ["h" ++ show x]
        _       -> s
runStack Call            p s h _ =
    case val (h!!getAddress (last s)) h of
        DEF {}  -> s ++ ["c" ++ show (p+1)]
        PRE _ _ -> s ++ ["c" ++ show (p+1)]
        _       -> s
runStack Return          _ s _ _ = init (init s) ++ [last s]
runStack (Pushpre op)    _ s h _ = s ++ ["h" ++ show (length h)]
runStack Updateop        _ s h _ = init (init $ init s) ++ [s!!(length s - 2), s!!(length s - 3)]
runStack (Operator op)   _ s h _ =
    case op of
        UnaryOp  -> init (init $ init s) ++ [s!!(length s-2), "h" ++ show (length h)]
        BinaryOp -> init (init $ init $ init $ init s) ++ [s!!(length s-3), "h" ++ show (length h)]
        _        -> let a = val (h!!getAddress (last s)) h
                    in init (init $ init $ init $ init s) ++ [s!!(length s-2)] ++
                        case a of
                            VAL Bool 1 -> ["h" ++ show (add2arg(s!!(length s-5)) h)]
                            VAL Bool _ -> ["h" ++ show (add2arg(s!!(length s-6)) h)]
                            _          -> throw (NoValueFound (show op ++ show a))
runStack Alloc           _ s h _ = s ++ ["h" ++ show (length h)]
runStack (Updatelet _)   _ s h _ = init s
runStack (Slidelet arg)  _ s _ _ = slider (length s-arg-1) s 0 ++ [last s]
runStack _               _ s _ _ = s

runHeap :: Instruction -> [String] -> [HeapCell] -> [HeapCell]
runHeap (Pushval t v) _ h = h ++ [VAL t v]
runHeap Makeapp       s h = h ++ [APP (getAddress (last s))  (getAddress (s!!(length s-2)))]
runHeap (Pushpre op)  _ h = h ++ [PRE op (arity op)]
runHeap Updateop      s h = insert1 (getAddress (s!!(length s-3))) 0 h ++ [h!!getAddress (last s)] ++ insert2 (getAddress (s!!(length s-3))) h -- val(h!!...) h??
runHeap (Updatefun f) s h = insert1 (getAddress (s!!(length s-f-3))) 0 h ++ [IND (getAddress (last s))] ++ insert2 (getAddress (s!!(length s-f-3))) h
runHeap (Operator op) s h =
    case op of
        UnaryOp  -> let a = val (h!!getAddress (s!!(length s-3))) h
                        b = val (h!!getAddress (last s)) h
                    in
                        case a of
                            PRE op _ ->
                                case b of
                                    VAL t v -> h ++ [VAL t (compute op v 0)]
                                    _       -> throw (NoValueFound (show b ++ show s ++ "1"))
                            _        -> throw (NoValueFound (show a ++ show s ++ "2"))
        BinaryOp -> let a = val (h!!getAddress (s!!(length s-4))) h
                        b = val (h!!getAddress (s!!(length s-2))) h
                        c = val (h!!getAddress (last s)) h
                    in
                        case a of
                            PRE op _ ->
                                case b of
                                    VAL _ v1 ->
                                        case c of
                                            VAL _ v2 -> h ++ [VAL (findType op) (compute op v1 v2)]
                                            _        -> throw (NoValueFound (show c ++ show s ++ "3"))
                                    _        -> throw (NoValueFound (show b ++ show s ++ show h++ "4"))
                            _ -> throw (NoValueFound (show c ++ show s ++ "5"))
        _        -> h
runHeap Alloc         _ h = h ++ [UNINITIALIZED]
runHeap (Updatelet n) s h = insert1 (add2arg (s!!(length s-n-2)) h) 0 h ++ [IND (getAddress (last s))] ++ insert2 (add2arg (s!!(length s-n-2)) h) h
runHeap _             _ h = h

compute :: Token -> Value -> Value -> Value
compute Not      x _ | x == 0    = 1
                     | otherwise = 0
compute Minus    x _ = - x
compute DivBy    x _ = 1 / x
compute LessThan x y | x < y     = 1
                     | otherwise = 0
compute Is       x y | x == y    = 1
                     | otherwise = 0
compute Plus     x y = x + y
compute Times    x y = x * y

--- Hilfsfunktionen Emulator:

-- sucht in der globalen Umgebung nach einem Funktionsnamen und liefert die HeapAdresse dieser Definition
address :: String -> [(String, Int)] -> Int
address arg ((x, y):xs)
    | x == arg  = y
    | otherwise = address arg xs
address arg []    = throw (VariableNotInScope arg)     -- Fehler wird z.B. bei f a b = x; geworfen 

-- liefert das 2. Argument einer APP-Zelle an einer bestimmten Heap-Adresse
add2arg :: String -> [HeapCell] -> Int
add2arg adr h =
    case val (h!!getAddress adr) h of
        APP _ x -> x
        _       -> throw WrongAddress

-- liefert den Typ einer VAL-Zelle an einer bestimmten Heap-Adresse
typ :: Int -> [HeapCell] -> Type
typ adr h =
    case val (h!!adr) h of
        VAL Float _ -> Float
        _              -> Bool

-- liefert den Wert einer IND-Zelle an einer bestimmten Heap-Adresse
val :: HeapCell -> [HeapCell] -> HeapCell
val x h =
    case x of
        IND adr -> val (h!!adr) h
        _       -> x

slider :: Int -> [String] -> Int -> [String]
slider n s akk | n > 0     = s!!akk : slider (n-1) s (akk+1)
               | otherwise = []

findType :: Token -> Type
findType Plus  = Float
findType Times = Float
findType _     = Bool

-- prüft, ob in Global eine main Funktion vorhanden ist                     
mainInGlobal :: [([Char], b)] -> Bool
mainInGlobal (("main",_):xs) = True
mainInGlobal (x:xs)          = mainInGlobal xs
mainInGlobal []              = False

insert1 :: Int -> Int -> [HeapCell] -> [HeapCell]
insert1 adr akk h | adr > 0          = h!!akk : insert1 (adr-1) (akk+1) h
                  | otherwise        = []

insert2 :: Int -> [HeapCell]-> [HeapCell]
insert2 adr h   | adr < length h-1  = h!!(adr+1)  : insert2 (adr+1) h
                | otherwise         = []

arity :: Token -> Op
arity If    = IfOp
arity Not   = UnaryOp
arity Minus = UnaryOp
arity DivBy = UnaryOp
arity _     = BinaryOp

getAddress :: String -> Int
getAddress xs = read (tail xs) :: Int


---------- test examples:

e1 = emulate "bool x = ((x == true) | (x == false)); f x = if bool x | x < 1 then 1 else x * f (x - 1); main = f 6;"
e2 = emulate "main = quadrat (quadrat (3 * 1)); quadrat x = x * x;"
e3 = emulate "main = f 3; f x = let y = 5, z = false in if (y < x) == z then x / (y / 3) else f (x - 1); f = 1;"
e4 = emulate "main = 1;"
e5 = emulate "main = 1+2;"
e6 = emulate "f x y = 1;main = e;" --passt -- VariableNotInScope e
e7 = emulate "f x y = x + y;" --passt -- MissingMain
e8 = emulate "f x y = c; main = f 3 4;"
e9 = emulate "vector x y = (x y); main = vector 3 2;" -- Prelude.!!: negative index 
e10 = emulate "id x = x; main = id 1;"
e11 = emulate "vektorpr x y z w = x*z + y*w; main = vektorpr 1 2 3 4;"
e12 = emulate "main = #;"
e13 = emulate " main = 3; +" -- passt -- "parse error on input: +" 