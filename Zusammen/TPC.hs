import Data.Char(isDigit, isAlpha, isAlphaNum)
import Control.Exception (Exception, throw)
import GHC.Float (int2Float)

data CompilerException 
    = InvalidName !String
    | WrongAddress
    | NoValueFound !String
    | MissingMain
    | TypeCheck !String
    | VariableNotInScope !String -- Nochmal checken, welche wir im Endeffekt wirklich brauchen
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
    deriving (Eq, Show)

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

newtype Program = Program [Definition] deriving Show

data Definition = Definition [Expression] Expression deriving Show

newtype LocDefs = LocDefs [LocDef] deriving (Eq, Show)

data LocDef     = LocDef Expression Expression deriving (Eq, Show)

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
    | Pushpre Token --vorher Op
    | Updatefun Int
    | Updateop
    | Operator Op
    | Alloc -- ab hier extra für Let, siehe Zhus Skript S. 25
    | Updatelet Int
    | Slidelet Int
    deriving Eq
    
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
          show (Updatefun i)      = "Update " ++ show i
          show Updateop           = "Update op"
          show (Operator o)       = "Operator " ++ show o
          show Alloc              = "Alloc"
          show (Updatelet i)      = "Updatelet " ++ show i
          show (Slidelet i)       = "Slidelet " ++ show i

          
data Type  = Float | Bool deriving (Eq, Show)  -- 1 = Bool, 0 = Zahl
type Value = Float    -- 0 = False, 1 = True oder jede andere Zahl falls Type 0

data State = State
    {
        pc     :: Int,
        code   :: [Instruction],
        stack  :: Stack,
        heap   :: Heap,
        global :: Global
    } -- deriving Show

instance Show State
    where show (State _ code stack heap global) = "\n\n················\n: Instructions :\n················\n" ++ showCode code 0
                                               ++ "\n·········\n: Stack :\n·········\n"                        ++ showStack stack 0
                                               ++ "\n········\n: Heap :\n········\n"                           ++ showHeap heap 0
                                               ++ "\n··········\n: Global :\n··········\n"                     ++ showGlobal global
                                               ++ "\n\n\n>>> Result: "                                         ++ showResult (val(last heap) heap)
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
                name n (x:xs)                 =     -- Bringt mit emulate nichts, nur mit compile
                    case x of
                        DEF f _ adr -> if n == adr then f else name n xs
                        _           -> ""
                name _ []                     = ""
                showStack (x:xs) akk          = "s" ++ show akk ++ ":" ++ indent (4 - length (show akk)) ++ show x ++ "\n" ++ showStack xs (akk+1)
                showStack [] _                = ""
                showHeap (x:xs) akk           = "h" ++ show akk ++ ":" ++ indent (4 - length (show akk)) ++ show x ++ "\n" ++ showHeap xs (akk+1)
                showHeap [] _                 = ""
                showGlobal ((x, y):xs)        = "h" ++ show y ++ ":" ++ indent (4 - length (show y)) ++ x ++ "\n" ++ showGlobal xs
                showGlobal []                 = ""
                indent 0                      = ""
                indent n                      = " " ++ indent (n-1)
                showResult x                  =
                    case x of
                        VAL Bool 1 -> show True
                        VAL Bool _ -> show False
                        VAL _    a -> show a
                        _          -> show x
    
type Stack  = [Int] -- speichert Adressen von auszuwertenden Ausdrücken (heap)
type Heap   = [HeapCell]
type Global = [(String, Int)]

data HeapCell 
    = APP Int Int           -- Konstruktor für Knoten
    | DEF String Int Int    -- DEF f N Code-Adr, f = Funktion, N = Stelligkeit -- HeapCell statt Int (IND?)
    | VAL Type Value        -- Blätter
    | IND Int               -- HeapAdress
    | PRE Token Op
    | UNINITIALIZED

instance Show HeapCell
    where show (APP a b  )   = "APP " ++ show a ++ " " ++ show b
          show (DEF f x y)   = "DEF " ++ f ++ " " ++ show x ++ " " ++ show y
          show (VAL t v  )   = "VAL " ++ show t ++ " " ++ show v
          show (IND x    )   = "IND " ++ show x
          show (PRE t op )   = "PRE " ++ show t ++ " " ++ show op
          show UNINITIALIZED = ""


data Op
    = UnaryOp
    | BinaryOp
    | IfOp
    deriving Eq

instance Show Op
    where show UnaryOp  = "1"
          show BinaryOp = "2"
          show IfOp     = "if"

----------------------------------------------------------------------------------------------------------------------------------------------

tokenize xs = tokenizer $ words $ spaceyfier xs

spaceyfier :: String -> String
spaceyfier x =
   case x of
       ';' : xs       -> " ;"   ++ spaceyfier xs        -- hier kein Leerzeichen danach, da Strichpunkte in Namen eh nicht erlaubt sind!
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
tokenizer (","     : xs) = Comma         : tokenizer xs
tokenizer ("="     : xs) = Equals        : tokenizer xs
tokenizer ("true"  : xs) = Boolean True  : tokenizer xs -- klein wär schöner und ala Skript aber input soll output matchen! (was ist output lol)
tokenizer ("false" : xs) = Boolean False : tokenizer xs
tokenizer []             = []
tokenizer (x:xs)
    | checkNumber x                   = Number (read x) : tokenizer xs
    | isAlpha (head x) && checkName x = Name x          : tokenizer xs
    | otherwise                       = throw (InvalidName x)

checkNumber :: String -> Bool
checkNumber (x:xs) = isDigit x && checkNumber xs
checkNumber []     = True

checkName :: String -> Bool
checkName (x:xs) = (isAlphaNum x || x == '_' || x == '\'') && checkName xs
checkName []     = True

-- tokenizer $ words $ spaceyfier xs


----------------------------------------------------------------------------------------------------------------------------------------------

--hilfsfunktion für fehlermeldungen
show2 :: Definition -> String
show2 (Definition (Variable a : _)_) = a -- brauchen wir das noch für die fehlermeldungen?

program :: Parser Program
program xs1 = do
    (e, xs2)  <- def xs1
    case xs2 of
        Semicolon : xs3 -> do
            (es, xs4) <- restProgram xs3
            case xs4 of
                []    -> return (Program (e:es), xs4)
                (x:_) -> Left ("Parse error on input: " ++ show x)   -- immer wenn die Restliste nicht leer ist (wenn Code nicht vollständig geparst werden konnte) -> Fehler 
                -- Hier die einzige Stelle an der Token bei einer Fehlermeldung einfach so ausgegeben werden, eventuell eine showToken Funktion implementieren, 
                -- um immer schön anzeigen zu können wo die Fehler stattfinden. 
        _               -> Left ("Semicolon expected after definition " ++ show2 e)

restProgram :: Parser [Definition]
restProgram xs1 = do
    case xs1 of
        Name i : _ -> do
            (e, xs2) <- def xs1
            case xs2 of
                Semicolon : xs3 -> do
                    (es, xs4) <- restProgram xs3
                    return (e:es, xs4)
                _               -> Left ("Semicolon expected after definition " ++ show2 e)
        _          -> return ([], xs1)

def :: Parser Definition
def xs1 = do
    (e, xs2)  <- variable xs1
    (es, xs3) <- restDef xs2
    (f, xs4)  <- expr xs3
    return (Definition (e:es) f, xs4)

restDef :: Parser [Expression]
restDef (Name i : xs1) = do
    (is, xs2) <- restDef xs1
    return (Variable i:is, xs2)
restDef (Equals : xs1) = return ([], xs1)
restDef _              = Left "Definition incomplete"

locDefs :: Parser LocDefs
locDefs xs1 = do
    (e, xs2)  <- locDef xs1
    (es, xs3) <- restLocDefs xs2
    return (LocDefs (e:es), xs3)

restLocDefs :: Parser [LocDef]
restLocDefs (Comma : xs1) = do
    (e, xs2)  <- locDef xs1
    (es, xs3) <- restLocDefs xs2
    return (e:es, xs3)
restLocDefs xs                = return ([], xs)

locDef :: Parser LocDef
locDef xs1 = do
    (e, xs2)  <- variable xs1
    case xs2 of
        Equals : xs3 -> do
            (es, xs4) <- expr xs3
            return (LocDef e es, xs4)
        _            -> Left "Local definition incomplete"

----------------------------------------------------------------------------------------------------------------------------------------------

expr :: Parser Expression
expr (Let : xs1) = do
    (e, xs2)  <- locDefs xs1
    case xs2 of
        In : xs3 -> do
            (es, xs4) <- expr xs3
            return (LetX e es, xs4)
        _        -> Left "Expected 'in' after local definition"
expr (If : xs1)  = do
    (e1, xs2) <- expr xs1
    case xs2 of
        Then : xs3 -> do
            (e2, xs4) <- expr xs3
            case xs4 of
                Else : xs5 -> do
                    (es, xs6) <- expr xs5
                    return (IfX e1 e2 es, xs6)
                _          -> Left "Expected 'else' after 'then' block"
        _          -> Left "Expected 'then' after 'if' block"
expr xs          = orExpr xs

orExpr :: Parser Expression
orExpr xs1 = do
    (e, xs2)  <- andExpr xs1
    case xs2 of
        Or : xs3 -> do
            (es, xs4) <- orExpr xs3
            return (OrX e es, xs4)
        _        -> return (e, xs2)

andExpr :: Parser Expression
andExpr xs1 = do
    (e, xs2)  <- notExpr xs1
    case xs2 of
        And : xs3 -> do
            (es, xs4) <- andExpr xs3
            return (AndX e es, xs4)
        _         -> return (e, xs2)

notExpr :: Parser Expression
notExpr (Not : xs1) = do
    (e, xs2)  <- compareExpr xs1
    return (NotX e, xs2)
notExpr xs          = compareExpr xs

compareExpr :: Parser Expression
compareExpr xs1 = do
    (e, xs2)  <- addExpr xs1
    case xs2 of
        LessThan : xs3 -> do
            (es, xs4) <- addExpr xs3
            return (LessThanX e es, xs4)
        Is : xs3       -> do
            (es, xs4) <- addExpr xs3
            return (IsX e es, xs4)
        _              -> return (e, xs2)

----------------------------------------------------------------------------------------------------------------------------------------------

addExpr :: Parser Expression
addExpr xs1 = do
    (e, xs2)  <- multExpr xs1
    (es, xs3) <- restAddExpr xs2
    return (foldl Sum e es, xs3)
    
restAddExpr :: Parser [Expression]
restAddExpr (Plus : xs1)  = do
    (e, xs2)  <- multExpr2 xs1              -- damit 5+-2 nicht geht
    (es, xs3) <- restAddExpr xs2
    return (e:es, xs3)
restAddExpr (Minus : xs1) = do
    (e, xs2)  <- multExpr2 xs1
    (es, xs3) <- restAddExpr xs2
    return (Neg e:es, xs3)
restAddExpr xs            = return ([], xs)

multExpr :: Parser Expression
multExpr xs1 = do
    (e, xs2)  <- negExpr xs1
    (es, xs3) <- restMultExpr xs2
    return (foldl Mult e es, xs3)

multExpr2 :: Parser Expression              -- falls keine Negation möglich sein soll, zB (5--3) ist doof
multExpr2 xs1 = do
    (e, xs2)  <- atomicExpr xs1
    (es, xs3) <- restMultExpr xs2
    return (foldl Mult e es, xs3)-- case xs2 of 

restMultExpr :: Parser [Expression]
restMultExpr (Times : xs1) = do
    (e, xs2)  <- atomicExpr xs1             -- damit 5--2 nicht geht
    (es, xs3) <- restMultExpr xs2
    return (e:es, xs3)
restMultExpr (DivBy : xs1) = do
    (e, xs2)  <- atomicExpr xs1
    (es, xs3) <- restMultExpr xs2
    return (NegExpo e:es, xs3)
restMultExpr xs            = return ([], xs)

negExpr :: Parser Expression
negExpr (Minus : xs1) = do
    (e, xs2)  <- atomicExpr xs1
    return (Neg e, xs2)
negExpr xs            = atomicExpr xs

atomicExpr :: Parser Expression
atomicExpr (Number i : xs1)  = return (Val i, xs1) -- do
    -- (is, xs2) <- restAtomicExpr xs1
    -- return (foldl Function (Val i) is, xs2)
atomicExpr (Boolean i : xs1) = return (BoolVal i, xs1) -- do
    -- (is, xs2) <- restAtomicExpr xs1
    -- return (foldl Function (BoolVal i) is, xs2)
atomicExpr (OpenPar : xs1)   = do
    (e, xs2)  <- expr xs1
    case xs2 of
        ClosePar : xs3 -> return (e, xs3) -- do
            -- (es, xs4) <- restAtomicExpr xs3
            -- return (foldl Function e es, xs4)
        _              -> Left "Missing ')'"
atomicExpr (Name i : xs1)    = do
    (is, xs2) <- restAtomicExpr xs1
    return (foldl Function (Variable i) is, xs2)
atomicExpr _                 = Left "Expected: number, boolean or variable"

restAtomicExpr :: Parser [Expression]
restAtomicExpr (Number i : xs1)  = do
    (is, xs2) <- restAtomicExpr xs1
    return (Val i:is, xs2)
restAtomicExpr (Boolean i : xs1) = do
    (is, xs2) <- restAtomicExpr xs1
    return (BoolVal i:is, xs2)
restAtomicExpr (OpenPar : xs1)   = do
    (e, xs2)  <- expr xs1
    case xs2 of
        ClosePar : xs3 -> do
            (es, xs4) <- restAtomicExpr xs3
            return (e:es, xs4)
        _              -> Left "Missing ')'"
restAtomicExpr (Name i : xs1)    = do
    (is, xs2) <- restAtomicExpr xs1
    return (Variable i:is, xs2)
restAtomicExpr xs                = return ([], xs)

variable :: Parser Expression
variable (Name i : xs) = return (Variable i, xs)
variable _             = Left "Variable expected"

---------- Compiler:

posifyer :: [Expression] -> [(Expression, Int)]
posifyer xs = pos xs 1
    where 
        pos [] _ = []
        pos (x:xs) akk = (x, akk) : pos xs (akk + 1)

pos :: Expression -> [(Expression, Int)] -> Maybe Int
pos _ []                      = Nothing
pos s ((x, i):xs) | s == x    = return i
                  | otherwise = pos s xs

---------- compileFunktionen:

compileProgram :: [Definition] -> State
compileProgram = foldl modifyStateForOneDef State{pc=0, code=startList, stack=[], heap=[], global=[]}
  where
    modifyStateForOneDef :: State -> Definition -> State
    modifyStateForOneDef s@State{code = code, heap = heap, global = global} def@(Definition (Variable fun:args) body) =
      s { code   = code ++ compileDef def
        , heap   = heap ++ [DEF fun (length args) (length code)]
        , global = global ++ [(fun, length heap)] }

startList :: [Instruction]
startList = [Reset, Pushfun "main", Call, Halt]
    ++ [Pushparam 1, Unwind, Call, Pushparam 3, Unwind, Call, Operator BinaryOp, Updateop, Return] --BinärOp
    ++ [Pushparam 1, Unwind, Call, Operator IfOp, Updateop, Unwind, Call, Return]                  --If
    ++ [Pushparam 1, Unwind, Call, Operator UnaryOp, Updateop, Return]                             --UnärOp

-------------

compileDef :: Definition -> [Instruction]
compileDef (Definition (Variable fun:args) body) = do
    case body of
        Variable x -> [Pushfun x, Updatefun n, Slide (n + 1), Unwind, Call, Return]
        _          -> compileExpr body (posifyer args) ++ [Updatefun n, Slide (n + 1), Unwind, Call, Return]
    where
        n = length args

compileExpr :: Expression -> [(Expression, Int)] -> [Instruction]
compileExpr (LetX      (LocDefs a) b)   env = compileLocDefs a envLet ++ compileExpr b envLet ++ [Slidelet n]
    where
        n = length a -- Anzahl der Lokaldefinitionen
        envLet = [(v, pos-1) | (v, pos) <- posifyerLet a] ++ [(v, pos+n) | (v, pos) <- env]

compileExpr (IfX                a  b c) env = compileExpr c env ++ compileExpr b [(v, pos+1) | (v, pos) <- env] ++ compileExpr a [(v, pos+2) | (v, pos) <- env] ++ [Pushpre If, Makeapp, Makeapp, Makeapp]

compileExpr (NotX               a)      env = compileExpr a env ++ [Pushpre Not, Makeapp]
compileExpr (Neg                a)      env = compileExpr a env ++ [Pushpre Minus, Makeapp]
compileExpr (NegExpo            a)      env = compileExpr a env ++ [Pushpre DivBy, Makeapp] -- Token DivBy hier 1/x da kein NegExpo Token existiert

compileExpr (OrX                a  b)   env = compileExpr (IfX a (BoolVal True) b)  env
compileExpr (AndX               a  b)   env = compileExpr (IfX a b (BoolVal False)) env 

compileExpr (LessThanX          a  b)   env = compileExpr b env ++ compileExpr a  [(v, pos+1) | (v, pos) <- env] ++ [Pushpre LessThan, Makeapp, Makeapp]
compileExpr (IsX                a  b)   env = compileExpr b env ++ compileExpr a  [(v, pos+1) | (v, pos) <- env] ++ [Pushpre Is, Makeapp, Makeapp]
compileExpr (Sum                a  b)   env = compileExpr b env ++ compileExpr a  [(v, pos+1) | (v, pos) <- env] ++ [Pushpre Plus, Makeapp, Makeapp]
compileExpr (Mult               a  b)   env = compileExpr b env ++ compileExpr a  [(v, pos+1) | (v, pos) <- env] ++ [Pushpre Times, Makeapp, Makeapp]
compileExpr (Function           a  b)   env = 
    case pos b env of
        Nothing -> compileExpr b env ++ compileExpr a env ++ [Makeapp]
        _       -> compileExpr b env ++ compileExpr a [(v, pos+1) | (v, pos) <- env] ++ [Makeapp]
compileExpr (Val                a)      env = [Pushval Float (int2Float a)]
compileExpr (BoolVal            a)      env = [Pushval Bool x] -- x ist 0 oder 1  
    where x | a         = 1
            | not a     = 0
compileExpr (Variable           a)      env = 
    case pos (Variable a) env of
        Nothing -> [Pushfun a]
        Just x  -> [Pushparam x]

compileLocDefs :: [LocDef] -> [(Expression, Int)] -> [Instruction]
compileLocDefs x env = alloc n ++ cLocDefs x env n
    where
        n                                     = length x -- Anzahl der Lokaldefinitionen
        alloc 0                               = []
        alloc x                               = [Alloc, Alloc, Makeapp] ++ alloc (x-1)
        cLocDefs ((LocDef var expr):xs) env n = compileExpr expr env ++ [Updatelet (n-1)] ++ cLocDefs xs env (n-1)
        cLocDefs []                     _   _ = []

posifyerLet :: [LocDef] -> [(Expression, Int)]
posifyerLet xs = posL xs n
    where 
        n = length xs
        posL ((LocDef var _):xs) n = (var, n-1) : posL xs (n-1)
        posL []                  n = []

---------- combining functions

parse :: String -> Either String (Program, [Token])
parse xs = program $ tokenize xs

compile :: String -> Either String State
compile xs = 
    case parse xs of
        (Right (Program xs, [])) -> return (compileProgram xs)
        Left string              -> Left string

----------------- Emulator:

-- interprete :: Either String State -> Either String State
-- interprete xs = 
--     case xs of
--         Left xs     -> Left xs
--         Right s     -> return (hCode s)

-- run :: State -> State
-- run s@State{pc = pc, code = code, stack = stack, heap = heap, global = global} = s{pc = pc+1, stack = (\s@State{stack = stack} -> stack) (hCode s)}

----------Hilfsfunktionen Interpreter ala Skript:

-- s = stack
-- h = heap
-- g = global

-- address sucht in der globalen Umgebung nach dem Funktionsnamen einer Definition und liefert die HeapAdresse dieser Definition
address :: String -> [(String, Int)] -> Int
address arg ((x, y):xs)
    | x == arg  = y
    | otherwise = address arg xs
address _ []    = throw WrongAddress

-- liefert das 2. Argument einer APP-Zelle an einer bestimmten Heap-Adresse
add2arg :: Int -> [HeapCell] -> Int
add2arg adr h =
    case h!!adr of
        APP _ x -> x
        _       -> throw WrongAddress
        
-- liefert den Typ einer VAL-Zelle an einer bestimmten Heap-Adresse
typ :: Int -> [HeapCell] -> Type
typ adr h =
    case h!!adr of
        VAL Float _ -> Float
        _              -> Bool

-- liefert den Wert einer IND-Zelle an einer bestimmten Heap-Adresse
val :: HeapCell -> [HeapCell] -> HeapCell
val x h =
    case x of
        IND adr -> val (h!!adr) h
        _       -> x

---------------

emulate :: String -> Either String State
emulate xs = 
    case compile xs of
        Right x -> return (run x)
        Left x  -> Left x

run :: State -> State
run s@State{pc = pc, code = code, stack = stack, heap = heap, global = global} = 
    let i = code!!pc in
    if i /= Halt then run s { pc    = pcf i pc stack heap
                            , stack = stackf i pc stack heap global
                            , heap  = heapf i stack heap }
                 else s

pcf :: Instruction -> Int -> [Int] -> [HeapCell] -> Int
pcf Unwind p s h = 
    case val (h!!last s) h of
        APP _ _ -> p
        _       -> p+1
pcf Call   p s h =
    case val (h!!last s) h of
        DEF _ _ adr    -> adr
        PRE _ BinaryOp -> 4
        PRE _ IfOp     -> 13
        PRE _ UnaryOp  -> 21
        _              -> p+1 -- VAL
pcf Return p s _ = s!!(length s-2)
pcf _      p _ _ = p+1

-- falls new hier nicht klappt, dann für die funktionen die es benötigen
-- (Makeapp, Pushval,...) oben cases im run erstellen!
stackf :: Instruction -> Int -> [Int] -> [HeapCell] -> [(String, Int)] -> [Int]
stackf (Pushfun arg)   _ s _ g = s ++ [address arg g]
stackf (Pushval t v)   _ s h _ = s ++ [length h] -- new
stackf (Pushparam arg) _ s h _ = s ++ [add2arg (s!!(length s-arg-2)) h] -- hier -3 da length s eins zu groß ist
stackf Makeapp         _ s h _ = init (init s) ++ [length h] -- letztes stack element überschreiben (t-1 im skript)
stackf (Slide arg)     _ s _ _ = slider (length s-arg-2) s 0 ++ [s!!(length s-2), s!!(length s-1)]
stackf Unwind          _ s h _ =
    case val (h!!last s) h of
        APP x _ -> s ++ [x]
        _       -> s
stackf Call            p s h _ =        -- passt wir habens 100 mal getestet
    case val (h!!last s) h of
        DEF {}  -> s ++ [p+1]
        PRE _ _ -> s ++ [p+1]
        _       -> s
stackf Return          _ s _ _ = init (init s) ++ [last s]
stackf (Pushpre op)    _ s h _ = s ++ [length h]
stackf Updateop        _ s h _ = init (init $ init s) ++ [s!!(length s - 2), s!!(length s - 3)] -- Diskrepanz zwischen Zhus NOtiz auf Seite 84 und Auswertung!) unseres ist nach Auswertung (heap[stack[T]] = last heap)
stackf (Operator op)   _ s h _ =
    case op of
        UnaryOp  -> init (init $ init s) ++ [s!!(length s-2), length h]
        BinaryOp -> init (init $ init $ init $ init s) ++ [s!!(length s-3), length h]
        _        -> let a = val (h!!last s) h
                    in init (init $ init $ init $ init s) ++ [s!!(length s-2)] ++
                        case a of
                            VAL Bool 1 -> [add2arg(s!!(length s-5)) h]
                            VAL Bool _ -> [add2arg(s!!(length s-6)) h]
                            _          -> throw (NoValueFound (show op ++ show a))

                    --   if (\(VAL Bool v) -> v) a == 1
                    --   then [add2arg(s!!(length s-5)) h, s!!(length s-2)]
                    --   else [add2arg(s!!(length s-6)) h, s!!(length s-2)]
stackf Alloc           _ s h _ = s ++ [length h]
stackf (Updatelet _)   _ s h _ = init s
stackf (Slidelet arg)  _ s _ _ = slider (length s-arg-1) s 0 ++ [last s]
stackf _               _ s _ _ = s -- Reset, Updatefun

heapf :: Instruction -> [Int] -> [HeapCell] -> [HeapCell]
heapf (Pushval t v) _ h = h ++ [VAL t v]
heapf Makeapp       s h = h ++ [APP (last s)  (s!!(length s-2))]
heapf (Pushpre op)  _ h = h ++ [PRE op (arity op)]
heapf Updateop      s h = insert1 (s!!(length s-3)) 0 h ++ [h!!last s] ++ insert2 (s!!(length s-3)) h
heapf (Updatefun f) s h = insert1 (s!!(length s-f-3)) 0 h ++ [IND (last s)] ++ insert2 (s!!(length s-f-3)) h
heapf (Operator op) s h =
    case op of
        UnaryOp  -> let a = val (h!!(s!!(length s-3))) h
                        b = val (h!!last s) h
                    in 
                        case a of
                            PRE op _ ->
                                case b of
                                    VAL t v -> h ++ [VAL t (compute op v 0)]
                                    _ -> throw (NoValueFound (show b ++ show s ++ "1"))
                            _        -> throw (NoValueFound (show a ++ show s ++ "2"))

                        -- h ++ [VAL ((\(VAL t _) -> t) b) ((\(VAL _ v) (PRE op _) -> compute op v 0) b a)]
        BinaryOp -> let a = val (h!!(s!!(length s-4))) h
                        b = val (h!!(s!!(length s-2))) h
                        c = val (h!!last s) h
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
                        -- h ++ [VAL (findType ((\(PRE op _) -> op) a)) ((\(VAL _ v1) (VAL _ v2) (PRE op _) -> compute op v1 v2) b c a)]
        _        -> h   -- stimmt das?
heapf Alloc         _ h = h ++ [UNINITIALIZED]
heapf (Updatelet n) s h = insert1 (add2arg (s!!(length s-n-2)) h) 0 h ++ [IND (last s)] ++ insert2 (add2arg (s!!(length s-n-2)) h) h
heapf _             _ h = h -- Pushfun, Reset, Pushparam, Slide, Unwind, Call, Return

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

slider :: Int -> [Int] -> Int -> [Int]
slider n s akk | n > 0     = s!!akk : slider (n-1) s (akk+1)
               | otherwise = []

findType :: Token -> Type
findType Plus  = Float
findType Times = Float
findType _     = Bool

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

---------- test examples

ex2 = posifyer [Variable "x", Variable "y"]
ex3 = pos (Variable "x") ex2
ex4 = pos (Variable "q") ex2
ex5 = compileProgram [Definition [Variable "main"] (Function (Variable "f") (Val 5)),Definition [Variable "f",Variable "x"] (Mult (Val 3) (Variable "x"))]
skriptex = compileProgram [Definition [Variable "main"] (Function (Variable "quadrat") (Function (Variable "quadrat") (Mult (Val 3) (Val 1)))),Definition [Variable "quadrat",Variable "x"] (Mult (Variable "x") (Variable "x"))]
ex6 = compileProgram [Definition [Variable "main"] (Function (Variable "f") (Val 5)),Definition [Variable "f",Variable "x"] (Sum (Val 3) (Variable "x"))]
ex7 = compileProgram [Definition [Variable "main"] (Function (Variable "f") (Val 5)),Definition [Variable "f",Variable "x"] (IfX (LessThanX (Variable "x") (Val 3)) (BoolVal True) (BoolVal False))]
ex8 = compileProgram [Definition [Variable "main"] (Function (Variable "f") (Val 5)),Definition [Variable "f",Variable "x"] (LetX (LocDefs [LocDef (Variable "y") (Val 2)]) (Variable "x"))] -- klappt
ex9 = compileProgram [Definition [Variable "main"] (Function (Variable "f") (Val 5)),Definition [Variable "f",Variable "x"] (LetX (LocDefs [LocDef (Variable "y") (Val 2)]) (Sum (Variable "x") (Variable "y")))]
ex10 = compileProgram [Definition [Variable "main"] (Function (Variable "f") (Val 5)),Definition [Variable "f",Variable "x"] (LetX (LocDefs [LocDef (Variable "y") (Val 2),LocDef (Variable "z") (Val 3)]) (Variable "y"))]

ex11 = compile "main = f 5; f x = 2 + x;" -- passt
ex12 = compile "main = f 5; f x = 2 * x;" -- passt
ex13 = compile "main = f 5; f x = 2 - x;" -- passt
ex14 = compile "main = f 5; f x = 2 / x;" -- passt
ex15 = compile "main = f 5; f x = 2 * x + 1;" -- passt
ex16 = compile "main = f 5; f x = 2 / x - 1;" -- passt
ex17 = compile "main = f 5; f x = 2 < 1;" -- passt
ex18 = compile "main = f 5; f x = 2 == 1;" -- passt
ex19 = compile "main = f 5; f x = 2 | 1;" -- passt
ex20 = compile "main = 2 & 1;" -- passt
ex21 = compile "main = let x = 1; y = z; z = 2 in x + y;" -- passt
ex22 = compile "main = let v1 = v2; v2 = 9 in v1;" --passt
ex23 = compile "main = let x = y; y = z; z= 1/5 in x;" -- passt
ex24 = compile "main = let y = x*x; x = 3 in y + 1;" -- passt


-- teste Anzahl Definitionen
e1 = compile "f1 = 1;"
e2 = compile "f1 = 1; f2 = 2;"
e3 = compile "f1 = 1; f2 = 2; f3 = 3;"

-- teste Anzahl Funktionsparameter
e4 = compile "f x = 1;"
e5 = compile "f x y z w e r = 1;"

-- teste Inhalt Expressions
e6 = compile "f x = True;"
e7 = compile "f x = cool;"

-- teste error cases
e8 = compile "f;"
e9 = compile "f ="
e10 = compile "f = 3"
e11 = compile "f = x;"
e12 = compile "bool x = ((x == True) | (x == False)); f x = if bool x | x < 1 then 1 else x * f (x - 1); main = f 6;"
e13 = compile "f x = if bool x | x < 1 then 1 else x * f (x - 1);"
e14 = compile "f x = if True then 1 else x * f (x - 1);"
e15 = compile "f x = f x;"
e16 = compile "f x y = f x y;"
e17 = compile "main = quadrat (quadrat (3 * 1)); quadrat x = x * x;"
e18 = compile "main = f 3; f x = let y = 5, z = False in if (y < x) == z then x / (y / 3) else f (x - 1); f = 1;"
e21 = emulate "main = 1;" -- passt! <3
e22 = run (State 0 ([Reset, Pushfun "main", Call, Halt]
                 ++ [Pushparam 1, Unwind, Call, Pushparam 3, Unwind, Call, Operator BinaryOp, Updateop, Return]
                 ++ [Pushparam 1, Unwind, Call, Operator IfOp, Updateop, Unwind, Call, Return]
                 ++ [Pushparam 1, Unwind, Call, Operator UnaryOp, Updateop, Return]
                 ++ [Pushfun "x", Updatefun 0, Slide 1, Unwind, Call, Return]) [] [DEF "f" 0 27] [("f", 0)]) -- emulate "f = x;" ausgeschrieben
e23 = emulate "main = 1+2;"
e25 = run (State 0 (startList ++ [Call, Halt]) [] [DEF "main" 0 27] [("main", 0)])
-- call PRE funktioniert, aber danach gitb es ein Index Problem