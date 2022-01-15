import Data.Char(isDigit, isAlpha, isAlphaNum)
import Control.Exception (Exception, throw)

data CompilerException 
    = InvalidName !String
    | MissingMain
    | TypeCheck !String
    | VariableNotInScope !String
    -- | Undefined
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
    -- | Update Arg
    | Updatefun Int
    | Updateop
    | Operator Op
    | Alloc -- ab hier extra für Let, siehe Zhus Skript S. 25
    | Updatelet Int
    | Slidelet Int
    deriving Show

data Type  = Int | Bool deriving Show  -- 1 = Bool, 0 = Zahl
type Value = Int    -- 0 = False, 1 = True oder jede andere Zahl falls Type 0

data State = State
    {
        pc     :: Int,
        code   :: [Instruction],
        stack  :: Stack,
        heap   :: Heap,
        global :: Global
    } -- deriving Show

instance Show State
    where show (State _ code _ _ _)= showCode code
            where
                showCode (x:xs) = show x ++ "\n" ++ showCode xs
                showCode []     = []


type Stack  = [Int] -- speichert Adressen von auszuwertenden Ausdrücken (heap)
type Heap   = [HeapCell]
type Global = [(String, Int)]

data HeapCell 
    = APP Int Int           -- Konstruktor für Knoten
    | DEF String Int Int    -- DEF f N Code-Adr, f = Funktion, N = Stelligkeit
    | VAL Type Value        -- Blätter
    | IND Int               -- HeapAdress
    | PRE Op
    deriving Show

data Op
    = UnaryOp
    | BinaryOp
    | IfOp
    deriving Show

----------------------------------------------------------------------------------------------------------------------------------------------

tokenize xs = tokenizer $ words $ spaceyfier xs

spaceyfier :: String -> String
spaceyfier x =
   case x of
       ';' : xs       -> " ;"   ++ spaceyfier xs        -- hier kein Leerzeichen danach, da Strichpunkte in Namen eh nicht erlaubt sind!
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
show2 (Definition (Variable a : _)_) = a

program :: Parser Program
program xs1 = do
    (e, xs2)  <- def xs1
    case xs2 of
        Semicolon : xs3 -> do
            (es, xs4) <- restProgram xs3
            case xs4 of
                []    -> return (Program (e:es), xs4)
                (x:_) -> Left ("Parse error on input: " ++ show x)   -- immer wenn die Restliste nicht leer ist (wenn Code nicht vollständig geparst werden konne) -> Nothing 
        x               -> Left ("Parser error on input: " ++ show x)

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
restDef (x:_)          = Left ("Expected: '=', Actual: " ++ show x)
restDef []             = Left "Definition incomplete"

locDefs :: Parser LocDefs
locDefs xs1 = do
    (e, xs2)  <- locDef xs1
    (es, xs3) <- restLocDefs xs2
    return (LocDefs (e:es), xs3)

restLocDefs :: Parser [LocDef]
restLocDefs (Semicolon : xs1) = do
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
        (x:_)        -> Left ("Expected: '=', Actual: " ++ show x)
        []           -> Left "Local definition incomplete"

----------------------------------------------------------------------------------------------------------------------------------------------

expr :: Parser Expression
expr (Let : xs1) = do
    (e, xs2)  <- locDefs xs1
    case xs2 of
        In : xs3 -> do
            (es, xs4) <- expr xs3
            return (LetX e es, xs4)
        (x:_)    -> Left ("Expected: 'in', Actual: " ++ show x)
        []       -> Left "Expected 'in' after local definition"
expr (If : xs1)  = do
    (e1, xs2) <- expr xs1
    case xs2 of
        Then : xs3 -> do
            (e2, xs4) <- expr xs3
            case xs4 of
                Else : xs5 -> do
                    (es, xs6) <- expr xs5
                    return (IfX e1 e2 es, xs6)
                (x:_)      -> Left ("Expected: 'else', Actual: " ++ show x)
                []         -> Left "Expected 'else' after 'then' block"
        (x:_)      -> Left ("Expected: 'then', Actual: " ++ show x)
        []         -> Left "Expected 'then' after 'if' block"
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
atomicExpr (Number i : xs1)  = do
    (is, xs2) <- restAtomicExpr xs1
    return (foldl Function (Val i) is, xs2)
atomicExpr (Boolean i : xs1) = do
    (is, xs2) <- restAtomicExpr xs1
    return (foldl Function (BoolVal i) is, xs2)
atomicExpr (OpenPar : xs1)   = do
    (e, xs2)  <- expr xs1
    case xs2 of
        ClosePar : xs3 -> do
            (es, xs4) <- restAtomicExpr xs3
            return (foldl Function e es, xs4)
        (x:_)          -> Left ("Expected: ')', Actual: " ++ show x)
        []             -> Left "Missing ')'"
atomicExpr (Name i : xs1)    = do
    (is, xs2) <- restAtomicExpr xs1
    return (foldl Function (Variable i) is, xs2)
atomicExpr (x:_)             = Left ("Expected: number, boolean or variable, Actual: " ++ show x)
atomicExpr []                = Left "Expected: number, boolean or variable"

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
        (x:_)          -> Left ("Expected: ')', Actual: " ++ show x)
        []             -> Left "Missing ')'"
restAtomicExpr (Name i : xs1)    = do
    (is, xs2) <- restAtomicExpr xs1
    return (Variable i:is, xs2)
restAtomicExpr xs                = return ([], xs)

variable :: Parser Expression
variable (Name i : xs) = return (Variable i, xs)
variable (x:_)         = Left ("Expected: variable, Actual: " ++ show x)
variable []            = Left "Expected: variable"

-- data Operators -- using this instead of token in Pushpre Token?
--     = If
--     | Or
--     | And
--     | LessThan
--     | Is
--     | Plus
--     | Minus
--     | Times
--     | DivBy
--     | Not
--     deriving (Eq, Show)

posifyer :: [Expression] -> [(Expression, Int)]
posifyer xs = pos xs 1
    where 
        pos [] _ = []
        pos (x:xs) akk = (x, akk) : pos xs (akk + 1)

pos :: Expression -> [(Expression, Int)] -> Int
pos (Variable s) []           = throw (VariableNotInScope s)
pos s ((x, i):xs) | s == x    = i
                  | otherwise = pos s xs

---------- compileFunktionen:

compileProgram :: [Definition] -> State
compileProgram (x:xs) = State 0 (compileMain x ++ cProg xs) [] [] []
    where             --pc---------code-----------------stack-heap-global
        cProg (x:xs) = compileDef x ++ cProg xs
        cProg []     = []
-- build HeapCells here or in emulator?

compileMain :: Definition -> [Instruction]
compileMain (Definition [Variable "main"] body) = [Reset, Pushfun "main", Call, Halt, Pushparam 1, Unwind, Call, Pushparam 3, Unwind, Call, Operator BinaryOp, Updateop, Return, --BinärOp
                                                            Pushparam 1, Unwind, Call, Operator IfOp, Updateop, Unwind, Call, Return,                                               --If
                                                            Pushparam 1, Unwind, Call, Operator UnaryOp, Updateop, Return]                                                               --UnärOp
                                                            ++ compileExpr body [] 0 ++ [Updatefun 0, Slide 1, Unwind, Call, Return]
compileMain _ = throw MissingMain

compileDef :: Definition -> [Instruction]
compileDef (Definition (fun:args) body) = compileExpr body (posifyer args) 0 ++ [Updatefun n, Slide (n + 1), Unwind, Call, Return]
    where
        n = length args   

-- getOp :: Expression -> Token
-- getOp x =
--     case x of
--         LessThanX a b   -> LessThan
--         IsX       a b   -> Is
--         Sum       a b   -> Plus
--         Mult      a b   -> Times
--         NotX      a     -> Not
--         Neg       a     -> Minus
--         NegExpo   a     -> DivBy

compileExpr :: Expression -> [(Expression, Int)] -> Int -> [Instruction]
compileExpr (LetX      (LocDefs a) b)   env i = compileLocDefs a envLet (i-1) ++ compileExpr b envLet (i-1) ++ [Slidelet n]
    where                                                               -- hier i? checken durch lokaldefinitionen mit parametern!
        n = length a -- Anzahl der Lokaldefinitionen
        envLet = posifyerLet a

compileExpr (IfX                a  b c) env i = compileExpr c env i ++ compileExpr b env (i+1) ++ compileExpr a env 2 ++ [Pushpre If, Makeapp, Makeapp, Makeapp]

compileExpr (NotX               a)      env i = compileExpr a env i ++ [Pushpre Not, Makeapp]
compileExpr (Neg                a)      env i = compileExpr a env i ++ [Pushpre Minus, Makeapp]
compileExpr (NegExpo            a)      env i = compileExpr a env i ++ [Pushpre DivBy, Makeapp] -- Token DivBy hier 1/x da kein NegExpo Token existiert

-- compileExpr (OrX                a  b)   env i = compileExpr b env i ++ compileExpr a env (i+1) ++ [Pushpre Or, Makeapp, Makeapp]
compileExpr (OrX                a  b)   env i = compileExpr (IfX a (BoolVal True) b)  env i
compileExpr (AndX               a  b)   env i = compileExpr (IfX a b (BoolVal False)) env i 

compileExpr (LessThanX          a  b)   env i = compileExpr b env i ++ compileExpr a  env (i+1) ++ [Pushpre LessThan, Makeapp, Makeapp]
compileExpr (IsX                a  b)   env i = compileExpr b env i ++ compileExpr a  env (i+1) ++ [Pushpre Is, Makeapp, Makeapp]
compileExpr (Sum                a  b)   env i = compileExpr b env i ++ compileExpr a  env (i+1) ++ [Pushpre Plus, Makeapp, Makeapp]
compileExpr (Mult               a  b)   env i = compileExpr b env i ++ compileExpr a  env (i+1) ++ [Pushpre Times, Makeapp, Makeapp]

compileExpr (Function (Variable a) b)   env i = compileExpr b env i ++ [Pushfun a, Makeapp]
compileExpr (Val                a)      env i = [Pushval Int a]
compileExpr (BoolVal            a)      env i = [Pushval Bool x] -- x ist 0 oder 1  
    where x | a         = 1
            | not a     = 0
compileExpr (Variable           a)      env i = [Pushparam (pos (Variable a) env + i)]

compileLocDefs :: [LocDef] -> [(Expression, Int)] -> Int -> [Instruction]
compileLocDefs x env i = alloc n ++ cLocDefs x env i n
    where
        n = length x -- Anzahl der Lokaldefinitionen
        alloc 0 = []
        alloc x = [Alloc, Alloc, Makeapp] ++ alloc (x-1)
        cLocDefs ((LocDef var expr):xs) env i n = compileExpr expr env i ++ [Updatelet (n-1)] ++ cLocDefs xs env i (n-1)
        cLocDefs []                     _   _ _ = []

posifyerLet :: [LocDef] -> [(Expression, Int)]
posifyerLet xs = pos xs n
    where 
        n = length xs
        pos ((LocDef var _):xs) n = (var, n-1) : pos xs (n-1)
        pos []                  n = []

---------- combining functions
parse xs = program $ tokenize xs
compile2 (Right (Program xs, [])) = compileProgram xs
compile xs = compile2 $ parse xs

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