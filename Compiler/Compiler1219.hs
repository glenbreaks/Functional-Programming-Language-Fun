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

data Expression
    = LetX      LocDefs Expression
    | IfX       Expression Expression Expression
    | OrX       Expression Expression
    | AndX      Expression Expression
    | NotX      Expression
    | LessThanX Expression Expression
    | IsX       Expression Expression
    | Diff      Expression Expression
    | Sum       Expression Expression
    | Div       Expression Expression
    | Mult      Expression Expression
    | Neg       Expression
    | Function  Expression Expression
    | Val       Integer
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

data Type  = Num | Bool deriving Show  -- 1 = Bool, 0 = Zahl
type Value = Integer    -- 0 = False, 1 = True oder jede andere Zahl falls Type 0

data State = State
    {
        pc     :: Int,
        code   :: [Instruction],
        stack  :: Stack,
        heap   :: Heap,
        global :: Global
    } -- deriving Show

instance Show State where show (State _ code _ _ _)= showCode code
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

-- data Arg --bei zwei Update Instructions nicht mehr nötig
--     = Op 
--     | N Int
--     deriving Show

data Op
    = UnaryOp
    | BinaryOp
    | IfOp
    deriving Show

-- data Operators --stattdessen einfach token wiederverwenden?
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
--     deriving (Eq, Show) -- Token?

-- Nebenfunktionen:

-- arity :: Expression -> Op 
-- arity (NotX x)    = UnaryOp 
-- arity (Neg x)     = UnaryOp 
-- arity (IfX x y z) = IfOp
-- arity  _          = BinaryOp
 
posifyer :: [Expression] -> [(Expression, Int)]
posifyer xs = pos xs 1
    where 
        pos [] _ = []
        pos (x:xs) akk = (x, akk) : pos xs (akk + 1)

-- pos :: Expression -> [(Expression, Int)] -> Int -- liefert i nur, wenn x im ersten Tupel ist
-- pos s ((x, i):_) = 
--     if s == x then i else 0

pos :: Expression -> [(Expression, Int)] -> Int -- durchsucht die ganze Liste nach x
pos (Variable s) []           = 0 --throw (VariableNotInScope s)
pos s ((x, i):xs) | s == x    = i
                  | otherwise = pos s xs
---------- compileFunktionen:

compileProgram :: [Definition] -> State
compileProgram (x:xs) = State 0 (compileMain x ++ cProg xs) [] [] []
    where             --pc---------code-----------stack-heap-global
        cProg (x:xs) = compileDef x ++ cProg xs
        cProg []     = []
-- HeapCells hier aufbauen ?

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

-- let n   = length args     -- Stelligkeit von fun
-- let a   = length args + 1 -- Länge des Anwendungsgraphen
-- code = code ++ L:ÜbDef(Expr, posifyer xs, n)
-- heap = heap ++ DEF f n L
-- global = global ++ (f, addr DEF f)

-- compileLocDef :: Expression -> Expression -> [Instruction]
-- compileFun :: [Expression] -> [Instruction]
-- compileFun (Variable f:args) = Pushfun f : paramizer (posifyer args)
--     where
--         paramizer ((x, y):xs) = Pushparam y : paramizer xs
--         paramizer []          = []
--         -- paramizer packt die Parameter aus der liste pos wieder aus und in die Instruction Pushparam


compileExpr :: Expression -> [(Expression, Int)] -> Int -> [Instruction]
-- compileExpr x ((u,v):xs) = 
    -- case arity x of 
    --     UnaryOp  -> [Pushparam 1, Unwind, Call, Operator UnaryOp, Updateop, Return]
    --     BinaryOp -> [Pushparam 1, Unwind, Call, Pushparam 3, Unwind, Call, Operator BinaryOp, Updateop, Return]
    --     IfOp     -> [Pushparam 1, Unwind, Call, Operator IfOp, Updateop, Return]

-- Idee: nur 3 comileExpr nach arity geordnet, patternmatching mit
-- compileExpr (Expression a b) env i = ... ++ [Pushpre getOp Expression, ...]
-- getOp :: Expression -> Token

compileExpr (LetX      (LocDefs a) b)   env i = compileLocDefs a env i ++ compileExpr b env (i+n) ++ [Slidelet n]
    where n = length a -- Anzahl der Lokaldefinitionen

compileExpr (IfX                a  b c) env i = compileExpr c env i ++ compileExpr b env (i+1) ++ compileExpr a env 2 ++ [Pushpre If, Makeapp, Makeapp, Makeapp]

compileExpr (NotX               a)      env i = compileExpr a env i ++ [Pushpre Not]
compileExpr (Neg                a)      env i = compileExpr a env i ++ [Pushpre Minus]

compileExpr (OrX                a  b)   env i = compileExpr b env i ++ compileExpr a env (i+1) ++ [Pushpre Or, Makeapp, Makeapp]
compileExpr (AndX               a  b)   env i = compileExpr b env i ++ compileExpr a env (i+1) ++ [Pushpre And, Makeapp, Makeapp]
compileExpr (LessThanX          a  b)   env i = compileExpr b env i ++ compileExpr a env (i+1) ++ [Pushpre LessThan, Makeapp, Makeapp]
compileExpr (IsX                a  b)   env i = compileExpr b env i ++ compileExpr a env (i+1) ++ [Pushpre Is, Makeapp, Makeapp]
compileExpr (Sum                a  b)   env i = compileExpr b env i ++ compileExpr a env (i+1) ++ [Pushpre Plus, Makeapp, Makeapp]
compileExpr (Mult               a  b)   env i = compileExpr b env i ++ compileExpr a env (i+1) ++ [Pushpre Times, Makeapp, Makeapp]

compileExpr (Function (Variable a) b)   env i = compileExpr b env i ++ [Pushfun a, Makeapp]
compileExpr (Val                a)      env i = [Pushval Num a]
compileExpr (BoolVal            a)      env i = [Pushval Bool x] -- x ist 0 oder 1  
    where x | a         = 1
            | not a     = 0
            | otherwise = throw (TypeCheck ("Expected: Bool, Actual: " ++ show a))
compileExpr (Variable           a)      env i = [Pushparam (pos (Variable a) env + i)]

compileLocDefs :: [LocDef] -> [(Expression, Int)] -> Int -> [Instruction]
compileLocDefs x env i = alloc n ++ cLocDefs x env i n
    where
        n = length x -- Anzahl der Lokaldefinitionen
        alloc 0 = []
        alloc x = [Alloc, Alloc, Makeapp] ++ alloc (x-1)
        cLocDefs ((LocDef var expr):xs) env i n = compileExpr expr env (i+n) ++ [Updatelet (n-1)] ++ cLocDefs xs env i (n-1)
        cLocDefs []                     _   _ _ = []

-- compileLocDef :: LocDef -> [(Expression, Int)] -> Int -> [Instruction]
-- compileLocDef (LocDef(Variable a) expr) env i = [Alloc, Alloc, Makeapp] ++ compileExpr expr env i ++ [Alloc, Alloc, Makeapp]

----------

-- ex1 = compileFun [Variable "x", Variable "y"]
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