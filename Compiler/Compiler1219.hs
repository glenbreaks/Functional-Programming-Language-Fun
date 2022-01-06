data Instruction -- = Instruction
    = Reset
    | Pushfun String
    | Pushval Type Value
    | Pushparam Int
    | Makeapp
    | Slide Int
    -- | Reduce
    | Return
    | Halt
    | Unwind
    | Call
    | Return
    | Pushpre Op -- Op definieren
    | Update Arg
    | Operator Op
    -- {
    --     address :: String        -> Int
    --     add2arg :: HeapCell      -> HeapCell
    --     new     :: Knotentyp A B -> HeapCell
    --     type    :: HeapCell      -> HeapCell
    -- }
    -- | ...

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
    deriving Show

-- data Stack = Stack []
-- data Heap = APP HeapCell HeapCell
-- data Global
-- data State = State (Int, [Instruction], Stack, Heap, Global)

data State = State
    {
        pc     :: Int,
        code   :: [Instruction],
        stack  :: Stack,
        heap   :: Heap,
        global :: Global
    }

type Stack  = [Int] -- speichert Adressen von auszuwertenden Ausdrücken (heap)
type Heap   = [HeapCell]
type Global = [(String, Int)]

data HeapCell 
    = APP Int Int           -- Konstruktor für Knoten
    | DEF String Int Int    -- DEF f N Code-Adr, f = Funktion, N = Stelligkeit
    | VAL Type Value        -- Blätter
    | IND Int               -- HeapAdress
    | PRE Op

type Type  = Int    -- 1 = Bool, 0 = Zahl
type Value = Int    -- 0 = False, 1 = True oder jede andere Zahl falls Type 0

data Op
    = If
    | Or
    | And
    | LessThan
    | Is
    | Plus
    | Minus
    | Times
    | DivBy
    | Not
    deriving (Eq, Show) -- Token?

posifyer :: [Expression] -> [(Expression,Int)]
posifyer xs = pos xs 1
    where 
        pos [] _ = []
        pos (x:xs) akk = (x, akk) : pos xs (akk + 1)

compileProgram :: [Definition] -> State
-- compileProgram (x:xs) = compileDef x & compileProgram xs
-- compileProgram [] = []
compileProgram xs = 0 ([Reset, Pushfun main, Call, Halt] ++ cProg xs) [] [] []
    where
        cProg (x:xs) = compileDef x : cProg xs
        cProg []     = []

compileDef :: Definition -> [Instruction]
compileDef (Definition (fun:args) body) = compileExpr body (posifyer args) ++ [Slide 1, Reduce, Return]
-- let n   = length xs
-- code = code ++ L:ÜbDef(Expr, posifyer xs, n)
-- heap = heap ++ DEF f n L
-- global = global ++ (f, addr DEF f)

-- compileLocDef :: Expression -> Expression -> [Instruction]

compileExpr :: Expression -> [(Expression, Int)] -> [Instruction]
compileExpr 
compileExpr (LetX      a b)   ((u,v):xs) = ... compareExpr u xs   compileLocDefs 
compileExpr (IfX       a b c) env = 
compileExpr (OrX       a b)   env = compileExpr a 
compileExpr (AndX      a b)   env = 
compileExpr (NotX      a)     env = 
compileExpr (LessThanX a b)   env = 
compileExpr (IsX       a b)   env = 
compileExpr (Diff      a b)   env = 
compileExpr (Sum       a b)   env = 
compileExpr (Div       a b)   env = 
compileExpr (Mult      a b)   env = 
compileExpr (Neg       a)     env = 
compileExpr (Function  a b)   env = Pushfun a
compileExpr (Val       a)     env = Pushval Num a
compileExpr (BoolVal   a)     env = Pushval Bool w -- w ist 0 oder 1  
    -- if a == False then Pushval Bool 0 else Pushval Bool 1
compileExpr (Variable  a)     env = Pushparam (snd env)

compileLocDefs :: [LocDef] -> [Instruction]
compileLocDefs 

-- compileOrExpr :: Expression -> State

-- compileAndExpr :: Expression -> State

-- compileNotExpr :: Expression -> State

-- compileCompareExpr :: Expression -> State

-- compileAddExpr :: Expression -> State

-- compileMultExpr :: Expression -> State

-- compileNegExpr :: Expression -> State

-- compileAtomicExpr :: Expression -> State

-- compileVariable :: Expression -> State
