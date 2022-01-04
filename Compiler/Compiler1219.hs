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
    = APP HeapCell HeapCell -- Konstruktor für Knoten
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

compileProgram :: [Definition] -> State
-- compileProgram (x:xs) = compileDef x & compileProgram xs
-- compileProgram [] = []
compileProgram xs = 0 ([Reset, Pushfun main, Call, Halt] ++ cProg xs) [] [] []
    where
        cProg (x:xs) = compileDef x : cProg xs
        cProg []     = []

compileDef :: [Expression] Expression -> [Instruction]
compileDef (f:xs) y = 
    -- let n   = length xs
-- code = code ++ L:ÜbDef(Expr, posifyer xs, n)
-- heap = heap ++ DEF f n L
-- global = global ++ (f, addr DEF f)

posifyer :: [Expression] -> [(Expression,Int)]
posifyer xs = pos xs 1
    where 
        pos [] _ = []
        pos (x:xs) akk = (x, akk) : pos xs (akk + 1)

compileLocDefs :: [LocDef] -> [Instruction]

compileLocDef :: Expression Expression -> [Instruction]

compileExpr :: Expression -> [Instruction]

-- compileOrExpr :: Expression -> State

-- compileAndExpr :: Expression -> State

-- compileNotExpr :: Expression -> State

-- compileCompareExpr :: Expression -> State

-- compileAddExpr :: Expression -> State

-- compileMultExpr :: Expression -> State

-- compileNegExpr :: Expression -> State

-- compileAtomicExpr :: Expression -> State

-- compileVariable :: Expression -> State
