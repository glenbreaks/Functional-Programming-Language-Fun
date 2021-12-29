data Instruction = Instruction
    = Reset
    | Pushfun String
    | Pushval Type Value
    | Pushparam Int
    | Makeapp
    | Slide Int
    | Reduce
    | Return
    | Halt
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
        -- stack  :: [Instruction],     -- Stack?
        -- heap   :: [HeapCell],        -- Heap?
        -- global :: String -> HeapCell -- Global?
        stack  :: Stack, -- [Int]
        heap   :: Heap,
        global :: Global
    }

newtype Stack  = [Int] -- speichert Adressen von auszuwertenden Ausdrücken (heap)
newtype Heap   = [HeapCell]
newtype Global = (String, Int)

data HeapCell 
    = APP HeapCell HeapCell -- Konstruktor für Knoten
    | DEF String Int Int    -- DEF f N Code-Adr, f = Funktion, N = Stelligkeit
    | VAL Type Value        -- Blätter

type Type  = Int    -- 1 = Bool, 0 = Zahl
type Value = Int    -- 0 = False, 1 = True oder jede andere Zahl falls Type 0


compileProgram :: [Definition] -> State
-- compileProgram (x:xs) = compileDef x & compileProgram xs
-- compileProgram [] = []

compileDef :: [Expression] Expression -> State

compileLocDefs :: [LocDef] -> State

compileLocDef :: Expression Expression -> State

compileExpr :: Expression -> State

compileOrExpr :: Expression -> State

compileAndExpr :: Expression -> State

compileNotExpr :: Expression -> State

compileCompareExpr :: Expression -> State

compileAddExpr :: Expression -> State

compileMultExpr :: Expression -> State

compileNegExpr :: Expression -> State

compileAtomicExpr :: Expression -> State

compileVariable :: Expression -> State