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

