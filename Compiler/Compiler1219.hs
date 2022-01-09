data Instruction
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
    deriving Show
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
    deriving Show

type Type  = Int    -- 1 = Bool, 0 = Zahl
type Value = Int    -- 0 = False, 1 = True oder jede andere Zahl falls Type 0

data Arg 
    = Op 
    | N Int
    deriving Show

data Op
    = "1"
    | "2"
    | "if"
    deriving Show

-- data Op
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

arity :: Expression -> Op 
arity NotX  = "1"
arity NegX  = "1" 
arity IfX   = "if"
arity  _    = "2"
 
posifyer :: [Expression] -> [(Expression,Int)]
posifyer xs = pos xs 1
    where 
        pos [] _ = []
        pos (x:xs) akk = (x, akk) : pos xs (akk + 1)

compileMain :: Definition -> [Instruction]
compileMain (Definition [Variable "main"] body) = [Reset, Pushfun "main", Call, Halt, Pushparam 1, Unwind, Call, Pushparam 3, Unwind, Call, Operator 2, Update op, Return, --BinärOp
                                                            Pushparam 1, Unwind, Call, Operator "if", Update op, Unwind, Call, Return                                               --If
                                                            Pushparam 1, Unwind, Call, Operator 1, Update op, Return]                                                               --UnärOp
                                                            ++ compileExpr body [] ++ [Slide 1, Unwind, Call, Return]
compileMain _ = []
-- Exception throwen?

compileProgram :: [Definition] -> State
compileProgram x:xs = 0 compileMain x ++ cProg xs [] [] []
    where
        cProg (x:xs) = compileDef x : cProg xs
        cProg []     = []
-- HeapCells hier aufbauen ?

compileDef :: Definition -> [Instruction]
compileDef (Definition (fun:args) body) = (Pushfun fun) : paramizer (posifyer args) ++ compileExpr body (posifyer args) ++ [Slide ((length args) + 1), Unwind, Call, Return]
    where   
        paramizer (x, y):xs = (Pushparam y) : paramizer xs
        paramizer []        = []

-- let n   = length args     -- Stelligkeit von fun
-- let a   = length args + 1 -- Länge des Anwendungsgraphen
-- code = code ++ L:ÜbDef(Expr, posifyer xs, n)
-- heap = heap ++ DEF f n L
-- global = global ++ (f, addr DEF f)

-- compileLocDef :: Expression -> Expression -> [Instruction]

compileExpr :: Expression -> [(Expression, Int)] -> [Instruction]
compileExpr 
compileExpr (LetX      a b)   ((u,v):xs) = ... compileExpr u xs   compileLocDefs 
compileExpr x ((u,v):xs) = 
    case arity x of 
        "1"  -> [Pushparam 1, Unwind, Call, Operator 1, Update 1, Return]
        "2"  -> [Pushparam 1, Unwind, Call, Pushparam 3, Unwind, Call, Operator 2, Update 2, Return]
        "if" -> 
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
compileExpr (Variable  a)     ((u,v):xs) = Pushparam v

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
