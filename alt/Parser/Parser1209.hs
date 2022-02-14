module Main where
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

data Program = Program [Definition]

data Definition = Definition String [String]  

data Expression
    = Val       Integer -- returntyp für Number
    | ValBool   Bool    -- returntyp für Boolean
    | Word      String  -- returntyp für Variable
    | OrX       Expression Expression   -- Expressions spezifizieren?
    | AndX      Expression Expression
    | NotX      Expression
    | NeutralX  Expression
    | LessThanX Expression Expression
    | IsX       Expression Expression
    | NegX      Expression
    | Diff      Expression Expression
    | Sum       Expression Expression
    | DivX      Expression Expression
    | Mult      Expression Expression
    | LetX      Expression Expression
    | IfX       Expression Expression Expression
    | Programs  Expression Expression -- haben jetzt eigene types
    | Def       Expression Expression
    | LocDefs   Expression Expression
    | LocDef    Expression Expression
    | Atomics   Expression Expression
    deriving Show

type Parser a = [Token] -> Maybe (a, [Token])

program :: Parser Expression
program xs1 = do
    (e, xs2)  <- def xs1
    case xs2 of
        Semicolon : xs3 -> do
            (es, xs4) <- restProgram xs3
            return (foldl Programs e es, xs4) -- fold??
        _               -> Nothing

restProgram :: Parser [Expression]
restProgram (Name i : xs1) = do
    case xs1 of
        Semicolon : xs2 -> do
            (es, xs3) <- restProgram xs2
            return (Word i:es, xs3)
        _               -> Nothing
restProgram xs             = return ([], xs)

def :: Parser Expression
def xs1 = do
    (e, xs2)  <- variable xs1
    (es, xs3) <- restDef xs2
    return (foldl Def e es, xs3) -- fold?

restDef :: Parser [Expression]
restDef (Name i : xs1) = do
    (es, xs2) <- restDef xs1
    return (Word i:es, xs2)
restDef (Equals : xs1) = do
    (e, xs2)  <- expr xs1
    return ([e], xs2)
restDef _              = Nothing

locDefs :: Parser Expression
locDefs xs1 = do
    (e, xs2)  <- locDef xs1
    (es, xs3) <- restLocDefs xs2
    return (foldl LocDefs e es, xs3) -- fold?

restLocDefs :: Parser [Expression]
restLocDefs (Semicolon : xs1) = do
    (e, xs2)  <- locDef xs1
    (es, xs3) <- restLocDefs xs2
    return (e:es, xs3)
restLocDefs xs                = return ([], xs)

locDef :: Parser Expression
locDef xs1 = do
    (e, xs2)  <- variable xs1
    case xs2 of
        Equals : xs3 -> do
            (es, xs4) <- expr xs3
            return (LocDef e es, xs4)
        _            -> Nothing

expr :: Parser Expression
expr (Let : xs1) = do
    (e, xs2)  <- locDefs xs1
    case xs2 of
        In : xs3 -> do
            (es, xs4) <- expr xs3
            return (LetX e es, xs4)
        _        -> Nothing
expr (If : xs1)  = do
    (e1, xs2) <- expr xs1
    case xs2 of
        Then : xs3 -> do
            (e2, xs4) <- expr xs3
            case xs4 of
                Else : xs5 -> do
                    (es, xs6) <- expr xs5
                    return (IfX e1 e2 es, xs6)
                _          -> Nothing
        _          -> Nothing
expr xs          = orExpr xs

orExpr :: Parser Expression
orExpr xs1 = do
    (e, xs2)  <- andExpr xs1
    (es, xs3) <- restOrExpr xs2
    return (foldr OrX e es, xs3)

restOrExpr :: Parser [Expression]
restOrExpr (Or : xs1) = do
    (e, xs2)  <- andExpr xs1    -- Or ausgeschrieben
    (es, xs3) <- restOrExpr xs2
    return (e:es, xs3)
restOrExpr xs         = return ([], xs)

andExpr :: Parser Expression
andExpr xs1 = do
    (e, xs2)  <- notExpr xs1
    (es, xs3) <- restAndExpr xs2
    return (foldr AndX e es, xs3)

restAndExpr :: Parser [Expression]
restAndExpr (And : xs1) = do
    (e, xs2)  <- notExpr xs1    -- And ausgeschrieben
    (es, xs3) <- restAndExpr xs2
    return (e:es, xs3)
restAndExpr xs          = return ([], xs)

-- notExpr :: Parser Expression
-- notExpr (Not : xs1) = do
--     (e, xs2)  <- compareExpr xs1
--     return (NotX e, xs2)
-- notExpr xs1         = do
--     (e, xs2)  <- compareExpr xs1
--     return (NeutralX e, xs2)

notExpr :: Parser Expression
notExpr (Not : xs1) = do
    (e, xs2)  <- compareExpr xs1
    return (NotX e, xs2)
notExpr xs = compareExpr xs

-- compareExpr :: Parser Expression
-- compareExpr xs1 = do
--     (e, xs2)  <- negExpr xs1
--     case xs2 of
--         LessThan : xs3 -> do
--             (es, xs4) <- negExpr xs3
--             return (LessThanX e es, xs4)
--         _              -> do
--             (es, xs3) <- restCompareExpr xs2
--             return (foldl IsX e es, xs3)

-- ruft jetzt addExpr auf statt negExpr
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
        _              -> addExpr xs1

-- alt:
-- restCompareExpr :: Parser [Expression]
-- restCompareExpr (Is : xs1)       = do
--     case xs1 of
--         Minus : xs2 ->  do
--             (e, xs2)  <- negExpr xs1
--             return ([e], xs2)
--         _           -> do
--             (e, xs2)  <- multExpr xs1    -- addExpr ausgeschrieben
--             (es, xs3) <- restAddExpr xs2 -- [e]?, e = addExpr
--             return (e:es, xs3)
-- restCompareExpr xs               = return ([], xs)

-- alt:
-- negExpr :: Parser Expression
-- negExpr (Minus : xs1) = do
--     (e, xs2)  <- addExpr xs1
--     return (NegX e, xs2)
-- negExpr xs            = addExpr xs

-- alt:
-- addExpr :: Parser Expression
-- addExpr xs1 = do
--     (e, xs2)  <- multExpr xs1
--     case xs2 of 
--         Minus : xs3 -> do
--             (es, xs4) <- multExpr xs3
--             return (Diff e es, xs4)
--         _           -> do
--             (es, xs3) <- restAddExpr xs2
--             return (foldl Sum e es, xs3)

-- wenn kein "_" oder "+" geparsed wird, wir jetzt eine MultExpr zurückgegeben
-- statt einer Sum mit leerem Rest
-- addExpr :: Parser Expression
-- addExpr xs1 = do
--     (e, xs2)  <- multExpr xs1
--     case xs2 of 
--         Minus : xs3 -> do
--             (es, xs4) <- multExpr xs3
--             return (Diff e es, xs4)
--         Plus : xs3  -> do
--             (es, xs3) <- restAddExpr xs2
--             return (foldl Sum e es, xs3)
--         _           -> multExpr xs1

-- Anpassung an neue Grammatik: Add kommt jetzt vor Neg
addExpr :: Parser Expression
addExpr xs1 = do
    (e, xs2)  <- negExpr xs1
    case xs2 of 
        Minus : xs3 -> do
            (es, xs4) <- multExpr xs3
            return (Diff e es, xs4)
        Plus : xs3  -> do
            (es, xs3) <- restAddExpr xs2
            return (foldl Sum e es, xs3)
        _           -> negExpr xs1

restAddExpr :: Parser [Expression] -- wird jetzt nur noch aufgrufen falls Plus kommt
restAddExpr (Plus : xs1)  = do
    (e, xs2)  <- multExpr xs1
    (es, xs3) <- restAddExpr xs2
    return (e:es, xs3)
restAddExpr xs            = return ([], xs)

-- Anpassung an neue Grammatik: neg hat jetzt höhere Präz.
negExpr :: Parser Expression
negExpr (Minus : xs1) = do
    (e, xs2)  <- multExpr xs1
    return (NegX e, xs2)
negExpr xs            = multExpr xs


-- alt:
-- multExpr :: Parser Expression
-- multExpr xs1 = do
--     (e, xs2)  <- atomicExpr xs1
--     case xs2 of 
--         Div : xs3 -> do
--             (es, xs4) <- atomicExpr xs3
--             return (DivX e es, xs4)
--         _         -> do
--             (es, xs3) <- restMultExpr xs2
--             return (foldl Mult e es, xs3)

-- neu: wenn kein "/" oder "*" geparsed wird wird kein Mult sondern
-- eine AtomicExpr übergeben!
-- multExpr :: Parser Expression
-- multExpr xs1 = do
--     (e, xs2)  <- atomicExpr xs1
--     case xs2 of 
--         Div : xs3   -> do
--             (es, xs4) <- atomicExpr xs3
--             return (DivX e es, xs4)
--         Times : xs3 -> do
--             (es, xs3) <- restMultExpr xs2
--             return (foldl Mult e es, xs3)
--         _           -> atomicExpr xs1

-- mit moreAtomicExpr
multExpr :: Parser Expression
multExpr xs1 = do
    (e, xs2)  <- moreAtomicExpr xs1
    case xs2 of 
        Div : xs3   -> do
            (es, xs4) <- moreAtomicExpr xs3
            return (DivX e es, xs4)
        Times : xs3 -> do
            (es, xs3) <- restMultExpr xs2
            return (foldl Mult e es, xs3)
        _           -> moreAtomicExpr xs1


-- restMultExpr :: Parser [Expression]
-- restMultExpr (Times : xs1) = do
--     (e, xs2)  <- atomicExpr xs1
--     (es, xs3) <- restMultExpr xs2
--     return (e:es, xs3)
-- restMultExpr xs            = return ([], xs)

-- mit moreAtomicExpr
restMultExpr :: Parser [Expression]
restMultExpr (Times : xs1) = do
    (e, xs2)  <- moreAtomicExpr xs1
    (es, xs3) <- restMultExpr xs2
    return (e:es, xs3)
restMultExpr xs            = return ([], xs)

moreAtomicExpr :: Parser Expression
moreAtomicExpr xs1 = do
    (e, xs2)  <- atomicExpr xs1
    (es, xs3) <- restAtomicExpr xs2
    return (foldl Atomics e es, xs3)

-- jetzt mehrmals hintereinander möglich
atomicExpr :: Parser Expression
atomicExpr (Number i : xs)  = return (Val i, xs)
atomicExpr (Boolean i : xs) = return (ValBool i, xs)
atomicExpr (OpenPar : xs1)  = do
    (e, xs2) <- expr xs1
    case xs2 of
        ClosePar : xs3 -> return (e, xs3)
        _              -> Nothing
atomicExpr xs = variable xs

-- neu
restAtomicExpr :: Parser [Expression]
restAtomicExpr (Number i : xs1)  = do
    (e, xs2)  <- restAtomicExpr xs1
    return (Val i:e, xs2)
restAtomicExpr (Boolean i : xs1) = do
    (e, xs2)  <- restAtomicExpr xs1
    return (ValBool i:e, xs1)
restAtomicExpr (OpenPar : xs1)   = do
    (e, xs2) <- expr xs1
    case xs2 of
        ClosePar : xs3 -> do
            (es, xs4) <- restAtomicExpr xs3
            return (e:es, xs4)
        _              -> Nothing
restAtomicExpr (Name i : xs1)    = do
    (e, xs2)  <- restAtomicExpr xs1
    return (Word i:e, xs2)
restAtomicExpr xs                = return ([], xs)

variable :: Parser Expression
variable (Name i : xs) = return (Word i, xs)
variable xs            = Nothing

ex1  = orExpr [Number 1, Plus, Number 1] -- passt
ex2  = variable [Name "wort"]-- passt
ex3  = variable [Number 1]   -- passt
ex4  = atomicExpr [Number 1] -- passt
ex5  = atomicExpr [OpenPar, Number 1, ClosePar] -- passt
ex6  = atomicExpr [OpenPar, Number 1]    -- passt
ex7  = expr [Number 4, Is, Minus, Number 5] -- passt
ex8  = expr [If, Name "x", Is, Number 5, Then, Boolean True, Else, Number 3] -- passt
ex9  = expr [Number 3, LessThan, Number 5, And, Not, Boolean False, Or, Minus, Number 6, Is, Minus, OpenPar, Minus, Number 8, ClosePar, Plus, OpenPar, Minus, Number 2, ClosePar] -- passt
ex10 = program [Name "x", Equals, Number 5, Semicolon] -- passt 
ex11 = expr [Let, Name "x", Equals, Number 2, In, Number 2, Times, Name "x"] -- passt

main :: IO()
main = print ex9