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

newtype Program = Program [Definition] deriving Show

data Definition = Definition [AtomicExpression] Expression deriving Show

newtype LocDefs = LocDefs [LocDef] deriving Show

data LocDef     = LocDef AtomicExpression Expression deriving Show

data Expression
    -- = Val       Integer -- returntyp für Number
    -- | ValBool   Bool    -- returntyp für Boolean
    -- | Word      String  -- returntyp für Variable
    = LetX      LocDefs Expression
    | IfX       Expression Expression Expression
    | OrX       Expression Expression
    | AndX      Expression Expression
    | NotX      Expression
    | LessThanX Expression Expression
    | IsX       Expression Expression
    | Neg       Expression
    | Diff      Expression Expression
    | Sum       Expression Expression
    | Div       Expression Expression
    | Mult      Expression Expression
    | A         [AtomicExpression]
    -- | Programs  Expression Expression -- haben jetzt eigene types
    -- | Def       Expression Expression
    -- | LocDefs   Expression Expression
    -- | LocDef    Expression Expression
    -- | Atomics   Expression Expression
    deriving Show

-- newtype AtomicExpressions = AtomicExpressions [AtomicExpression] deriving Show

data AtomicExpression
    = Val      Integer
    | BoolVal  Bool
    | Variable String
    | NewX     Expression
    deriving Show

--

type Parser a = [Token] -> Maybe (a, [Token])

program :: Parser Program
program xs1 = do
    (e, xs2)  <- def xs1
    case xs2 of
        Semicolon : xs3 -> do
            (es, xs4) <- restProgram xs3
            return (Program (e:es), xs4)
        _               -> Nothing

restProgram :: Parser [Definition]
restProgram (Name i : xs1) = do
            (e, xs2)  <- def xs1
            case xs2 of
                Semicolon : xs3 -> do
                    (es, xs4) <- restProgram xs3
                    return (e:es, xs4)
                _               -> Nothing
restProgram xs             = return ([], xs)

def :: Parser Definition
def xs1 = do
    (e, xs2)  <- variable xs1
    (es, xs3) <- restDef xs2
    (f, xs4)  <- expr xs3
    return (Definition (e:es) f, xs4)

restDef :: Parser [AtomicExpression]
restDef (Name i : xs1) = do
    (is, xs2) <- restDef xs1
    return (Variable i:is, xs2)
restDef (Equals : xs1) = return ([], xs1)
restDef _              = Nothing


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

notExpr :: Parser Expression
notExpr (Not : xs1) = do
    (e, xs2)  <- compareExpr xs1
    return (NotX e, xs2)
notExpr xs = compareExpr xs

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

addExpr :: Parser Expression
addExpr xs1 = do
    (e, xs2)  <- multExpr xs1 -- Präzedenzänderung: Add, Mult, Neg
    case xs2 of 
        Minus : xs3 -> do
            (es, xs4) <- multExpr2 xs3 -- damit 5--2 nicht geht
            return (Diff e es, xs4)
        Plus :  _   -> do
            (es, xs3) <- restAddExpr xs2
            return (foldl Sum e es, xs3)
        _           -> multExpr xs1

restAddExpr :: Parser [Expression] -- wird jetzt nur noch aufgrufen falls Plus kommt
restAddExpr (Plus : xs1)  = do
    (e, xs2)  <- multExpr2 xs1 -- damit 5+-2 nicht geht
    (es, xs3) <- restAddExpr xs2
    return (e:es, xs3)
restAddExpr xs            = return ([], xs)

multExpr :: Parser Expression
multExpr xs1 = do
    (e, xs2)  <- negExpr xs1
    case xs2 of 
        DivBy : xs3 -> do
            (es, xs4) <- multExpr2 xs3 -- damit 5/-2 nicht geht
            return (Div e es, xs4)
        Times : _   -> do
            (es, xs3) <- restMultExpr xs2
            return (foldl Mult e es, xs3)
        _           -> negExpr xs1

multExpr2 :: Parser Expression -- falls keine Negation möglich sein soll, zB (5--3) ist doof
multExpr2 xs1 = do
    (e, xs2) <- atomicExpr xs1
    case xs2 of 
        DivBy : xs3 -> do
            (es, xs4) <- multExpr xs3
            return (Div e es, xs4)
        Times : _   -> do
            (es, xs3) <- restMultExpr xs2
            return (foldl Mult e es, xs3)
        _           -> atomicExpr xs1

restMultExpr :: Parser [Expression]
restMultExpr (Times : xs1) = do
    (e, xs2)  <- multExpr2 xs1 -- damit 5--2 nicht geht
    (es, xs3) <- restMultExpr xs2
    return (e:es, xs3)
restMultExpr xs            = return ([], xs)

negExpr :: Parser Expression
negExpr (Minus : xs1) = do
    (e, xs2)  <- atomicExpr xs1
    return (Neg e, xs2)
negExpr xs            = atomicExpr xs

atomicExpr :: Parser Expression
atomicExpr (Number i : xs1)  = do
    (is, xs2) <- restAtomicExpr xs1
    return (A (Val i:is), xs2)
atomicExpr (Boolean i : xs1) = do
    (is, xs2) <- restAtomicExpr xs1
    return (A (BoolVal i:is), xs2)
atomicExpr (OpenPar : xs1)   = do
    (e, xs2)  <- expr xs1
    case xs2 of
        ClosePar : xs3 -> do
            (es, xs4) <- restAtomicExpr xs3
            return (A (NewX e:es), xs4)
        _              -> Nothing
atomicExpr (Name i : xs1)    = do
    (is, xs2) <- restAtomicExpr xs1
    return (A (Variable i:is), xs2)
atomicExpr _                 = Nothing

restAtomicExpr :: Parser [AtomicExpression]
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
            return (NewX e:es, xs4)
        _              -> Nothing
restAtomicExpr (Name i : xs1)    = do
    (is, xs2) <- restAtomicExpr xs1
    return (Variable i:is, xs2)
restAtomicExpr xs                = return ([], xs)

variable :: Parser AtomicExpression
variable (Name i : xs) = return (Variable i, xs)
variable _             = Nothing

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
ex12 = program [Name "f",  Name "x", Equals, Number 2, Number 3, Times, Name "x", Semicolon]
main :: IO()
main = print ex9

ex13 = expr [Name "f", OpenPar, Name "a", LessThan, Name "b", ClosePar] -- passt
ex14 = expr [Number 1, Times, Number 2, Times, Number 3] -- passt
ex15 = program [Name "a", Name "b", Name "c", Equals, Number 1, Times, Number 2, DivBy, Number 4, Semicolon] -- passt
ex16 = expr [Minus, Number 2, Times, Number 3] -- passt
ex17 = expr [Minus, Number 2, Minus, OpenPar, Minus, Number 5, ClosePar] -- passt
ex18 = expr [Minus, Number 2, Plus, OpenPar, Minus, Number 5, ClosePar] -- passen alle
ex19 = expr [Minus, Number 2, Minus, OpenPar, Minus, Number 5, ClosePar]
ex20 = expr [Minus, Number 3, Times, OpenPar, Minus, Number 4, ClosePar]
ex21 = expr [Minus, Number 2, DivBy, OpenPar, Minus, Number 5, ClosePar]