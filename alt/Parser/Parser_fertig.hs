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

data Definition = Definition [Expression] Expression deriving Show

newtype LocDefs = LocDefs [LocDef] deriving Show

data LocDef     = LocDef Expression Expression deriving Show

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

type Parser a = [Token] -> Maybe (a, [Token])

program :: Parser Program
program xs1 = do
    (e, xs2)  <- def xs1
    case xs2 of
        Semicolon : xs3 -> do
            (es, xs4) <- restProgram xs3
            case xs4 of
                [] -> return (Program (e:es), xs4)
                _  -> Nothing
        _               -> Nothing

restProgram :: Parser [Definition]
restProgram xs1 = do
    case xs1 of
        Name i : _ -> do
            (e, xs2) <- def xs1
            case xs2 of
                Semicolon : xs3 -> do
                    (es, xs4) <- restProgram xs3
                    return (e:es, xs4)
                _               -> Nothing
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

addExpr :: Parser Expression
addExpr xs1 = do
    (e, xs2)  <- multExpr xs1
    case xs2 of 
        Minus : xs3 -> do
            (es, xs4) <- multExpr2 xs3 -- damit 5--2 nicht geht
            return (Diff e es, xs4)
        Plus :  _   -> do
            (es, xs3) <- restAddExpr xs2
            return (foldl Sum e es, xs3)
        _           -> return (e, xs2)

restAddExpr :: Parser [Expression]
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
            (es, xs4) <- atomicExpr xs3 -- negExpr übersprungen, damit 5/-2 nicht geht
            return (Div e es, xs4)
        Times : _   -> do
            (es, xs3) <- restMultExpr xs2
            return (foldl Mult e es, xs3)
        _           -> return (e, xs2)

multExpr2 :: Parser Expression -- falls keine Negation möglich sein soll, zB (5--3) ist doof
multExpr2 xs1 = do
    (e, xs2)  <- atomicExpr xs1
    case xs2 of 
        DivBy : xs3 -> do
            (es, xs4) <- atomicExpr xs3
            return (Div e es, xs4)
        Times : _   -> do
            (es, xs3) <- restMultExpr xs2
            return (foldl Mult e es, xs3)
        _           -> return (e, xs2)

restMultExpr :: Parser [Expression]
restMultExpr (Times : xs1) = do
    (e, xs2)  <- atomicExpr xs1 -- damit 5--2 nicht geht
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
        _              -> Nothing
atomicExpr (Name i : xs1)    = do
    (is, xs2) <- restAtomicExpr xs1
    return (foldl Function (Variable i) is, xs2)
atomicExpr _                 = Nothing

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
        _              -> Nothing
restAtomicExpr (Name i : xs1)    = do
    (is, xs2) <- restAtomicExpr xs1
    return (Variable i:is, xs2)
restAtomicExpr xs                = return ([], xs)

variable :: Parser Expression
variable (Name i : xs) = return (Variable i, xs)
variable _             = Nothing

main :: IO()
main = print ex9

ex1  = orExpr [Number 1, Plus, Number 1] -- passt
ex2  = variable [Name "wort"] -- passt
ex3  = variable [Number 1] -- passt
ex4  = atomicExpr [Number 1] -- passt
ex5  = atomicExpr [OpenPar, Number 1, ClosePar] -- passt
ex6  = atomicExpr [OpenPar, Number 1]    -- passt
ex7  = expr [Number 4, Is, Minus, Number 5] -- passt
ex8  = expr [If, Name "x", Is, Number 5, Then, Boolean True, Else, Number 3] -- passt
ex9  = expr [Number 3, LessThan, Number 5, And, Not, Boolean False, Or, Minus, Number 6, Is, Minus, OpenPar, Minus, Number 8, ClosePar, Plus, OpenPar, Minus, Number 2, ClosePar] -- passt
ex10 = program [Name "x", Equals, Number 5, Semicolon] -- passt 
ex11 = expr [Let, Name "x", Equals, Number 2, In, Number 2, Times, Name "x"] -- passt
ex12 = program [Name "f",  Name "x", Equals, Number 2, Number 3, Times, Name "x", Semicolon]
ex13 = expr [Name "f", OpenPar, Name "a", LessThan, Name "b", ClosePar] -- passt
ex14 = expr [Number 1, Times, Number 2, Times, Number 3] -- passt
ex15 = program [Name "a", Name "b", Name "c", Equals, Number 1, Times, Number 2, DivBy, Number 4, Semicolon] -- passt
ex16 = expr [Minus, Number 2, Times, Number 3] -- passt
ex17 = expr [Minus, Number 2, Minus, OpenPar, Minus, Number 5, ClosePar] -- passt
ex18 = expr [Minus, Number 2, Plus, OpenPar, Minus, Number 5, ClosePar] -- passen alle
ex19 = expr [Minus, Number 2, Minus, OpenPar, Minus, Number 5, ClosePar]
ex20 = expr [Minus, Number 3, Times, OpenPar, Minus, Number 4, ClosePar]
ex21 = expr [Minus, Number 2, DivBy, OpenPar, Minus, Number 5, ClosePar]
ex22 = compareExpr [Minus, Number 2, LessThan, Minus, Number 1, LessThan, Number 10] --passt
ex23 = compareExpr [Minus, Number 2, LessThan, Minus, Number 1] -- passt
ex24 = addExpr [Number 2, Plus, Number 3, Plus, Number 4] -- passt
ex25 = multExpr [Number 2, Times, Number 3, Times, Number 4] -- passt
ex26 = expr [Not, Boolean True, Is, Boolean False] -- passt
ex27 = def [Boolean True, Equals, Boolean False] -- diesen Fehler mit Nothing abfangen wie bei compare???
ex28 = expr [Boolean True, And, Boolean False] -- passt
ex29 = expr [Number 1, And, Number 2] -- passt
ex30 = expr [Boolean True, And, Boolean False, And, Name "d"] -- passt
ex31 = expr [Number 1, Or, Number 2, Or, Number 3] -- passt
ex32 = expr [Let, Name "x", Equals, Number 5, In, Name "x", Times, Name "x"] -- passt
ex33 = expr [Let, Name "x", Equals, Number 5, Semicolon, Name "y", Equals, Number 4, In, Name "x", Times, Name "y"] -- passt
ex34 = expr [If, Name "x", Is, Number 3, Or, Name "x", Is, Number 5, Then, Boolean True, Else, Boolean False] -- passt
ex35 = def [Name "a", Name "b", Equals, Name "a", Or, Name "b", Plus, Number 3] -- passt
ex36 = program [Name "a", Name "b", Equals, Name "a", Or, Name "b", Plus, Number 3, Semicolon, Name "z", Equals, Boolean True, Semicolon] -- passt
ex37 = program [Name "a", Name "b", Equals, Name "a", Or, Name "b", Plus, Number 3, Semicolon, Name "a", Name "b", Equals, Name "a", Or, Name "b", Plus, Number 3, Semicolon] -- passt
ex38 = program [Name "a", Name "b", Equals, Name "a", Or, Name "b", Plus, Number 3, Semicolon, Name "x", Equals, Number 3, Semicolon] -- passt
ex39 = program [Name "a", Equals, Boolean True, Semicolon, Name "y", Equals, Boolean False, Semicolon] -- passt
ex40 = expr [Number 1, Or, Number 2, Or, Number 3, And, Number 4, And, Number 5]
ex41 = program [Name "f", Equals, Number 2, Times, Number 3, DivBy, Number 2, Times, Number 3]


-- bool x = x == true | x == false;
-- f x = if bool x | x < 1
-- then 1
-- else x * f (x - 1);
-- main = f 6;

exScript = program [Name "bool", Name "x", Equals, Name "x", Is, Boolean True, Or, Name "x", Is, Boolean False, Semicolon, Name "f", Name "x", Equals, If, Name "bool", Name "x", Or, Name "x", LessThan, Number 1, Then, Number 1, Else, Name "x", Times, Name "f", OpenPar, Name "x", Minus, Number 1, ClosePar, Semicolon]