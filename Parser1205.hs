data Token
    = Number Integer
    | Name String
    | Boolean Bool
    | Or
    | And
    | Not
    | LessThan
    | Is
    -- | Neg
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

data Expression
    = Val       Integer -- Value
    | ValBool   Bool
    | Word      String  -- für Name
    | OrX       Expression Expression -- Expressions spezifizieren?
    | AndX      Expression Expression
    | NotX      Expression
    | CompareX  Expression Expression
--     | LessThanX Expression Expression
--     | IsX       Expression Expression
    | NegX      Expression
    | Diff      Expression Expression
    | Sum       Expression Expression
    | DivX      Expression Expression
    | Mult      Expression Expression
    | LetX      Expression Expression
    | IfX       Expression Expression
    | ProgramX  Expression Expression   -- sinnvoll ab hier?
    | DefX      Expression Expression Expression
    | LocDefsX  Expression Expression
    | LocDefX   Expression Expression
    -- | X         Expression Expression Expression
    -- wie funktioniert das lol
    deriving Show

type Parser a = [Token] -> Maybe (a, [Token])

program :: Parser [Expression]
program xs1 = do
    (e, xs2)  <- def xs1
    case xs2 of
        Semicolon : xs3 -> do
            (es, xs4) <- restProgram xs3
            return (e:es, xs4)
        _ -> Nothing

restProgram :: Parser [[Expression]]
restProgram xs1 = do
    (e, xs2)  <- def xs1
    case xs2 of
        Semicolon : xs3 -> do
            (es, xs3) <- restProgram xs2
            return (e:es, xs3)
        _ -> Nothing

-- Def             ::= Variable Restdef
-- RestDef         ::= "=" Expr | Variable Restdef

def :: Parser [Expression]
def xs1 = do
    (e1, xs2) <- variable xs1
    (e2, xs3) <- restDef xs2
    return (e:es, xs3)

restDef :: Parser [Expression]
restDef (Name i : xs1) = restDef xs1
restDef (Equals : xs1) = expr xs1
restDef _ = Nothing

    
-- restDef2 :: Parser Expression
-- restDef2 

locDefs :: Parser [Expression]
locDefs xs1 = do
    (e, xs2)  <- locDef xs1
    (es, xs3) <- restLocDefs xs2
    return (e:es, xs3)

restLocDefs :: Parser [Expression]
restLocDefs (Semicolon : xs1) = do
    (e, xs2)  <- locDef xs1
    (es, xs3) <- restLocDefs xs2
    return (e:es, xs3)
restLocDefs xs = return ([], xs)

locDef :: Parser Expression
locDef xs1 = do
    (e, xs2) <- variable xs1
    case xs2 of
        Equals : xs3 -> do
            (es, xs4) <- expr xs3
            return (e : es, xs4)
            -- (e2, xs4) <- expr xs3
            -- (es, xs5) <- restDef2 xs4
            -- return (e1:e2:es, xs5)
        _ -> Nothing -- Nothing nur bei AtomicX?
        -- hier vielleicht eher return ([], xs)?


expr :: Parser [Expression] -- wirklich Parser X?
expr (Let : xs1) = do
    (e, xs2) <- locDefs xs1
    case xs2 of
        In : xs3 -> do
            (es, xs4) <- expr xs3
            return (e:es, xs4)
        _ -> Nothing
expr (If : xs1) = do
    (e1, xs2) <- expr xs1
    case xs2 of
        Then : xs3 -> do
            (e2, xs4) <- expr xs3
            case xs4 of
                Else : xs5 -> do
                    (es, xs6) <- expr xs5
                    return (e1:e2:es, xs6)
                _ -> Nothing
        _ -> Nothing
expr xs = orExpr xs

-- ab hier analog zum ToyParser
orExpr :: Parser Expression
orExpr xs1 = do
    (e, xs2)  <- andExpr xs1
    (es, xs3) <- restOrExpr xs2
    return (foldr OrX e es, xs3)

restOrExpr :: Parser [Expression]
restOrExpr (Or : xs1) = do
    (e, xs2)  <- andExpr xs1
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
    (e, xs2)  <- notExpr xs1
    (es, xs3) <- restAndExpr xs2
    return (e:es, xs3)
restAndExpr xs          = return ([], xs)

notExpr :: Parser Expression
notExpr (Not : xs1) = do
    (e, xs2) <- compareExpr xs1
    return (NotX e, xs2)
notExpr xs1         = do
    (e, xs2) <- compareExpr xs1
    return (NotX e, xs2) -- type check bool?

compareExpr :: Parser Expression
compareExpr xs1 = do
    (e, xs2)  <- negExpr xs1
    (es, xs3) <- restCompareExpr xs2
    return (foldl CompareX e es, xs3)

restCompareExpr :: Parser [Expression]
restCompareExpr (LessThan : xs1) = do
    case xs1 of
        Minus : xs2 ->  do      -- NegEx ausgeschrieben
            (e, xs2)  <- multExpr xs1
            (es, xs3) <- restAddExpr xs2
            return (e:es, xs2)
        _ -> do
            (e, xs2)  <- multExpr xs1
            (es, xs3) <- restAddExpr xs2
            return (e:es, xs2)
    -- -- (e, xs2) <- negExpr xs1
    -- return (e, xs2)       -- geht das
restCompareExpr (Is : xs1)       = do
    case xs1 of
        Minus : xs2 ->  do      -- NegEx ausgeschrieben
            (e, xs2)  <- multExpr xs1
            (es, xs3) <- restAddExpr xs2
            return (e:es, xs2)
        _ -> do
            (e, xs2)  <- multExpr xs1
            (es, xs3) <- restAddExpr xs2
            return (e:es, xs2)
restCompareExpr xs               = return ([], xs)

negExpr :: Parser Expression
negExpr (Minus : xs1) = do
    (e, xs2)  <- addExpr xs1
    return (NegX e, xs2)
negExpr xs1          = do
    (e, xs2)  <- addExpr xs1
    return (NegX e, xs2) -- anderer Rückgabe typ!!

addExpr :: Parser Expression
addExpr xs1 = do
    (e, xs2)  <- multExpr xs1
    (es, xs3) <- restAddExpr xs2
    return (foldl Sum e es, xs3)

restAddExpr :: Parser [Expression] -- doch 2 funktionen? eine mit [Expression] eine mit Expression
restAddExpr (Minus : xs1) = do
    (e, xs2)  <- atomicExpr xs1
    (es, xs3) <- restMultExpr xs2
    return (e:es, xs3)
restAddExpr (Plus : xs1)  = do
    (e, xs2)  <- multExpr xs1
    (es, xs3) <- restAddExpr xs2
    return (e:es, xs3)
restAddExpr xs           = return ([], xs)

multExpr :: Parser Expression
multExpr xs1 = do
    (e, xs2)  <- atomicExpr xs1
    (es, xs3) <- restMultExpr xs2
    return (foldl Mult e es, xs3)

restMultExpr :: Parser [Expression] -- Lösung: hier A / B / C ermöglichen, dann später abfangen
restMultExpr (Div : xs1)   = do 
    (e, xs2) <- atomicExpr xs1  -- also hier atomic & restMult
    return ([e], xs2)             -- dann hier Liste zurück
restMultExpr (Times : xs1) = do
    (e, xs2)  <- atomicExpr xs1
    (es, xs3) <- restMultExpr xs2
    return (e:es, xs3)

atomicExpr :: Parser Expression
-- atomicExpr (Variable : xs) ??? wie!
atomicExpr (Number i : xs)  = return (Val i, xs)
atomicExpr (Boolean i : xs) = return (ValBool i, xs)
atomicExpr (OpenPar : xs1)  = do
    (e, xs2) <- expr xs1
    case xs2 of
        ClosePar : xs3 -> return (e, xs3)
        _ -> Nothing
atomicExpr _ = Nothing

variable :: Parser Expression
variable (Name i : xs) = return (Word i, xs)
variable xs            = Nothing