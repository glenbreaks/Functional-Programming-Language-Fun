data Token
    = Number Integer
    | Name String
    | Boolean Bool
    | Or
    | And
--     | Not    -- das selbe Token wie Minus und Neg?
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

data Expression
    = Val       Integer
    | ValBool   Bool
    | Word      String
    | OrX       Expression Expression   -- Expressions spezifizieren?
    | AndX      Expression Expression
    | NotX      Expression              -- [Token] Expression
    | CompareX  Expression Expression
    | LessThanX Expression Expression
    | IsX       Expression Expression
    | NegX      Expression
    | Diff      Expression Expression
    | Sum       Expression Expression
    | DivX      Expression Expression
    | Mult      Expression Expression
    | LetX      Expression Expression   -- sinnvoll ab hier?
    | IfX       Expression Expression Expression
    | Programs  Expression Expression
    | DefX      Expression Expression
    | LocDefs   Expression Expression
    | LocDef    Expression Expression
--     | X         Expression Expression Expression -- eig nicht mehr nötig wg IfX 
                                                    -- und LetX
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
restProgram xs = return ([], xs)

-- Def             ::= Variable Restdef
-- RestDef         ::= "=" Expr | Variable Restdef

def :: Parser Expression
def xs1 = do
    (e, xs2)  <- variable xs1
    (es, xs3) <- restDef xs2
    return (foldl DefX e es, xs3) -- fold? !!!!

restDef :: Parser [Expression]
restDef (Name i : xs1) = do
    (es, xs2) <- restDef xs1
    return (Word i:es, xs2)
restDef (Equals : xs1) = do
    (e, xs2)  <- expr xs1 -- hier Liste übergeben!
    return ([e], xs2)       -- wenn [e] hier erlaubt ist, dann war die Aufteilung 
                            -- in zwei Codezeilen mit e:es überall sonst unnötig?? 
                            -- Welche Art von Expr soll denn übergeben werden?
restDef _              = Nothing

    
-- restDef2 :: Parser Expression
-- restDef2 

locDefs :: Parser Expression
locDefs xs1 = do
    (e, xs2)  <- locDef xs1
    (es, xs3) <- restLocDefs xs2
    return (foldl LocDefs e es, xs3) -- macht fold hier Sinn? mann könnte auch 
                                      -- bei data Expression so definieren: LocDefs 
                                      -- Expression [Expression]

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
            -- (e2, xs4) <- expr xs3
            -- (es, xs5) <- restDef2 xs4
            -- return (e1:e2:es, xs5)
        _            -> Nothing -- Nothing nur bei AtomicX?
        -- hier vielleicht eher return ([], xs)?


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
-- notExpr xs1 = do
--     (e, xs2)  <- restNotExpr xs1
--     (es, xs3) <- compareExpr xs2
--     return (foldl NotX e es, xs3)
notExpr (Minus : xs1) = do
    (e, xs2)  <- compareExpr xs1
    return (NotX e, xs2)
notExpr xs1         = do        -- compareExpr ausgeschrieben
    (e, xs2)  <- negExpr xs1

    -- (es, xs3) <- restCompareExpr xs2
    case xs2 of
        LessThan : xs3 -> do
            (es, xs4) <- negExpr xs3
            return (LessThanX e es, xs3)
        Equals : xs3   -> do
            (es, xs4) <- negExpr xs3
            return (IsX e es, xs3)
        _              -> do
            (es, xs3) <- restCompareExpr xs2
            return (foldl CompareX e es, xs3) -- wie umgeht man diesen epsilon Fall?

    -- return (foldl CompareX e es, xs3) -- hier muss man schon wissen, ob < oder ==!

-- restNotExpr :: Parser [Token]
-- restNotExpr (Minus : xs) = return ([Minus], xs)
-- restNotExpr xs           = return ([], xs)

compareExpr :: Parser Expression --hier dieselben cases wie bei -,+ und /,*
compareExpr xs1 = do
    (e, xs2)  <- negExpr xs1

    case xs2 of
        LessThan : xs3 -> do
            (es, xs4) <- negExpr xs3
            return (LessThanX e es, xs3)
        -- Equals : xs3   -> do
        --     (es, xs4) <- negExpr xs3
        --     return (IsX e es, xs3)
        _              -> do
            (es, xs3) <- restCompareExpr xs2
            return (foldl CompareX e es, xs3)
            -- return ([], xs2) -- wie umgeht diesen epsilon Fall?

    -- (es, xs3) <- restCompareExpr xs2
    -- return (foldl CompareX e es, xs3)

restCompareExpr :: Parser [Expression]
-- restCompareExpr (LessThan : xs1) = do
--     case xs1 of
--         Minus : xs2 ->  do      -- NegExpr ausgeschrieben
--             (e, xs2)  <- multExpr xs1
--             (es, xs3) <- restAddExpr xs2
--             return (e:es, xs2)
--         _           -> do
--             (e, xs2)  <- multExpr xs1
--             (es, xs3) <- restAddExpr xs2
--             return (e:es, xs2)
--     -- -- (e, xs2) <- negExpr xs1
--     -- return (e, xs2)       -- geht das
restCompareExpr (Is : xs1)       = do
    case xs1 of
        Minus : xs2 ->  do      -- NegExpr ausgeschrieben
            (e, xs2)  <- multExpr xs1
            (es, xs3) <- restAddExpr xs2
            return (e:es, xs2)
        _           -> do
            (e, xs2)  <- multExpr xs1
            (es, xs3) <- restAddExpr xs2
            return (e:es, xs2)
restCompareExpr xs               = return ([], xs)

negExpr :: Parser Expression
negExpr (Minus : xs1) = do
    (e, xs2)  <- addExpr xs1
    return (NegX e, xs2)
negExpr xs1           = do
    (e, xs2)  <- multExpr xs1
    -- (es, xs3) <- restAddExpr xs2 -- weg*
    case xs2 of 
        Minus : xs3 -> do
            (es, xs4) <- multExpr xs3
            return (Diff e es, xs3)
        _ : xs3     -> do
            (es, xs3) <- restAddExpr xs2
            return (foldl Sum e es, xs3)

addExpr :: Parser Expression
addExpr xs1 = do
    (e, xs2)  <- multExpr xs1
    -- (es, xs3) <- restAddExpr xs2 -- weg*
    case xs2 of 
        Minus : xs3 -> do
            (es, xs4) <- multExpr xs3
            return (Diff e es, xs3)
        _ : xs3     -> do
            (es, xs3) <- restAddExpr xs2
            return (foldl Sum e es, xs3)
    -- return (foldl Sum e es, xs3) -- weg*

restAddExpr :: Parser [Expression] -- wird jetzt nur noch aufgrufen falls Plus kommt
-- restAddExpr (Minus : xs1) = do
--     (e, xs2)  <- atomicExpr xs1
--     (es, xs3) <- restMultExpr xs2
--     return (e:es, xs3)
restAddExpr (Plus : xs1)  = do
    (e, xs2)  <- multExpr xs1
    (es, xs3) <- restAddExpr xs2
    return (e:es, xs3)
restAddExpr xs            = return ([], xs)

multExpr :: Parser Expression
multExpr xs1 = do
    (e, xs2)  <- atomicExpr xs1
    -- (es, xs3) <- restMultExpr xs2 -- weg*
    case xs2 of 
        Div : xs3 -> do
            (es, xs4) <- atomicExpr xs3
            return (DivX e es, xs3)
        _ : xs3   -> do
            (es, xs3) <- restMultExpr xs2
            return (foldl Mult e es, xs3)
    -- return (foldl Mult e es, xs3) -- weg*

restMultExpr :: Parser [Expression]
-- restMultExpr (Div : xs1)   = do 
--     (e, xs2) <- atomicExpr xs1
--     return ([e], xs2)
restMultExpr (Times : xs1) = do
    (e, xs2)  <- atomicExpr xs1
    (es, xs3) <- restMultExpr xs2
    return (e:es, xs3)
restMultExpr xs            = return ([], xs)

atomicExpr :: Parser Expression
-- atomicExpr (Variable : xs) ??? wie!
atomicExpr (Number i : xs)  = return (Val i, xs)
atomicExpr (Boolean i : xs) = return (ValBool i, xs)
atomicExpr (OpenPar : xs1)  = do
    (e, xs2) <- expr xs1
    case xs2 of
        ClosePar : xs3 -> return (e, xs3)
        _              -> Nothing
atomicExpr xs = variable xs
-- atomicExpr _ = Nothing

variable :: Parser Expression
variable (Name i : xs) = return (Word i, xs)
variable xs            = Nothing