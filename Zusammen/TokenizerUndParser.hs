import Data.Char(isDigit, isAlpha, isAlphaNum)
import Control.Exception (Exception, throw)

data CompilerException 
    = InvalidName !String
    | MissingMain 
    | Undefined 
    deriving Show

instance Exception CompilerException

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

newtype Program = Program [Definition] deriving Show

data Definition = Definition [Expression] Expression deriving Show

newtype LocDefs = LocDefs [LocDef] deriving Show

data LocDef     = LocDef Expression Expression deriving Show

type Parser a = [Token] -> Maybe (a, [Token])

----------------------------------------------------------------------------------------------------------------------------------------------

spaceyfier :: String -> String
spaceyfier x = 
   case x of
       ';' : xs       -> " ;"   ++ spaceyfier xs        -- hier kein Leerzeichen danach, da Strichpunkte in Namen eh nicht erlaubt sind!
       '|' : xs       -> " | "  ++ spaceyfier xs
       '&' : xs       -> " & "  ++ spaceyfier xs
       '<' : xs       -> " < "  ++ spaceyfier xs
       '=' : '=' : xs -> " == " ++ spaceyfier xs
       '=' : xs       -> " = "  ++ spaceyfier xs
       '+' : xs       -> " + "  ++ spaceyfier xs
       '-' : xs       -> " - "  ++ spaceyfier xs
       '*' : xs       -> " * "  ++ spaceyfier xs
       '/' : xs       -> " / "  ++ spaceyfier xs
       '(' : xs       -> " ( "  ++ spaceyfier xs
       ')' : xs       -> " ) "  ++ spaceyfier xs
       []             -> []
       _   : xs       -> head x : spaceyfier xs

tokenizer :: [String] -> [Token]
tokenizer ("|"     : xs) = Or            : tokenizer xs
tokenizer ("&"     : xs) = And           : tokenizer xs
tokenizer ("not"   : xs) = Not           : tokenizer xs
tokenizer ("<"     : xs) = LessThan      : tokenizer xs
tokenizer ("=="    : xs) = Is            : tokenizer xs
tokenizer ("-"     : xs) = Minus         : tokenizer xs
tokenizer ("+"     : xs) = Plus          : tokenizer xs
tokenizer ("/"     : xs) = DivBy         : tokenizer xs
tokenizer ("*"     : xs) = Times         : tokenizer xs
tokenizer ("("     : xs) = OpenPar       : tokenizer xs
tokenizer (")"     : xs) = ClosePar      : tokenizer xs
tokenizer ("let"   : xs) = Let           : tokenizer xs
tokenizer ("in"    : xs) = In            : tokenizer xs
tokenizer ("if"    : xs) = If            : tokenizer xs
tokenizer ("then"  : xs) = Then          : tokenizer xs
tokenizer ("else"  : xs) = Else          : tokenizer xs
tokenizer (";"     : xs) = Semicolon     : tokenizer xs
tokenizer ("="     : xs) = Equals        : tokenizer xs
tokenizer ("True"  : xs) = Boolean True  : tokenizer xs 
tokenizer ("False" : xs) = Boolean False : tokenizer xs
tokenizer []             = []
tokenizer (x:xs)
    | checkNumber x                   = Number (read x) : tokenizer xs
    | isAlpha (head x) && checkName x = Name x          : tokenizer xs
    | otherwise                       = throw (InvalidName x) 

checkNumber :: String -> Bool
checkNumber (x:xs) = isDigit x && checkNumber xs
checkNumber []     = True

checkName :: String -> Bool
checkName (x:xs) = (isAlphaNum x || x == '_' || x == '\'') && checkName xs
checkName []     = True

tokenize xs = tokenizer $ words $ spaceyfier xs

ex1 = tokenizer ["main", "=", "foo", "x", "y", "=", "2", "*", "x", "+", "y", ";"]
ex2 = tokenize "f a b c = 2*a + 3*x/4;"
ex3 = tokenizer ["f", ";x", "23", ";"] -- False (Exception: InvalidName)


----------------------------------------------------------------------------------------------------------------------------------------------

program :: Parser Program
program xs1 = do
    (e, xs2)  <- def xs1
    case xs2 of
        Semicolon : xs3 -> do
            (es, xs4) <- restProgram xs3
            case xs4 of
                [] -> return (Program (e:es), xs4)
                _  -> Nothing     -- immer wenn die Restliste nicht leer ist (wenn Code nicht vollständig geparst werden konne) -> Nothing 
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

ex4 = program [Name "main", Equals, Number 1, Plus, Number 2, Semicolon]
ex5 = locDef [Name "x", Equals, Not, Boolean True]


----------------------------------------------------------------------------------------------------------------------------------------------

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

ex6 = expr [Boolean True, Or, Boolean False]
ex7 = expr [Boolean True, Is, Boolean True, Or, Boolean False]
ex8 = expr [Number 2, LessThan, Number 3, LessThan, Number 4] --hier wird Code nicht fertig geparsed, nichtleere Liste wird in program abgefangen -> Nothing


----------------------------------------------------------------------------------------------------------------------------------------------

addExpr :: Parser Expression
addExpr xs1 = do
    (e, xs2)  <- multExpr xs1
    case xs2 of 
        Minus : xs3 -> do
            (es, xs4) <- multExpr2 xs3      -- damit 5--2 nicht geht
            return (Diff e es, xs4)
        Plus :  _   -> do
            (es, xs3) <- restAddExpr xs2
            return (foldl Sum e es, xs3)
        _           -> return (e, xs2)

restAddExpr :: Parser [Expression]
restAddExpr (Plus : xs1)  = do
    (e, xs2)  <- multExpr2 xs1              -- damit 5+-2 nicht geht
    (es, xs3) <- restAddExpr xs2
    return (e:es, xs3)
-- restAddExpr (Minus : xs1) = do -- würde ermöglichen: a + b - c, diese eingaben sind aber nicht erlaubt da + und - dieselbe Präzedenz haben
--     (e, xs1) <-  multExpr xs1
--     return ([Neg e], xs1)
restAddExpr xs            = return ([], xs)

multExpr :: Parser Expression
multExpr xs1 = do
    (e, xs2)  <- negExpr xs1
    case xs2 of 
        DivBy : xs3 -> do
            (es, xs4) <- atomicExpr xs3     -- negExpr übersprungen, damit 5/-2 nicht geht
            return (Div e es, xs4)
        Times : _   -> do
            (es, xs3) <- restMultExpr xs2
            return (foldl Mult e es, xs3)
        _           -> return (e, xs2)

multExpr2 :: Parser Expression              -- falls keine Negation möglich sein soll, zB (5--3) ist doof
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
    (e, xs2)  <- atomicExpr xs1             -- damit 5--2 nicht geht
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

tokUndPar xs = program $ tokenizer $ words $ spaceyfier xs

ex9 = addExpr [Number 5, Plus, Minus, Number 2] -- erklärung multexpr 2
ex10 = addExpr [Number 1 , Plus, Number 2, Minus, Number 3, Minus, Number 5] -- 3-2+1= 0 / 2
ex11 = addExpr [Number 1 , Minus, Number 2, Minus, Number 3]
