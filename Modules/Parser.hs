module Parser (showParse, parse) where
import Datatypes
import Tokenizer
import Show
---------- Parser:

-- bsp zeigen, showParse bsp

showParse :: String -> IO()
showParse xs =
    case parse xs of
        Left x -> putStrLn x
        Right (Program xs, _) -> putStr (showBoxed "Parser" ++ "\n\n" ++ hshowParse xs ++ "\n")
            where
                hshowParse (x:xs) = show x ++ hshowParse xs
                hshowParse []     = ""

parse :: String -> Either String (Program, [Token])
parse xs = parseProgram $ tokenize xs

parseProgram :: Parser Program
parseProgram xs1 = do
    (e, xs2)  <- parseDef xs1
    case xs2 of
        Semicolon : xs3 -> do
            (es, xs4) <- parseRestProgram xs3
            case xs4 of
                []    -> return (Program (e:es), xs4)
                (x:_) -> Left ("Parse error on input: " ++ show x)   -- immer wenn die Restliste nicht leer ist (wenn Code nicht vollständig geparst werden konnte) -> Fehler  
        _               -> Left ("Semicolon expected after definition " ++ showDef e) -- showDef zieht Funktionsnamen aus Datentyp Definition

-- showParse "main = 1;, false," (allg Fehlerbehandlung) 396

parseRestProgram :: Parser [Definition]
parseRestProgram xs1 = do
    case xs1 of
        Name i : _ -> do
            (e, xs2) <- parseDef xs1
            case xs2 of
                Semicolon : xs3 -> do
                    (es, xs4) <- parseRestProgram xs3
                    return (e:es, xs4)
                _               -> Left ("Semicolon expected after definition " ++ showDef e)
        _          -> return ([], xs1)

parseDef :: Parser Definition
parseDef xs1 = do
    (e, xs2)  <- parseVariable xs1
    (es, xs3) <- parseRestDef xs2
    (f, xs4)  <- parseExpr xs3
    return (Definition (e:es) f, xs4)

parseRestDef :: Parser [Expression]
parseRestDef (Name i : xs1) = do
    (is, xs2) <- parseRestDef xs1
    return (Variable i:is, xs2)
parseRestDef (Equals : xs1) = return ([], xs1)
parseRestDef _              = Left "Definition incomplete"

parseLocDefs :: Parser LocDefs
parseLocDefs xs1 = do
    (e, xs2)  <- parseLocDef xs1
    (es, xs3) <- parseRestLocDefs xs2
    return (LocDefs (e:es), xs3)

parseRestLocDefs :: Parser [LocDef]
parseRestLocDefs (Comma : xs1) = do
    (e, xs2)  <- parseLocDef xs1
    (es, xs3) <- parseRestLocDefs xs2
    return (e:es, xs3)
parseRestLocDefs xs                = return ([], xs)

parseLocDef :: Parser LocDef
parseLocDef xs1 = do
    (e, xs2)  <- parseVariable xs1
    case xs2 of
        Equals : xs3 -> do
            (es, xs4) <- parseExpr xs3
            return (LocDef e es, xs4)
        _            -> Left "Local definition incomplete"

parseExpr :: Parser Expression
parseExpr (Let : xs1) = do
    (e, xs2)  <- parseLocDefs xs1
    case xs2 of
        In : xs3 -> do
            (es, xs4) <- parseExpr xs3
            return (LetX e es, xs4)
        _        -> Left "Expected 'in' after local definition"
parseExpr (If : xs1)  = do
    (e1, xs2) <- parseExpr xs1
    case xs2 of
        Then : xs3 -> do
            (e2, xs4) <- parseExpr xs3
            case xs4 of
                Else : xs5 -> do
                    (es, xs6) <- parseExpr xs5
                    return (IfX e1 e2 es, xs6)
                _          -> Left "Expected 'else' after 'then' block"
        _          -> Left "Expected 'then' after 'if' block"
parseExpr xs          = parseOrExpr xs

-- Beispiele: Semicolon weg, Equals weg

----------------------------------------------------------

parseOrExpr :: Parser Expression
parseOrExpr xs1 = do
    (e, xs2)  <- parseAndExpr xs1
    case xs2 of
        Or : xs3 -> do
            (es, xs4) <- parseOrExpr xs3
            return (OrX e es, xs4)
        _        -> return (e, xs2)

parseAndExpr :: Parser Expression
parseAndExpr xs1 = do
    (e, xs2)  <- parseNotExpr xs1
    case xs2 of
        And : xs3 -> do
            (es, xs4) <- parseAndExpr xs3
            return (AndX e es, xs4)
        _         -> return (e, xs2)

parseNotExpr :: Parser Expression
parseNotExpr (Not : xs1) = do
    (e, xs2)  <- parseCompareExpr xs1
    return (NotX e, xs2)
parseNotExpr xs          = parseCompareExpr xs

parseCompareExpr :: Parser Expression
parseCompareExpr xs1 = do
    (e, xs2)  <- parseAddExpr  xs1
    case xs2 of
        LessThan : xs3 -> do
            (es, xs4) <- parseAddExpr  xs3
            return (LessThanX e es, xs4)
        Is : xs3       -> do
            (es, xs4) <- parseAddExpr  xs3
            return (IsX e es, xs4)
        _              -> return (e, xs2)

parseAddExpr  :: Parser Expression
parseAddExpr  xs1 = do
    (e, xs2)  <- parseMultExpr  xs1
    (es, xs3) <- parseRestAddExpr xs2
    return (foldl Sum e es, xs3)

parseRestAddExpr :: Parser [Expression]
parseRestAddExpr (Plus : xs1)  = do
    (e, xs2)  <- parsePositiveMultExpr xs1
    (es, xs3) <- parseRestAddExpr xs2
    return (e:es, xs3)
parseRestAddExpr (Minus : xs1) = do
    (e, xs2)  <- parsePositiveMultExpr xs1
    (es, xs3) <- parseRestAddExpr xs2
    return (Neg e:es, xs3)
parseRestAddExpr xs            = return ([], xs)

parseMultExpr  :: Parser Expression
parseMultExpr  xs1 = do
    (e, xs2)  <- parseNegExpr xs1
    (es, xs3) <- parseRestMultExpr xs2
    return (foldl Mult e es, xs3)

parsePositiveMultExpr :: Parser Expression
parsePositiveMultExpr xs1 = do
    (e, xs2)  <- parseAtomicExpr  xs1
    (es, xs3) <- parseRestMultExpr xs2
    return (foldl Mult e es, xs3)

parseRestMultExpr :: Parser [Expression]
parseRestMultExpr (Times : xs1) = do
    (e, xs2)  <- parseAtomicExpr  xs1
    (es, xs3) <- parseRestMultExpr xs2
    return (e:es, xs3)
parseRestMultExpr (DivBy : xs1) = do
    (e, xs2)  <- parseAtomicExpr  xs1
    (es, xs3) <- parseRestMultExpr xs2
    return (NegExpo e:es, xs3)
parseRestMultExpr xs            = return ([], xs)

parseNegExpr :: Parser Expression
parseNegExpr (Minus : xs1) = do
    (e, xs2)  <- parseAtomicExpr  xs1
    return (Neg e, xs2)
parseNegExpr xs            = parseAtomicExpr  xs

parseAtomicExpr  :: Parser Expression
parseAtomicExpr  (Number i : xs1)  = return (Val i, xs1)
parseAtomicExpr  (Boolean i : xs1) = return (BoolVal i, xs1)
parseAtomicExpr  (OpenPar : xs1)   = do
    (e, xs2)  <- parseExpr xs1
    case xs2 of
        ClosePar : xs3 -> return (e, xs3)
        _              -> Left "Missing ')'"
parseAtomicExpr  (Name i : xs1)    = do
    (is, xs2) <- parseRestAtomicExpr xs1
    return (foldl Function (Variable i) is, xs2)
parseAtomicExpr  _                 = Left "Expected: number, boolean or variable"

parseRestAtomicExpr :: Parser [Expression]
parseRestAtomicExpr (Number i : xs1)  = do
    (is, xs2) <- parseRestAtomicExpr xs1
    return (Val i:is, xs2)
parseRestAtomicExpr (Boolean i : xs1) = do
    (is, xs2) <- parseRestAtomicExpr xs1
    return (BoolVal i:is, xs2)
parseRestAtomicExpr (OpenPar : xs1)   = do
    (e, xs2)  <- parseExpr xs1
    case xs2 of
        ClosePar : xs3 -> do
            (es, xs4) <- parseRestAtomicExpr xs3
            return (e:es, xs4)
        _              -> Left "Missing ')'"
parseRestAtomicExpr (Name i : xs1)    = do
    (is, xs2) <- parseRestAtomicExpr xs1
    return (Variable i:is, xs2)
parseRestAtomicExpr xs                = return ([], xs)

parseVariable :: Parser Expression
parseVariable (Name i : xs) = return (Variable i, xs)
parseVariable _             = Left "Expected: variable"

-- Beispiele: ParsePositiveMult, AtomicExpr: restAtomic nur noch in Name, Unäres - und /, Assoziativität von Minus und /!
