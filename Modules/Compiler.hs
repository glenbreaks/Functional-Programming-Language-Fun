{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Compiler (showCompile, compile) where
import Datatypes
import Parser
import Show
import GHC.Float (int2Float)

showCompile :: String -> IO()
showCompile xs =
    case compile xs of
        Left x  -> putStrLn ("*** " ++ x)
        Right x -> putStr (show (CompilerState x))

compile :: String -> Either String State
compile xs =
    case parse xs of
        Left x                 -> Left x
        Right (Program xs, []) -> return (compileProgram xs)

compileProgram :: [Definition] -> State
compileProgram = foldl compileDef State{pc=0, code=initCode, stack=[], heap=[], global=[]}
  where
    compileDef s@State{code = code, heap = heap, global = global} def@(Definition (Variable fun:args) body) =
      s { code   = code ++ codeDef def
        , heap   = heap ++ [DEF fun (length args) (length code)]
        , global = global ++ [(fun, length heap)] }

codeDef :: Definition -> [Instruction]
codeDef (Definition (Variable fun:args) body) = codeExpr body (buildEnv args) ++ [Updatefun n, Slide (n + 1), Unwind, Call, Return]
    where
        n = length args

codeExpr :: Expression -> [(Expression, Int)] -> [Instruction]
codeExpr (LetX      (LocDefs a) b)   env = codeLocDefs a envLet ++ codeExpr b envLet ++ [Slidelet n]
    where
        n = length a
        envLet = [(v, pos-1) | (v, pos) <- buildEnvLet a] ++ [(v, pos+n) | (v, pos) <- env]
codeExpr (IfX                a  b c) env = codeExpr c env ++ codeExpr b [(v, pos+1) | (v, pos) <- env] ++ codeExpr a [(v, pos+2) | (v, pos) <- env] ++ [Pushpre If, Makeapp, Makeapp, Makeapp]
codeExpr (NotX               a)      env = codeExpr a env ++ [Pushpre Not, Makeapp]
codeExpr (Neg                a)      env = codeExpr a env ++ [Pushpre Minus, Makeapp]
codeExpr (NegExpo            a)      env = codeExpr a env ++ [Pushpre DivBy, Makeapp] -- Token DivBy used here because there is no Token for NegExpo
codeExpr (OrX                a  b)   env = codeExpr (IfX a   (BoolVal True) b)  env
codeExpr (AndX               a  b)   env = codeExpr (IfX a b (BoolVal False))   env
codeExpr (LessThanX          a  b)   env = codeExpr b env ++ codeExpr a [(v, pos+1) | (v, pos) <- env] ++ [Pushpre LessThan, Makeapp, Makeapp]
codeExpr (IsX                a  b)   env = codeExpr b env ++ codeExpr a [(v, pos+1) | (v, pos) <- env] ++ [Pushpre Is, Makeapp, Makeapp]
codeExpr (Sum                a  b)   env = codeExpr b env ++ codeExpr a [(v, pos+1) | (v, pos) <- env] ++ [Pushpre Plus, Makeapp, Makeapp]
codeExpr (Mult               a  b)   env = codeExpr b env ++ codeExpr a [(v, pos+1) | (v, pos) <- env] ++ [Pushpre Times, Makeapp, Makeapp]
codeExpr (ExpoX              a  b)   env = codeExpr b env ++ codeExpr a [(v, pos+1) | (v, pos) <- env] ++ [Pushpre Expo, Makeapp, Makeapp]
codeExpr (Function           a  b)   env = codeExpr b env ++ codeExpr a [(v, pos+1) | (v, pos) <- env] ++ [Makeapp]
codeExpr (Val                a)      env = [Pushval Float (int2Float a)]
codeExpr (BoolVal            a)      env = [Pushval Bool x]
    where x | a         = 1
            | otherwise = 0
codeExpr (Variable           a)      env =
    case pos (Variable a) env of
        Nothing -> [Pushfun a]
        Just x  -> [Pushparam x]

codeLocDefs :: [LocDef] -> [(Expression, Int)] -> [Instruction]
codeLocDefs x env = alloc n ++ cLocDefs x env n
    where
        n                                   = length x
        alloc 0                             = []
        alloc x                             = [Alloc, Alloc, Makeapp] ++ alloc (x-1)
        cLocDefs ((LocDef _ expr):xs) env n = codeExpr expr env ++ [Updatelet (n-1)] ++ cLocDefs xs env (n-1)
        cLocDefs []                   _   _ = []

--- support functions:

initCode :: [Instruction]
initCode = [Reset, Pushfun "main", Call, Halt]
    ++ [Pushparam 1, Unwind, Call, Pushparam 3, Unwind, Call, Operator BinaryOp, Updateop, Return]
    ++ [Pushparam 1, Unwind, Call, Operator IfOp, Updateop, Unwind, Call, Return] 
    ++ [Pushparam 1, Unwind, Call, Operator UnaryOp, Updateop, Return]

buildEnv :: [Expression] -> [(Expression, Int)]
buildEnv xs = hpos xs 1
    where
        hpos [] _ = []
        hpos (x:xs) akk = (x, akk) : hpos xs (akk + 1)

buildEnvLet :: [LocDef] -> [(Expression, Int)]
buildEnvLet xs = hposL xs n
    where
        n = length xs
        hposL ((LocDef var _):xs) n = (var, n-1) : hposL xs (n-1)
        hposL []                  n = []

pos :: Expression -> [(Expression, Int)] -> Maybe Int
pos _ []                      = Nothing
pos s ((x, i):xs) | s == x    = return i
                  | otherwise = pos s xs
