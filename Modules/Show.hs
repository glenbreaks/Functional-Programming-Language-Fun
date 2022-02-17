{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Show (showDef, showStack, showHeap, showBoxed, Show) where

import Datatypes
---------- Show-Funktionen:

showDef :: Definition -> String
showDef (Definition (Variable a : _)_) = a

showStack :: [String] -> Int -> String
showStack [] _ = ""
showStack xs n = hshowStack xs n 0
    where hshowStack (x:xs) n akk = "s" ++ show akk ++ ":" ++ indent (n - length (show akk)) ++ x ++ "\n" ++ hshowStack xs n (akk+1)
          hshowStack []     _ _   = ""

showHeap :: [HeapCell] -> Int -> String
showHeap [] _ = ""
showHeap xs n = hshowHeap xs n 0
    where hshowHeap (x:xs) n akk = "h" ++ show akk ++ ":" ++ indent (n - length (show akk)) ++ show x ++ "\n" ++ hshowHeap xs n (akk+1)
          hshowHeap []     _ _   = ""
showGlobal :: [(String, Int)] -> String
showGlobal ((x, y):xs)        = "h" ++ show y ++ ":" ++ indent (4 - length (show y)) ++ x ++ "\n" ++ showGlobal xs
showGlobal []                 = ""

showBoxed :: String -> String
showBoxed xs = "\n" ++ dots (length xs+4) ++ "\n: " ++ xs ++ " :\n" ++ dots (length xs+4)
    where dots 0 = ""
          dots n = "·" ++ dots (n-1)

indent :: Int -> String
indent 0                      = ""
indent n                      = " " ++ indent (n-1)

columns :: Int -> [String] -> [HeapCell] -> String
columns pc stack (h:heap) =
    case stack of
        (s:stack)  -> "T:  " ++ show (length stack) ++ indent (15-length (show (length stack)))
                                   ++ "s0: " ++ indent 2 ++ s ++ indent (15-length s)
                                   ++ "h0: "++ indent 2 ++ show h
                             ++ "\n" ++ hcolumns 1 [pc] stack heap
        _          -> "T:  " ++ show (length stack-1) ++ indent (15-length (show (length stack-1)))          -- stack-1?
                            ++ indent 21 --    ++ "s0: " ++ indent 2 ++ s ++ indent (15-length stack)
                            ++ "h0: "++ indent 2 ++ show h
                ++ "\n" ++ hcolumns 1 [pc] stack heap
    where hcolumns akk [x] (y:ys) (z:zs) = "PC: " ++ show x ++ indent (15-length (show x))
                                           ++ "s" ++ show akk ++ ":" ++ indent (4-length (show akk)) ++ y ++ indent (15-length y)
                                           ++ "h" ++ show akk ++ ":" ++ indent (4-length (show akk)) ++ show z
                                           ++ "\n" ++ hcolumns (akk+1) [] ys zs
          hcolumns akk [x] (y:ys) []     = "PC: " ++ show x ++ indent (15-length (show x))
                                           ++  "s" ++ show akk ++ ":" ++ indent (4-length (show akk)) ++ y
                                           ++ "\n" ++ hcolumns (akk+1) [] ys []
          hcolumns akk [x] []     (z:zs) = "PC: " ++ show x ++ indent (15-length (show x))
                                           ++ indent 21
                                           ++ "h" ++ show akk ++ ":" ++ indent (4-length (show akk)) ++ show z
                                           ++ "\n" ++ hcolumns (akk+1) [] [] zs
          hcolumns akk [x] []     []     = "PC: " ++ show x ++ indent (15-length (show x)) ++ "\n"
          hcolumns akk []  (y:ys) (z:zs) = indent 19
                                           ++ "s" ++ show akk ++ ":" ++ indent (4-length (show akk)) ++ y ++ indent (15-length y)
                                           ++ "h" ++ show akk ++ ":" ++ indent (4-length (show akk)) ++ show z
                                           ++ "\n" ++ hcolumns (akk+1) [] ys zs
          hcolumns akk []  (y:ys) []     = indent 19
                                           ++ "s" ++ show akk ++ ":" ++ indent (4-length (show akk)) ++ y
                                           ++ "\n" ++ hcolumns (akk+1) [] ys []
          hcolumns akk []  []     (z:zs) = indent 40
                                           ++ "h" ++ show akk ++ ":" ++ indent (4-length (show akk)) ++ show z
                                           ++ "\n" ++ hcolumns (akk+1) [] [] zs
          hcolumns _   _   _      _      = ""
columns _ _ _ = ""


result :: State -> Result
result (State _ _ s h _) = Result (val (h!!getAddress (last s)) h)

getAddress :: String -> Int
getAddress xs = read (tail xs) :: Int

val :: HeapCell -> [HeapCell] -> HeapCell
val x h =
    case x of
        IND adr -> val (h!!adr) h
        _       -> x

instance Show Token
    where
        show Or              = "|"
        show And             = "&"
        show Not             = "not"
        show LessThan        = "<"
        show Is              = "=="
        show Minus           = "-"
        show Plus            = "+"
        show DivBy           = "/"
        show Times           = "*"
        show Expo            = "^"
        show OpenPar         = "("
        show ClosePar        = ")"
        show Let             = "let"
        show In              = "in"
        show If              = "if"
        show Then            = "then"
        show Else            = "else"
        show Semicolon       = ";"
        show Comma           = ","
        show Equals          = "="
        show (Boolean True)  = "true"
        show (Boolean False) = "false"
        show (Number x)      = show x
        show (Name   x)      = x

instance Show Program
    where show (Program xs) = show xs

instance Show Definition
    where show (Definition ((Variable fun):args) expr) = "Definition " ++ fun ++ indent (abs (15-length fun)) ++ show (hshowArgs args) ++ indent (abs (15-length (show (hshowArgs args)))) ++ "(" ++ show expr ++ ")\n"
           where hshowArgs ((Variable x):xs) = x:hshowArgs xs
                 hshowArgs []                = []

instance Show Instruction
    where show Reset              = "Reset"
          show (Pushfun fun)      = "Pushfun " ++ fun
          show (Pushval Bool v)
                      | v == 0    = "Pushval Bool false"
                      | otherwise = "Pushval Bool true"
          show (Pushval t v)      = "Pushval " ++ show t ++ " " ++ show v
          show (Pushparam i)      = "Pushparam " ++ show i
          show Makeapp            = "Makeapp"
          show (Slide i)          = "Slide " ++ show i
          show Return             = "Return"
          show Halt               = "Halt"
          show Unwind             = "Unwind"
          show Call               = "Call"
          show (Pushpre DivBy)    = "Pushpre 1/"
          show (Pushpre x)        = "Pushpre " ++ show x
          show (Updatefun i)      = "Update " ++ show i
          show Updateop           = "Update op"
          show (Operator o)       = "Operator " ++ show o
          show Alloc              = "Alloc"
          show (Updatelet i)      = "Updatelet " ++ show i
          show (Slidelet i)       = "Slidelet " ++ show i

instance Show HeapCell
    where show (APP a b  )   = "APP h" ++ show a ++ " h" ++ show b
          show (DEF f x y)   = "DEF " ++ f ++ " " ++ show x ++ " c" ++ show y
          show (VAL t v  )   = "VAL " ++ show t ++ " " ++ show v
          show (IND x    )   = "IND h" ++ show x
          show (PRE t op )   = "PRE " ++ show t ++ " " ++ show op
          show UNINITIALIZED = ""

instance Show State
    where show (State pc code stack heap global) = show code ++ "\n" ++ show heap ++ "\n" ++ show global 


instance Show Op
    where show UnaryOp  = "1"
          show BinaryOp = "2"
          show IfOp     = "3"

instance Show CompilerState
    where show (CompilerState (State _ code stack heap global)) = "\n\n················\n: Instructions :\n················\n" ++ showCode code 0
                                               ++ "\n········\n: Heap :\n········\n"                           ++ showHeap heap 4
                                               ++ "\n··········\n: Global :\n··········\n"                     ++ showGlobal global
                                               ++ "\n\n"
            where
                showCode (x:xs) 4             = "\n· binary operation ·\nc4:   " ++ show x ++ "\n" ++ showCode xs 5
                showCode (x:xs) 13            = "\n· if operation ·\nc13:  "     ++ show x ++ "\n" ++ showCode xs 14
                showCode (x:xs) 21            = "\n· unary operation ·\nc21:  "  ++ show x ++ "\n" ++ showCode xs 22
                showCode [Return] akk         = "c" ++ show akk ++ ":" ++ indent (4 - length (show akk)) ++ "Return\n"
                showCode (Return:xs) akk
                    | akk /= 12 && akk /=20   = "c" ++ show akk ++ ":" ++ indent (4 - length (show akk)) ++ "Return\n\n· " ++ name (akk+1) heap ++ " ·\n" ++ showCode xs (akk+1)
                    | otherwise               = "c" ++ show akk ++ ":" ++ indent (4 - length (show akk)) ++ "Return\n" ++ showCode xs (akk+1)
                showCode (x:xs) akk           = "c" ++ show akk ++ ":" ++ indent (4 - length (show akk)) ++ show x ++ "\n" ++ showCode xs (akk+1)
                showCode [] _                 = ""
                name n (x:xs)                 =
                    case x of
                        DEF f _ adr -> if n == adr then f else name n xs
                        _           -> ""
                name _ []                     = ""

instance Show EmulatorState
    where show (EmulatorState (s1@State{pc=pc1, code=code}:s2@State{pc=pc2, stack=stack, heap=heap, global=global}:xs)) =
                showBoxed (show (code!!pc1)) ++ "\n\n" ++ columns pc2 stack heap ++ show (EmulatorState (s2:xs))
          show (EmulatorState [s@State{pc=pc, code=code, stack=stack, heap=heap, global=global}]) =
                showBoxed (show (code!!pc))  ++ "\n\n" ++ if pc == 3 then show (result s) else "Runtime error\n"
          show (EmulatorState _) = ""

instance Show Result
    where show (Result x) = "\n>>> Result: " ++
            case x of
                VAL Bool 1 -> "true\n\n"
                VAL Bool _ -> "false\n\n"
                VAL _    a -> show a     ++ "\n\n"
                _          -> show x     ++ "\n\n"
