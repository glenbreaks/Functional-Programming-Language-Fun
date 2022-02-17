module Emulator (showEmulate, emulate) where
import Datatypes
import Compiler
import Show

----------------- functions for nice outputs of emulator:


showEmulate :: String -> IO()
showEmulate xs = case compile xs of
                    Left x -> putStrLn ("*** " ++ x)
                    Right x ->
                        case showRun x of
                            Left x  -> putStrLn ("*** " ++ x)
                            Right x -> putStr $ show (EmulatorState x)

showRun :: State -> Either String [State]
showRun s@State{pc = pc, code = code, stack = stack, heap = heap, global = global} =
    if not (mainInGlobal global) then Left "Missing main" else return (hshowRun s)
        where hshowRun s@State{pc = pc, code = code, stack = stack, heap = heap, global = global} =
                let i = code!!pc in
                    if i /= Halt then
                        case runPC i pc stack heap of
                            Left x -> [s]
                            Right xpc ->
                                case runStack i pc stack heap global of
                                    Left x -> [s]
                                    Right xstack ->
                                        case runHeap i stack heap of
                                            Left x -> [s]
                                            Right xheap -> s:hshowRun(s { pc    = xpc
                                                                        , stack = xstack
                                                                        , heap  = xheap })
                                else [s]

----------------- Emulator:

-- bsp zeigen, emulate bsp, showEmulate bsp

emulate :: String -> IO()
emulate xs =
    case compile xs of
        Left x  -> putStrLn ("*** " ++ x)
        Right x -> 
            case run x of
                Left x  -> putStrLn ("*** " ++ x)
                Right x -> putStr $ show (result x)

result :: State -> Result
result (State _ _ s h _) = Result (val (h!!getAddress (last s)) h)

run :: State -> Either String State
run s@State{pc = pc, code = code, stack = stack, heap = heap, global = global} =
    if not (mainInGlobal global) then Left "Missing main" else
        let i = code!!pc in
            if i /= Halt then
                case runPC i pc stack heap of
                    Left x -> Left x
                    Right xpc ->
                        case runStack i pc stack heap global of
                            Left x -> Left x
                            Right xstack ->
                                case runHeap i stack heap of
                                    Left x -> Left x
                                    Right xheap -> run s { pc    = xpc
                                                         , stack = xstack
                                                         , heap  = xheap }
                         else return s

-- p = pc
-- s = stack
-- h = heap
-- g = global

runPC :: Instruction -> Int -> [String] -> [HeapCell] -> Either String Int
runPC Unwind p s h =
    case val (h!!getAddress (last s)) h of
        APP _ _ -> return p
        _       -> return (p+1)
runPC Call   p s h =
    case val (h!!getAddress (last s)) h of
        DEF _ _ adr    -> return adr
        PRE _ BinaryOp -> return 4
        PRE _ IfOp     -> return 13
        PRE _ UnaryOp  -> return 21
        _              -> return (p+1)
runPC Return p s _ = if head (s!!(length s-2)) == 'c' then return (getAddress (s!!(length s-2))) else Left "Runtime error"
runPC _      p _ _ = return (p+1)

runStack :: Instruction -> Int -> [String] -> [HeapCell] -> [(String, Int)] -> Either String [String]
runStack (Pushfun arg)   _ s _ g =
    case address arg g of
        Left x  -> Left x
        Right x -> return (s ++ ["h" ++ show x])
runStack (Pushval t v)   _ s h _ = return (s ++ ["h" ++ show (length h)]) -- new
runStack (Pushparam arg) _ s h _ =
    case add2arg (s!!(length s-arg-2)) h of
        Left x  -> Left x
        Right x -> return (s ++ ["h" ++ show x])
runStack Makeapp         _ s h _ = return (init (init s) ++ ["h" ++ show (length h)])
runStack (Slide arg)     _ s _ _ = return (slider (length s-arg-2) s 0 ++ [s!!(length s-2), s!!(length s-1)])
runStack Unwind          _ s h _ =
    case val (h!!getAddress (last s)) h of
        APP x _ -> return (s ++ ["h" ++ show x])
        _       -> return s
runStack Call            p s h _ =
    case val (h!!getAddress (last s)) h of
        DEF {}  -> return (s ++ ["c" ++ show (p+1)])
        PRE _ _ -> return (s ++ ["c" ++ show (p+1)])
        _       -> return s
runStack Return          _ s _ _ = return (init (init s) ++ [last s])
runStack (Pushpre op)    _ s h _ = return (s ++ ["h" ++ show (length h)])
runStack Updateop        _ s h _ = return (init (init $ init s) ++ [s!!(length s - 2), s!!(length s - 3)])
runStack (Operator op)   _ s h _ =
    case op of
        UnaryOp  -> return (init (init $ init s) ++ [s!!(length s-2), "h" ++ show (length h)])
        BinaryOp -> return (init (init $ init $ init $ init s) ++ [s!!(length s-3), "h" ++ show (length h)])
        _        -> let a = val (h!!getAddress (last s)) h
                    in case a of
                            VAL Bool 1 ->
                                case add2arg(s!!(length s-5)) h of
                                    Left x  -> Left x
                                    Right x -> return (init (init $ init $ init $ init s) ++ [s!!(length s-2)] ++ ["h" ++ show x])
                            VAL Bool _ ->
                                case add2arg(s!!(length s-6)) h of
                                    Left x  -> Left x
                                    Right x -> return (init (init $ init $ init $ init s) ++ [s!!(length s-2)] ++ ["h" ++ show x])
                            _          -> Left "Runtime error"
runStack Alloc           _ s h _ = return (s ++ ["h" ++ show (length h)])
runStack (Updatelet _)   _ s h _ = return (init s)
runStack (Slidelet arg)  _ s _ _ = return (slider (length s-arg-1) s 0 ++ [last s])
runStack _               _ s _ _ = return s

runHeap :: Instruction -> [String] -> [HeapCell] -> Either String [HeapCell]
runHeap (Pushval t v) _ h = return (h ++ [VAL t v])
runHeap Makeapp       s h = return (h ++ [APP (getAddress (last s))  (getAddress (s!!(length s-2)))])
runHeap (Pushpre op)  _ h = return (h ++ [PRE op (arity op)])
runHeap Updateop      s h = return (insert1 (getAddress (s!!(length s-3))) 0 h ++ [h!!getAddress (last s)] ++ insert2 (getAddress (s!!(length s-3))) h)
runHeap (Updatefun f) s h = return (insert1 (getAddress (s!!(length s-f-3))) 0 h ++ [IND (getAddress (last s))] ++ insert2 (getAddress (s!!(length s-f-3))) h)
runHeap (Operator op) s h =
    case op of
        UnaryOp  -> let a = val (h!!getAddress (s!!(length s-3))) h
                        b = val (h!!getAddress (last s)) h
                    in
                        case a of
                            PRE op _ ->
                                case compute op b UNINITIALIZED of
                                    Left x  -> Left x
                                    Right x -> return (h ++ [VAL (findType op) x]) -- zum Aufruf compute
                            _        -> Left "Runtime error"
        BinaryOp -> let a = val (h!!getAddress (s!!(length s-4))) h
                        b = val (h!!getAddress (s!!(length s-2))) h
                        c = val (h!!getAddress (last s)) h
                    in
                        case a of
                            PRE op _ ->
                                case compute op b c of
                                    Left x  -> Left x
                                    Right x -> return (h ++ [VAL (findType op) x])
                            _        -> Left "Runtime error"
        _        -> return h
runHeap Alloc         _ h = return (h ++ [UNINITIALIZED])
runHeap (Updatelet n) s h =
    case add2arg (s!!(length s-n-2)) h of
        Left x  -> Left x
        Right x -> return (insert1 x 0 h ++ [IND (getAddress (last s))] ++ insert2 x h)
runHeap _             _ h = return h

compute :: Token -> HeapCell -> HeapCell -> Either String Value
compute Not      (VAL Bool x) _ | x == 0    = return 1
                                | otherwise = return 0
compute Minus    (VAL Float x) _ = return (- x)
compute DivBy    (VAL Float x) _ = return (1 / x)
compute LessThan (VAL Float x) (VAL Float y) | x < y     = return 1
                                             | otherwise = return 0
compute Is       (VAL Float x) (VAL Float y) | x == y    = return 1
                                             | otherwise = return 0
compute Is       (VAL Bool x)  (VAL Bool y)  | x == y    = return 1
                                             | otherwise = return 0
compute Plus     (VAL Float x) (VAL Float y) = return (x + y)
compute Times    (VAL Float x) (VAL Float y) = return (x * y)
compute Expo     (VAL Float x) (VAL Float y) | floor y /= ceiling y = Left "Integer expected in exponent"
                                             | y > 0                = return (x ^ round y)
                                             | y < 0                = return (1 / x ^ round (-y))
                                             | otherwise            = return 1

compute _ (VAL Bool x) (VAL Float y) = Left "Mismatched types"
compute _ (VAL Float x) (VAL Bool y) = Left "Mismatched types"
compute _ _             _            = Left "Mismatched types"


--- Hilfsfunktionen Emulator:

-- sucht in der globalen Umgebung nach einem Funktionsnamen und liefert die HeapAdresse dieser Definition
address :: String -> [(String, Int)] -> Either String Int
address arg ((x, y):xs)
    | x == arg  = return y
    | otherwise = address arg xs
address arg []    = Left ("Variable '" ++ arg ++ "' not in scope")     -- Fehler wird z.B. bei f a b = x; geworfen 

-- liefert das 2. Argument einer APP-Zelle an einer bestimmten Heap-Adresse
add2arg :: String -> [HeapCell] -> Either String Int
add2arg adr h =
    case val (h!!getAddress adr) h of
        APP _ x -> return x
        _       -> Left "Runtime error"

-- liefert den Typ einer VAL-Zelle an einer bestimmten Heap-Adresse
typ :: Int -> [HeapCell] -> Type
typ adr h =
    case val (h!!adr) h of
        VAL Float _ -> Float
        _           -> Bool

-- liefert den Wert einer IND-Zelle an einer bestimmten Heap-Adresse
val :: HeapCell -> [HeapCell] -> HeapCell
val x h =
    case x of
        IND adr -> val (h!!adr) h
        _       -> x

slider :: Int -> [String] -> Int -> [String]
slider n s akk | n > 0     = s!!akk : slider (n-1) s (akk+1)
               | otherwise = []

findType :: Token -> Type
findType Plus  = Float
findType Minus = Float
findType Times = Float
findType DivBy = Float
findType Expo  = Float
findType _     = Bool

-- prüft, ob in Global eine main Funktion vorhanden ist                     
mainInGlobal :: [([Char], b)] -> Bool
mainInGlobal (("main",_):xs) = True
mainInGlobal (x:xs)          = mainInGlobal xs
mainInGlobal []              = False

insert1 :: Int -> Int -> [HeapCell] -> [HeapCell]
insert1 adr akk h | adr > 0          = h!!akk : insert1 (adr-1) (akk+1) h
                  | otherwise        = []

insert2 :: Int -> [HeapCell]-> [HeapCell]
insert2 adr h   | adr < length h-1  = h!!(adr+1)  : insert2 (adr+1) h
                | otherwise         = []

arity :: Token -> Op
arity If    = IfOp
arity Not   = UnaryOp
arity Minus = UnaryOp
arity DivBy = UnaryOp
arity _     = BinaryOp

getAddress :: String -> Int
getAddress xs = read (tail xs) :: Int

-- Beispiele: showRun mit allen States, showEmulate, Fehler erzeugen, Stack aus Strings!!!
