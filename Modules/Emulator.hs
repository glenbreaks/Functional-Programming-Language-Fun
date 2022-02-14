module Emulator (showEmulate, emulate) where
import Datatypes
import Compiler
import Show
import Control.Exception (Exception, throw)

----------------- functions for nice outputs of emulator:


showEmulate :: String -> IO()
showEmulate xs = case compile xs of
                    Left x -> putStr x
                    Right x -> putStr $ show (EmulatorState (showRun x))

showRun :: State -> [State]
showRun s@State{pc = pc, code = code, stack = stack, heap = heap, global = global} =
    if not (mainInGlobal global) then throw MissingMain else
        let i = code!!pc in
            if i /= Halt then
                    s:showRun(s { pc    = runPC i pc stack heap
                                , stack = runStack i pc stack heap global
                                , heap  = runHeap i stack heap })
                         else [s]

----------------- Emulator:

-- bsp zeigen, emulate bsp, showEmulate bsp

emulate :: String -> IO()
emulate xs =
    case compile xs of
        Right x -> putStr $ show (result (run x))
        Left x  -> putStr x

result :: State -> Result
result (State _ _ s h _) = Result (val (h!!getAddress (last s)) h)

run :: State -> State
run s@State{pc = pc, code = code, stack = stack, heap = heap, global = global} =
    if not (mainInGlobal global) then throw MissingMain else
        let i = code!!pc in
            if i /= Halt then
                           run s { pc    = runPC i pc stack heap
                                 , stack = runStack i pc stack heap global
                                 , heap  = runHeap i stack heap }
                         else s

-- p = pc
-- s = stack
-- h = heap
-- g = global

runPC :: Instruction -> Int -> [String] -> [HeapCell] -> Int
runPC Unwind p s h =
    case val (h!!getAddress (last s)) h of
        APP _ _ -> p
        _       -> p+1
runPC Call   p s h =
    case val (h!!getAddress (last s)) h of
        DEF _ _ adr    -> adr
        PRE _ BinaryOp -> 4
        PRE _ IfOp     -> 13
        PRE _ UnaryOp  -> 21
        _              -> p+1
runPC Return p s _ = if head (s!!(length s-2)) == 'c' then getAddress (s!!(length s-2)) else throw ReturnAddressNotFound--c-Addr!
runPC _      p _ _ = p+1

runStack :: Instruction -> Int -> [String] -> [HeapCell] -> [(String, Int)] -> [String]
runStack (Pushfun arg)   _ s _ g = s ++ ["h" ++ show (address arg g)]
runStack (Pushval t v)   _ s h _ = s ++ ["h" ++ show (length h)] -- new
runStack (Pushparam arg) _ s h _ = s ++ ["h" ++ show (add2arg (s!!(length s-arg-2)) h)]
runStack Makeapp         _ s h _ = init (init s) ++ ["h" ++ show (length h)]
runStack (Slide arg)     _ s _ _ = slider (length s-arg-2) s 0 ++ [s!!(length s-2), s!!(length s-1)]
runStack Unwind          _ s h _ =
    case val (h!!getAddress (last s)) h of
        APP x _ -> s ++ ["h" ++ show x]
        _       -> s
runStack Call            p s h _ =
    case val (h!!getAddress (last s)) h of
        DEF {}  -> s ++ ["c" ++ show (p+1)]
        PRE _ _ -> s ++ ["c" ++ show (p+1)]
        _       -> s
runStack Return          _ s _ _ = init (init s) ++ [last s]
runStack (Pushpre op)    _ s h _ = s ++ ["h" ++ show (length h)]
runStack Updateop        _ s h _ = init (init $ init s) ++ [s!!(length s - 2), s!!(length s - 3)]
runStack (Operator op)   _ s h _ =
    case op of
        UnaryOp  -> init (init $ init s) ++ [s!!(length s-2), "h" ++ show (length h)]
        BinaryOp -> init (init $ init $ init $ init s) ++ [s!!(length s-3), "h" ++ show (length h)]
        _        -> let a = val (h!!getAddress (last s)) h
                    in init (init $ init $ init $ init s) ++ [s!!(length s-2)] ++
                        case a of
                            VAL Bool 1 -> ["h" ++ show (add2arg(s!!(length s-5)) h)]
                            VAL Bool _ -> ["h" ++ show (add2arg(s!!(length s-6)) h)]
                            _          -> throw (NoValueFound (show op ++ show a))
runStack Alloc           _ s h _ = s ++ ["h" ++ show (length h)]
runStack (Updatelet _)   _ s h _ = init s
runStack (Slidelet arg)  _ s _ _ = slider (length s-arg-1) s 0 ++ [last s]
runStack _               _ s _ _ = s

runHeap :: Instruction -> [String] -> [HeapCell] -> [HeapCell]
runHeap (Pushval t v) _ h = h ++ [VAL t v] --getAddress hier überall für h-Zellen
runHeap Makeapp       s h = h ++ [APP (getAddress (last s))  (getAddress (s!!(length s-2)))]
runHeap (Pushpre op)  _ h = h ++ [PRE op (arity op)]
runHeap Updateop      s h = insert1 (getAddress (s!!(length s-3))) 0 h ++ [h!!getAddress (last s)] ++ insert2 (getAddress (s!!(length s-3))) h
runHeap (Updatefun f) s h = insert1 (getAddress (s!!(length s-f-3))) 0 h ++ [IND (getAddress (last s))] ++ insert2 (getAddress (s!!(length s-f-3))) h
runHeap (Operator op) s h =
    case op of
        UnaryOp  -> let a = val (h!!getAddress (s!!(length s-3))) h
                        b = val (h!!getAddress (last s)) h
                    in
                        case a of
                            PRE op _ -> h ++ [VAL (findType op) (compute op b UNINITIALIZED)] -- zum Aufruf compute
                            _        -> throw (NoValueFound (show a ++ show s))
        BinaryOp -> let a = val (h!!getAddress (s!!(length s-4))) h
                        b = val (h!!getAddress (s!!(length s-2))) h
                        c = val (h!!getAddress (last s)) h
                    in
                        case a of
                            PRE op _ -> h ++ [VAL (findType op) (compute op b c)]
                            _        -> throw (NoValueFound (show c ++ show s))
        _        -> h
runHeap Alloc         _ h = h ++ [UNINITIALIZED]
runHeap (Updatelet n) s h = insert1 (add2arg (s!!(length s-n-2)) h) 0 h ++ [IND (getAddress (last s))] ++ insert2 (add2arg (s!!(length s-n-2)) h) h
runHeap _             _ h = h

compute :: Token -> HeapCell -> HeapCell -> Value
compute Not      (VAL Bool x) _ | x == 0    = 1
                                | otherwise = 0
compute Minus    (VAL Float x) _ = - x
compute DivBy    (VAL Float x) _ = 1 / x
compute LessThan (VAL Float x) (VAL Float y) | x < y     = 1
                     | otherwise = 0
compute Is       (VAL Float x) (VAL Float y) | x == y    = 1
                                             | otherwise = 0
compute Is       (VAL Bool x)  (VAL Bool y)  | x == y    = 1
                                             | otherwise = 0
compute Plus     (VAL Float x) (VAL Float y) = x + y
compute Times    (VAL Float x) (VAL Float y) = x * y
compute _ (VAL Bool x) (VAL Float y) = throw (TypeCheck "Mismatched Types")
compute _ (VAL Float x) (VAL Bool y) = throw (TypeCheck "Mismatched Types")
compute _ _ _ = throw (TypeCheck "Couldn't match type")


--- Hilfsfunktionen Emulator:

-- sucht in der globalen Umgebung nach einem Funktionsnamen und liefert die HeapAdresse dieser Definition
address :: String -> [(String, Int)] -> Int
address arg ((x, y):xs)
    | x == arg  = y
    | otherwise = address arg xs
address arg []    = throw (VariableNotInScope arg)     -- Fehler wird z.B. bei f a b = x; geworfen 

-- liefert das 2. Argument einer APP-Zelle an einer bestimmten Heap-Adresse
add2arg :: String -> [HeapCell] -> Int
add2arg adr h =
    case val (h!!getAddress adr) h of
        APP _ x -> x
        _       -> throw WrongAddress

-- liefert den Typ einer VAL-Zelle an einer bestimmten Heap-Adresse
typ :: Int -> [HeapCell] -> Type
typ adr h =
    case val (h!!adr) h of
        VAL Float _ -> Float
        _              -> Bool

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
