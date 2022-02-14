module Datatypes (CompilerException(..), Token(..), Program(..), Definition(..), 
LocDefs(..), LocDef(..), Expression(..), Parser(..), Instruction(..), 
Type(..), Value(..), State(..), Stack(..), Heap(..), Global(..), HeapCell(..), 
Op(..), CompilerState(..), EmulatorState(..), Result(..)) where
import Control.Exception (Exception, throw)

data CompilerException
    = InvalidName !String
    | WrongAddress
    | NoValueFound !String
    | MissingMain
    | TypeCheck !String
    | VariableNotInScope !String
    | ReturnAddressNotFound
    deriving Show

instance Exception CompilerException

data Token
    = Number Int
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
    | Comma
    | Equals
    deriving Eq

newtype Program = Program [Definition]

data Definition = Definition [Expression] Expression

newtype LocDefs = LocDefs [LocDef] deriving (Eq, Show)

data LocDef     = LocDef Expression Expression deriving (Eq, Show)

data Expression
    = LetX      LocDefs Expression
    | IfX       Expression Expression Expression
    | OrX       Expression Expression
    | AndX      Expression Expression
    | NotX      Expression
    | LessThanX Expression Expression
    | IsX       Expression Expression
    | Sum       Expression Expression
    | NegExpo   Expression -- x hoch minus 1 = 1/x
    | Mult      Expression Expression
    | Neg       Expression
    | Function  Expression Expression
    | Val       Int
    | BoolVal   Bool
    | Variable  String
    deriving (Eq, Show)

type Parser a = [Token] -> Either String (a, [Token])

data Instruction
    = Reset
    | Pushfun String
    | Pushval Type Value
    | Pushparam Int
    | Makeapp
    | Slide Int
    | Return
    | Halt
    | Unwind
    | Call
    | Pushpre Token
    | Updatefun Int
    | Updateop
    | Operator Op
    | Alloc
    | Updatelet Int
    | Slidelet Int
    deriving Eq

data Type  = Float | Bool deriving (Eq, Show)
type Value = Float

data State = State
    {
        pc     :: Int,
        code   :: [Instruction],
        stack  :: Stack,
        heap   :: Heap,
        global :: Global
    }

type Stack  = [String]
type Heap   = [HeapCell]
type Global = [(String, Int)]

data HeapCell
    = APP Int Int
    | DEF String Int Int
    | VAL Type Value
    | IND Int
    | PRE Token Op
    | UNINITIALIZED
    deriving Eq

data Op
    = UnaryOp
    | BinaryOp
    | IfOp
    deriving Eq

newtype CompilerState = CompilerState State

newtype EmulatorState = EmulatorState [State]

newtype Result = Result HeapCell
