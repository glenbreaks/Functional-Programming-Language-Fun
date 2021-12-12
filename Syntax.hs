Program         ::= Def ";" RestProgram
RestProgram     ::= e | Def ";" RestProgram
Def             ::= Variable Restdef Expr
RestDef         ::= "=" | Variable Restdef
LocDefs         ::= LocDef RestLocDefs
RestLocDefs     ::= e | ";" LocDef RestLocDefs
LocDef          ::= Variable "=" Expr
Expr            ::= "let" LocDefs "in" Expr
                  | "if" Expr "then" Expr "else" Expr         
                  | OrExpr
OrExpr          ::= AndExpr ["|" OrExpr]
AndExpr         ::= NotExpr ["&" AndExpr]
NotExpr         ::= ["not"] CompareExpr
CompareExpr     ::= AddExpr [("<" | "==") AddExpr]
AddExpr         ::= MultExpr ["-" MultExpr2 | "+" RestAddExpr]
RestAddExpr     ::= MultExpr2 ["+" RestAddExpr]
MultExpr        ::= NegExpr ["/" AtomicExpr | "*" RestMultExpr]
-- MultExpr2 is used after an operator instead of MultExpr to forbid
-- the input of negation directly after an operator without parentheses
MultExpr2       ::= AtomicExpr ["/" AtomicExpr | "*" RestMultExpr]
RestMultExpr    ::= AtomicExpr ["*" RestMultExpr]
NegExpr         ::= ["-"] AtomicExpr
AtomicExpr      ::= (Number | Boolean | "(" Expr ")" | Variable) RestAtomicExpr
RestAtomicExpr  ::= e | (Number | Boolean | "(" Expr ")" | Variable) RestAtomicExpr
Variable        ::= Name