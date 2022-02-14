Program          ::= Def ";" RestProgram
RestProgram      ::= e | Def ";" RestProgram
Def              ::= Variable Restdef Expr
RestDef          ::= "=" | Variable Restdef
LocDefs          ::= LocDef RestLocDefs
RestLocDefs      ::= e | "," LocDef RestLocDefs
LocDef           ::= Variable "=" Expr
Expr             ::= "let" LocDefs "in" Expr
                  | "if" Expr "then" Expr "else" Expr         
                  | OrExpr
OrExpr           ::= AndExpr ["|" OrExpr]
AndExpr          ::= NotExpr ["&" AndExpr]
NotExpr          ::= ["not"] CompareExpr
CompareExpr      ::= AddExpr [("<" | "==") AddExpr]
AddExpr          ::= MultExpr ["-" PositiveMultExpr | RestAddExpr]
RestAddExpr      ::= e | "+" PositiveMultExpr RestAddExpr
MultExpr         ::= NegExpr ["/" AtomicExpr | RestMultExpr]
-- PositiveMultExpr is used after an operator instead of MultExpr to forbid
-- the input of negation directly after an operator without parentheses
PositiveMultExpr ::= AtomicExpr ["/" AtomicExpr | RestMultExpr]
RestMultExpr     ::= e | "*" AtomicExpr RestMultExpr
NegExpr          ::= ["-"] AtomicExpr
AtomicExpr       ::= (Number | Boolean | "(" Expr ")" | Variable) RestAtomicExpr
RestAtomicExpr   ::= e | (Number | Boolean | "(" Expr ")" | Variable) RestAtomicExpr
Variable         ::= Name