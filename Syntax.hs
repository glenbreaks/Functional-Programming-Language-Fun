Program         ::= Def ";" RestProgram
RestProgram     ::= e | Def ";" RestProgram
Def             ::= Variable Restdef
RestDef         ::= "=" Expr | Variable Restdef
LocDefs         ::= LocDef RestLocDefs
RestLocDefs     ::= e | ";" LocDefs
LocDef          ::= Variable "=" Expr
Expr            ::= "let" LocDefs "in" Expr
                  | "if" Expr "then" Expr "else" Expr         
                  | OrExpr
OrExpr          ::= AndExpr RestOrExpr
RestOrExpr      ::= e | "|" OrExpr
AndExpr         ::= NotExpr RestAndExpr
RestAndExpr     ::= e | "&" AndExpr
NotExpr         ::= "not" CompareExpr | CompareExpr
CompareExpr     ::= AddExpr RestCompareExpr
RestCompareExpr ::= e | ("<" | "==") AddExpr
AddExpr         ::= MultExpr ("-" MultExpr2 | "+" RestAddExpr | e)
RestAddExpr     ::= e | MultExpr2 RestAddExpr
MultExpr        ::= NegExpr ("/" MultExpr2 | "*" RestMultExpr | e)
MultExpr2       ::= AtomicExpr ("/" MultExpr2 | "*" RestMultExpr | e)
RestMultExpr    ::= e | MultExpr2 RestMultExpr
NegExpr         ::= "-" AtomicExpr | AtomicExpr
AtomicExpr      ::= (Number | Boolean | "(" Expr ")" | Variable) RestAtomicExpr
RestAtomicExpr  ::= e | AtomicExpr RestAtomicExpr
Variable        ::= Name