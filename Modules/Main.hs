import Datatypes
import Tokenizer
import Parser
import Compiler
import Emulator

e1 = "bool x = ((x == true) | (x == false)); f x = if bool x | x < 1 then 1 else x * f (x - 1); main = f 6;"
e2 = "main = quadrat (quadrat (3 * 1)); quadrat x = x * x;"
e3 = "main = f 3; f x = let y = 5, z = false in if (y < x) == z then x / (y / 3) else f (x - 1); f = 1;"
e4 = "main = 1;"
e5 = "main = 1+2;"
e6 = "f x y = 1;main = e;" --passt -- VariableNotInScope e
e7 = "f x y = x + y;" --passt -- MissingMain
e8 = "f x y = c; main = f 3 4;"
e9 = "vector x y = (x y); main = vector 3 2;" -- Prelude.!!: negative index 
e10 = "id x = x; main = id 1;"
e11 = "vektorpr x y z w = x*z + y*w; main = vektorpr 1 2 3 4;"
e12 = "main = #;"
e13 = " main = 3; +" -- passt -- "parse error on input: +" 
e14 = "main = quadratwurzel 2; quadratwurzel x = 1 + qw x 1000; qw a b = if b == 0 then a else (a-1)/(2+qw a (b-1));"