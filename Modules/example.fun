main = quadratwurzel 25;
quadratwurzel x = 1 + qw x 20;
qw a b = if b == 0 then a else (a-1)/(2+qw a (b-1));