-- Copyright D Poole 2018. All rights reserved. Do no redistribute.
-- lect 18 clicker code

funs 0 = []
funs n = (\x -> x+n) : funs (n-1)
f5 = funs 5
v5 = [f 10 | f <- f5]
