{-# LANGUAGE OverloadedStrings #-}

module Typec.Example where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Typec.AST (Prog (Prog), Comb ((:=), Fun), Id, Exp ((:+), (:-), (:*), (:/), Exe))

{- x -}
i1 :: Id
i1 = "x"

{- yY32_ -}
i2 :: Id
i2 = "yY32_"

{- 5 -}
v1 :: Exp
v1 = 5

{- 5.5 -}
v2 :: Exp
v2 = 5.5

{- -5 -}
v3 :: Exp
v3 = -5

{- x -}
w1 :: Exp
w1 = "x"

{- yY32_ -}
w2 :: Exp
w2 = "yY32_"

{- 1 + 2 -}
b1 :: Exp
b1 = 1 :+ 2

{- 1 - 2 -}
b2 :: Exp
b2 = 1 :- 2

{- 1 * 2 -}
b3 :: Exp
b3 = 1 :* 2

{- 1 / 2 -}
b4 :: Exp
b4 = 1 :/ 2

{- -1 + 2 -}
b5 :: Exp
b5 = -1 :+ 2

{- 1 - -2 -}
b6 :: Exp
b6 = 1 :- -2

{- 5 + 1 - 3 + 6 * 2 / 4 -}
b7 :: Exp
b7 = 5 :+ 1 :- 3 :+ 6 :* 2 :/ 4

{- 5 + f x 3 - 3 -}
b8 :: Exp
b8 = 5 :+ Exe "f" ("x" :| [3]) :- 3

{- 5 + f x (3 - y) - 3 -}
b9 :: Exp
b9 = 5 :+ Exe "f" ("x" :| [3 - "y"]) :- 3

{- f 5 -}
e1 :: Exp
e1 = Exe "f" (5 :| [])

{- f 5 3 -}
e2 :: Exp
e2 = Exe "f" (5 :| [3])

{- f (5 + 3) -}
e3 :: Exp
e3 = Exe "f" ((5 :+ 3) :| [])

{- f 3 (5 + y) -}
e4 :: Exp
e4 = Exe "f" (3 :| [5 :+ "y"])

{- f x y -}
e5 :: Exp
e5 = Exe "f" ("x" :| ["y"])

{- f 3 (5 + y) x -}
e6 :: Exp
e6 = Exe "f" (3 :| [5 :+ "y", "x"])

{- x = 5 -}
a1 :: Comb
a1 = "x" := 5

{- x = 5 + 1 / 4 - 3 * 2 -}
a2 :: Comb
a2 = "x" := 5 :+ 1 :/ 4 :- 3 :* 2

{- x = 5 + y -}
a3 :: Comb
a3 = "x" := 5 :+ "y"

{- x = 5 + f y -}
a4 :: Comb
a4 = "x" := 5 :+ Exe "f" ("y" :| [])

{- f x = (x + x) * x -}
f1 :: Comb
f1 = Fun "f" ("x" :| []) (("x" :+ "x") :* "x") []

{- f x y = (x + y) * x / y -}
f2 :: Comb
f2 = Fun "f" ("x" :| ["y"]) (("x" :+ "y") :* "x" :/ "y") []

{-
f x y = g x + z
 where
  g a = a / 2
  z = y * 2
-}
f3 :: Comb
f3 = Fun
       "f"
       ("x" :| ["y"])
       (Exe "g" ("x" :| []) :+ "z")
       [Fun "g" ("a" :| []) ("a" :/ 2) [], "z" := "y" :* 2]

{-
x = 5
main = x * 2
-}
p1 :: Prog
p1 = Prog $ ("x" := 5) :| ["main" := "x" :* 2]

{-
x = 5
y = x * 2
main = x + 1 - y
-}
p2 :: Prog
p2 = Prog $ ("x" := 5) :|
  [
    "y" := "x" :* 2
  , "main" := "x" :+ 1 :- "y"
  ]

{-
y = 5
f x = y * 2 - x
main = f 3 - 2 * y
-}
p3 :: Prog
p3 = Prog $ ("y" := 5) :|
  [
    Fun "f" ("x" :| []) ("y" :* 2 :- "x") []
  , "main" := Exe "f" (3 :| []) :- 2 :* "y"
  ]

{-
z = 5

f x = y * 2 - x

u = 7 + f y

g x y = f y * h x * 3 / z - 2 * w
 where
  h x = f x / 3
  w = u + 2

main = f 3 - 2 * y + g x y - z
-}
p4 :: Prog
p4 = Prog $ ("z" := 5) :|
  [
    Fun "f" ("x" :| []) ("y" :* 2 :- "x") []
  , "u" := 7 :+ Exe "f" ("y" :| [])
  , Fun
      "g"
      ("x" :| ["y"])
      (Exe "f" ("y" :| []) :* Exe "h" ("x" :| []) :* 3 :/ "z" :- 2 :* "w")
      [
        Fun "h" ("x" :| []) (Exe "f" ("x" :| []) :/ 3) []
      , "w" := "u" :+ 2
      ]
  , "main" := Exe "f" (3 :| []) :- 2 :* "y" :+ Exe "g" ("x" :| ["y"]) :- "z"
  ]
