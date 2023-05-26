{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Trifecta (eof, foldResult, parseString)

import Typec.AST
  (
    Exp (Bin, Exe)
  , Op (Add, Div, Mul, Sub)
  )

import Typec.Example
  (
    a1, a2, a3, a4
  , b1, b2, b3, b4, b7, b8
  , e1, e2, e3, e4, e5, e6
  , f1, f2, f3
  , i1, i2
  , p1, p2, p3, p4, p5, p6, p7, p8, p9
  , v1, v2, v3
  , w1, w2
  )
import Typec.Parser (parseAss, parseComb, parseExe, parseExp, parseFun, parseId, parseProg, parseVal, parseVar)

testParseId :: Spec
testParseId = describe "Typec.Parser" $ do
  let parse = foldResult (const Nothing) Just . parseString (parseId <* eof) mempty

  it "parseVar x" $ do
    parse "x" `shouldBe` Just i1

  it "parseVar yY32_" $ do
    parse "yY32_" `shouldBe` Just i2

  it "parseVar x1_X" $ do
    parse "x1_X" `shouldBe` Just "x1_X"

  it "parseVar _x" $ do
    parse "_x" `shouldBe` Just "_x"

  it "parseVar X" $ do
    parse "X" `shouldBe` Just "X"

  it "parseVar 1x" $ do
    parse "1x" `shouldBe` Nothing

  it "parseVar 1" $ do
    parse "1" `shouldBe` Nothing

  it "parseVar _" $ do
    parse "_" `shouldBe` Just "_"

  it "parseVar '" $ do
    parse "'" `shouldBe` Nothing

  it "parseVar x+x" $ do
    parse "x+x" `shouldBe` Nothing

testParseVar :: Spec
testParseVar = describe "Typec.Parser" $ do
  let parse = foldResult (const Nothing) Just . parseString (parseVar <* eof) mempty

  it "parseVar x" $ do
    parse "x" `shouldBe` Just w1

  it "parseVar yY32_" $ do
    parse "yY32_" `shouldBe` Just w2

testParseVal :: Spec
testParseVal = describe "Typec.Parser" $ do
  let parse = foldResult (const Nothing) Just . parseString (parseVal <* eof) mempty

  it "parseVal 5" $ do
    parse "5" `shouldBe` Just v1

  it "parseVal +5" $ do
    parse "+5" `shouldBe` Just v1

  it "parseVal -5" $ do
    parse "-5" `shouldBe` Just v3

  it "parseVal empty" $ do
    parse "" `shouldBe` Nothing

  it "parseVal -" $ do
    parse "-" `shouldBe` Nothing

  it "parseVal +" $ do
    parse "+" `shouldBe` Nothing

  it "parseVal 5.5" $ do
    parse "5.5" `shouldBe` Just v2

  it "parseVal 5." $ do
    parse "5." `shouldBe` Nothing

  it "parseVal -5." $ do
    parse "-5." `shouldBe` Nothing

  it "parseVal .5" $ do
    parse ".5" `shouldBe` Nothing

  it "parseVal -.5" $ do
    parse "-.5" `shouldBe` Nothing

  it "parseVal 5.5e2" $ do
    parse "5.5e2" `shouldBe` Just 550

  it "parseVal 5.5E2" $ do
    parse "5.5E2" `shouldBe` Just 550

  it "parseVal 5.5e+2" $ do
    parse "5.5e+2" `shouldBe` Just 550

  it "parseVal 5.5E+2" $ do
    parse "5.5E+2" `shouldBe` Just 550

  it "parseVal 5.5e-2" $ do
    parse "5.5e-2" `shouldBe` Just 0.055

  it "parseVal 5.5E-2" $ do
    parse "5.5E-2" `shouldBe` Just 0.055

  it "parseVal ." $ do
    parse "." `shouldBe` Nothing

  it "parseVal -." $ do
    parse "-." `shouldBe` Nothing

  it "parseVal .e5" $ do
    parse ".e5" `shouldBe` Nothing

testParseExe :: Spec
testParseExe = describe "Typec.Parser" $ do
  let parse = foldResult (const Nothing) Just . parseString (parseExe <* eof) mempty

  it "parseExe f 5" $ do
    parse "f 5" `shouldBe` Just e1

  it "parseExe f 5 3" $ do
    parse "f 5 3" `shouldBe` Just e2

  it "parseExe f (5 + 3)" $ do
    parse "f (5 + 3)" `shouldBe` Just e3

  it "parseExe f 3 (5 + y)" $ do
    parse "f 3 (5 + y)" `shouldBe` Just e4

  it "parseExe f x y" $ do
    parse "f x y" `shouldBe` Just e5

  it "parseExe f 3 (5 + y) x" $ do
    parse "f 3 (5 + y) x" `shouldBe` Just e6

  it "parseExe f x y z a b c" $ do
    parse "f x y z a b c" `shouldBe` Just (Exe "f" ("x" :| ["y", "z", "a", "b", "c"]))

  it "parseExe f x (g a b) y" $ do
    parse "f x (g a b) y" `shouldBe` Just (Exe "f" ("x" :| [Exe "g" ("a" :| ["b"]), "y"]))

  it "parseExe f" $ do
    parse "f" `shouldBe` Nothing

testParseExp :: Spec
testParseExp = describe "Typec.Parser" $ do
  let parse = foldResult (const Nothing) Just . parseString (parseExp <* eof) mempty

  it "parseExp 5" $ do
    parse "5" `shouldBe` Just v1

  it "parseExp x" $ do
    parse "x" `shouldBe` Just w1

  it "parseExp (( 5 ))" $ do
    parse "(( 5 ))" `shouldBe` Just v1

  it "parseExp (( x ))" $ do
    parse "(( x ))" `shouldBe` Just w1

  it "parseExp ((1 + 2))" $ do
    parse "((1 + 2))" `shouldBe` Just b1

  it "parseExp (((1 + 2)))" $ do
    parse "(((1 + 2)))" `shouldBe` Just b1

  it "parseExp ( ( ( 1 + 2 ) ) )" $ do
    parse "( ( ( 1 + 2 ) ) )" `shouldBe` Just b1

  it "parseExp (1 + 2) " $ do
    parse "(1 + 2) " `shouldBe` Just b1

  it "parseExp (1 + 2 ) " $ do
    parse "(1 + 2 ) " `shouldBe` Just b1

  it "parseExp (1 + 2) + (3 + 4)" $ do
    parse "(1 + 2) + (3 + 4)" `shouldBe` Just (Bin Add (Bin Add 1 2) (Bin Add 3 4))

  it "parseExp (1 + 2) + 3" $ do
    parse "(1 + 2) + 3" `shouldBe` Just (Bin Add (Bin Add 1 2) 3)

  it "parseExp (1 + (2 + 3)) + 4" $ do
    parse "(1 + (2 + 3)) + 4" `shouldBe` Just (Bin Add (Bin Add 1 (Bin Add 2 3)) 4)

  it "parseExp (1 + (2 + 3)) + ((4))" $ do
    parse "(1 + (2 + 3)) + ((4))" `shouldBe` Just (Bin Add (Bin Add 1 (Bin Add 2 3)) 4)

  it "parseExp ((1 + 2) + 3)" $ do
    parse "((1 + 2) + 3)" `shouldBe` Just (Bin Add (Bin Add 1 2) 3)

  it "parseExp (((1 + 2)) + 3)" $ do
    parse "(((1 + 2)) + 3)" `shouldBe` Just (Bin Add (Bin Add 1 2) 3)

  it "parseExp (((1 + 2) + 3))" $ do
    parse "(((1 + 2) + 3))" `shouldBe` Just (Bin Add (Bin Add 1 2) 3)

  it "parseExp 1 + 2 + 3" $ do
    parse "1 + 2 + 3" `shouldBe` Just (Bin Add (Bin Add 1 2) 3)

  it "parseExp 1 + 2 + x" $ do
    parse "1 + 2 + x" `shouldBe` Just (Bin Add (Bin Add 1 2) "x")

  it "parseExp 1 + 2 - 3" $ do
    parse "1 + 2 - 3" `shouldBe` Just (Bin Sub (Bin Add 1 2) 3)

  it "parseExp 1 * 2 - 3" $ do
    parse "1 * 2 - 3" `shouldBe` Just (Bin Sub (Bin Mul 1 2) 3)

  it "parseExp 1 * 2 / 3" $ do
    parse "1 * 2 / 3" `shouldBe` Just (Bin Div (Bin Mul 1 2) 3)

  it "parseExp (1 + 2 + 3)" $ do
    parse "(1 + 2 + 3)" `shouldBe` Just (Bin Add (Bin Add 1 2) 3)

  it "parseExp ((1 + 2 + 3))" $ do
    parse "((1 + 2 + 3))" `shouldBe` Just (Bin Add (Bin Add 1 2) 3)

  it "parseExp (((1 + 2 + 3)))" $ do
    parse "(((1 + 2 + 3)))" `shouldBe` Just (Bin Add (Bin Add 1 2) 3)

  it "parseExp (1 + 2) + 3" $ do
    parse "(1 + 2) + 3" `shouldBe` Just (Bin Add (Bin Add 1 2) 3)

  it "parseExp 1 + (2 + 3)" $ do
    parse "1 + (2 + 3)" `shouldBe` Just (Bin Add 1 (Bin Add 2 3))

  it "parseExp ((1 + 2) + 3)" $ do
    parse "((1 + 2) + 3)" `shouldBe` Just (Bin Add (Bin Add 1 2) 3)

  it "parseExp (1 + (2 + 3))" $ do
    parse "(1 + (2 + 3))" `shouldBe` Just (Bin Add 1 (Bin Add 2 3))

  it "parseExp ((((1 + 2)) + 3))" $ do
    parse "((((1 + 2)) + 3))" `shouldBe` Just (Bin Add (Bin Add 1 2) 3)

  it "parseExp ((1 + ((2 + 3))))" $ do
    parse "((1 + ((2 + 3))))" `shouldBe` Just (Bin Add 1 (Bin Add 2 3))

  it "parseExp 1 + 2" $ do
    parse "1 + 2" `shouldBe` Just b1

  it "parseExp 1 - 2" $ do
    parse "1 - 2" `shouldBe` Just b2

  it "parseExp 1 * 2" $ do
    parse "1 * 2" `shouldBe` Just b3

  it "parseExp 1 / 2" $ do
    parse "1 / 2" `shouldBe` Just b4

  it "parseExp (1 + 2)" $ do
    parse "(1 + 2)" `shouldBe` Just b1

  it "parseExp ( 1 + 2 )" $ do
    parse "( 1 + 2 )" `shouldBe` Just b1

  it "parseExp ((1 + 2))" $ do
    parse "((1 + 2))" `shouldBe` Just b1

  it "parseExp (((1 + 2)))" $ do
    parse "(((1 + 2)))" `shouldBe` Just b1

  it "parseExp (1+2)" $ do
    parse "(1+2)" `shouldBe` Just b1

  it "parseExp ((1) + (2))" $ do
    parse "((1) + (2))" `shouldBe` Just b1

  it "parseExp (((1) + (2)))" $ do
    parse "(((1) + (2)))" `shouldBe` Just b1

  it "parseExp (((1)+(2)))" $ do
    parse "(((1)+(2)))" `shouldBe` Just b1

  it "parseExp ((((1)) + ((2))))" $ do
    parse "((((1)) + ((2))))" `shouldBe` Just b1

  it "parseExp (( (( 1 )) + (( 2 )) ))" $ do
    parse "(( (( 1 )) + (( 2 )) ))" `shouldBe` Just b1

  it "parseExp ( ( ( ( 1 ) ) + ( ( 2 ) ) ) )" $ do
    parse "( ( ( ( 1 ) ) + ( ( 2 ) ) ) )" `shouldBe` Just b1

  it "parseExp ((1)) + ((2))" $ do
    parse "((1)) + ((2))" `shouldBe` Just b1

  it "parseExp (((1)) + ((2)))" $ do
    parse "(((1)) + ((2)))" `shouldBe` Just b1

  it "parseExp (1) + (2)" $ do
    parse "(1) + (2)" `shouldBe` Just b1

  it "parseExp ((1) + 2)" $ do
    parse "((1) + 2)" `shouldBe` Just b1

  it "parseExp (((1)) + 2)" $ do
    parse "(((1)) + 2)" `shouldBe` Just b1

  it "parseExp 1 + 2)" $ do
    parse "1 + 2)" `shouldBe` Nothing

  it "parseExp (1 + 2" $ do
    parse "(1 + 2" `shouldBe` Nothing

  it "parseExp -1 + 2" $ do
    show <$> parse "-1 + 2" `shouldBe` Just "-1 + 2"

  it "parseExp 1 - -2" $ do
    show <$> parse "1 - -2" `shouldBe` Just "1 - -2"

  it "parseExp 5 + 1 - 3 + 6 * 2 / 4" $ do
    parse "5 + 1 - 3 + 6 * 2 / 4" `shouldBe` Just b7

  it "parseExp 5 + f x 3 - 3" $ do
    parse "5 + f x 3 - 3" `shouldBe` Just b8

  it "parseExp 5 + f x (3 / y) - 3" $ do
    show <$> parse "5 + f x (3 / y) - 3" `shouldBe` Just "5 + f x (3 / y) - 3"

  it "parseExp 5 + f x" $ do
    parse "5 + f x" `shouldBe` Just (Bin Add 5 (Exe "f" ("x" :| [])))

  it "parseExp 5 + f x y" $ do
    parse "5 + f x y" `shouldBe` Just (Bin Add 5 (Exe "f" ("x" :| ["y"])))

testParseAss :: Spec
testParseAss = describe "Typec.Parser" $ do
  let parse = foldResult (const Nothing) Just . parseString (parseAss <* eof) mempty

  it "parseAss x = 5" $ do
    parse "x = 5" `shouldBe` Just a1

  it "parseAss x = 5 + 1 / 4 - 3 * 2" $ do
    parse "x = 5 + 1 / 4 - 3 * 2" `shouldBe` Just a2

  it "parseAss x = 5 + y" $ do
    parse "x = 5 + y" `shouldBe` Just a3

  it "parseAss x = 5 + f y" $ do
    parse "x = 5 + f y" `shouldBe` Just a4

testParseFun :: Spec
testParseFun = describe "Typec.Parser" $ do
  let parse = foldResult (const Nothing) Just . parseString (parseFun <* eof) mempty

  it "parseFun f x = (x + x) * x" $ do
    parse "f x = (x + x) * x" `shouldBe` Just f1

  it "parseFun f x y = (x + y) * x / y" $ do
    parse "f x y = (x + y) * x / y" `shouldBe` Just f2

  it "parseFun f x y = g x + z\\n where\\n  g a = a / 2\\n  z = y * 2" $ do
    parse "f x y = g x + z\n where\n  g a = a / 2\n  z = y * 2" `shouldBe` Just f3

testParseComb :: Spec
testParseComb = describe "Typec.Parser" $ do
  let parse = foldResult (const Nothing) Just . parseString (parseComb <* eof) mempty

  it "parseComb x = 5" $ do
    parse "x = 5" `shouldBe` Just a1

  it "parseComb x = 5 + 1 / 4 - 3 * 2" $ do
    parse "x = 5 + 1 / 4 - 3 * 2" `shouldBe` Just a2

  it "parseComb x = 5 + y" $ do
    parse "x = 5 + y" `shouldBe` Just a3

  it "parseComb x = 5 + f y" $ do
    parse "x = 5 + f y" `shouldBe` Just a4

  it "parseComb f x = (x + x) * x" $ do
    parse "f x = (x + x) * x" `shouldBe` Just f1

  it "parseComb f x y = (x + y) * x / y" $ do
    parse "f x y = (x + y) * x / y" `shouldBe` Just f2

  it "parseComb f x y = g x + z\\n where\\n  g a = a / 2\\n  z = y * 2" $ do
    parse "f x y = g x + z\n where\n  g a = a / 2\n  z = y * 2" `shouldBe` Just f3

testParseProg :: Spec
testParseProg = describe "Typec.Parser" $ do
  let parse = foldResult (const Nothing) Just . parseString (parseProg <* eof) mempty

  it "parseProg x = 5\\nmain = x * 2" $ do
    parse "x = 5\nmain = x * 2" `shouldBe` Just p1

  it "parseProg x = 5\\n\\n\\nmain = x * 2" $ do
    parse "x = 5\n\n\nmain = x * 2" `shouldBe` Just p1

  it "parseProg x = 5\\ny = x * 2\\nmain = x + 1 - y" $ do
    parse "x = 5\ny = x * 2\nmain = x + 1 - y" `shouldBe` Just p2

  it "parseProg y = 5\\nf x = y * 2 - x\\nmain = f 3 - 2 * y" $ do
    parse "y = 5\nf x = y * 2 - x\nmain = f 3 - 2 * y" `shouldBe` Just p3

  it "parseProg z = 5\\n\\nf x = z * 2 - x\\n\\nu = 7 + f z\\n\\ng x y = f y * h x * 3 / z - 2 * w\\n where\\n  h x = f x / 3\\n  w = u + 2\\n\\nmain = f 3 - 2 * z + g u z - z" $ do
    parse "z = 5\n\nf x = z * 2 - x\n\nu = 7 + f z\n\ng x y = f y * h x * 3 / z - 2 * w\n where\n  h x = f x / 3\n  w = u + 2\n\nmain = f 3 - 2 * z + g u z - z" `shouldBe` Just p4

  it "parseProg main = 3 * 2" $ do
    parse "main = 3 * 2" `shouldBe` Just p5

  it "parseProg main = (3 + 2) / ((3 - 2) * 4 + 6)" $ do
    parse "main = (3 + 2) / ((3 - 2) * 4 + 6)" `shouldBe` Just p6

  it "parseProg f x = 2 - x\\nmain = f 3 - 2" $ do
    parse "f x = 2 - x\nmain = f 3 - 2" `shouldBe` Just p7

  it "parseProg x = 5\\ny = 7 + 5 * x\\n\\nf u w = g u * 3 / y - 2 * x + g w\\n where\\n  g a = 5 * a / 3\\n\\nmain = f 3 y - 2 * x + f y 0 - y" $ do
    parse "x = 5\ny = 7 + 5 * x\n\nf u w = g u * 3 / y - 2 * x + g w\n where\n  g a = 5 * a / 3\n\nmain = f 3 y - 2 * x + f y 0 - y" `shouldBe` Just p8

  it "parseProg a = x - 3\\nb = 5 * a - a\\nc = d + 4 - x\\nd = a\\nx = 5\\ny = x * 2\\nz = x + 4 * y + b\\nmain = x + 1 - y / a - 2 * z + b * 3 / c" $ do
    parse "a = x - 3\nb = 5 * a - a\nc = d + 4 - x\nd = a\nx = 5\ny = x * 2\nz = x + 4 * y + b\nmain = x + 1 - y / a - 2 * z + b * 3 / c" `shouldBe` Just p9
