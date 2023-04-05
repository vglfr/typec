{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Trifecta (eof, foldResult, parseString)

import Typec.AST (Exp ((:+), (:-), (:*), (:/), Exe))
import Typec.Example
  (
    a1, a2, a3, a4
  , b1, b2, b3, b4, b7, b8
  , e1, e2, e3, e4, e5, e6
  , f1, f2, f3
  , i1, i2
  -- , m1, m2, m3, m4, m5, m6, m7
  , v1, v2
  , w1, w2
  )
import Typec.Parser (parseAss, parseExe, parseExp, parseFun, parseId, parseVal, parseVar)

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
    show <$> parse "-5" `shouldBe` Just "-5"

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
    parse "(1 + 2) + (3 + 4)" `shouldBe` Just ((1 :+ 2) :+ (3 :+ 4))

  it "parseExp (1 + 2) + 3" $ do
    parse "(1 + 2) + 3" `shouldBe` Just ((1 :+ 2) :+ 3)

  it "parseExp (1 + (2 + 3)) + 4" $ do
    parse "(1 + (2 + 3)) + 4" `shouldBe` Just ((1 :+ (2 :+ 3)) :+ 4)

  it "parseExp (1 + (2 + 3)) + ((4))" $ do
    parse "(1 + (2 + 3)) + ((4))" `shouldBe` Just ((1 :+ (2 :+ 3)) :+ 4)

  it "parseExp ((1 + 2) + 3)" $ do
    parse "((1 + 2) + 3)" `shouldBe` Just ((1 :+ 2) :+ 3)

  it "parseExp (((1 + 2)) + 3)" $ do
    parse "(((1 + 2)) + 3)" `shouldBe` Just ((1 :+ 2) :+ 3)

  it "parseExp (((1 + 2) + 3))" $ do
    parse "(((1 + 2) + 3))" `shouldBe` Just ((1 :+ 2) :+ 3)

  it "parseExp 1 + 2 + 3" $ do
    parse "1 + 2 + 3" `shouldBe` Just (1 :+ 2 :+ 3)

  it "parseExp 1 + 2 + x" $ do
    parse "1 + 2 + x" `shouldBe` Just (1 :+ 2 :+ "x")

  it "parseExp 1 + 2 - 3" $ do
    parse "1 + 2 - 3" `shouldBe` Just (1 :+ 2 :- 3)

  it "parseExp 1 * 2 - 3" $ do
    parse "1 * 2 - 3" `shouldBe` Just (1 :* 2 :- 3)

  it "parseExp 1 * 2 / 3" $ do
    parse "1 * 2 / 3" `shouldBe` Just (1 :* 2 :/ 3)

  it "parseExp (1 + 2 + 3)" $ do
    parse "(1 + 2 + 3)" `shouldBe` Just (1 :+ 2 :+ 3)

  it "parseExp ((1 + 2 + 3))" $ do
    parse "((1 + 2 + 3))" `shouldBe` Just (1 :+ 2 :+ 3)

  it "parseExp (((1 + 2 + 3)))" $ do
    parse "(((1 + 2 + 3)))" `shouldBe` Just (1 :+ 2 :+ 3)

  it "parseExp (1 + 2) + 3" $ do
    parse "(1 + 2) + 3" `shouldBe` Just (1 :+ 2 :+ 3)

  it "parseExp 1 + (2 + 3)" $ do
    parse "1 + (2 + 3)" `shouldBe` Just (1 :+ (2 :+ 3))

  it "parseExp ((1 + 2) + 3)" $ do
    parse "((1 + 2) + 3)" `shouldBe` Just (1 :+ 2 :+ 3)

  it "parseExp (1 + (2 + 3))" $ do
    parse "(1 + (2 + 3))" `shouldBe` Just (1 :+ (2 :+ 3))

  it "parseExp ((((1 + 2)) + 3))" $ do
    parse "((((1 + 2)) + 3))" `shouldBe` Just (1 :+ 2 :+ 3)

  it "parseExp ((1 + ((2 + 3))))" $ do
    parse "((1 + ((2 + 3))))" `shouldBe` Just (1 :+ (2 :+ 3))

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
    parse "5 + f x" `shouldBe` Just (5 :+ Exe "f" ("x" :| []))

  it "parseExp 5 + f x y" $ do
    parse "5 + f x y" `shouldBe` Just (5 :+ Exe "f" ("x" :| ["y"]))

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

  it "parseFun f x y = g x + z\n where\n  g a = a / 2\n  z = y * 2" $ do
    parse "f x y = g x + z\n where\n  g a = a / 2\n  z = y * 2" `shouldBe` Just f3

-- Comb

-- testParseProg :: Spec
-- testParseProg = describe "Typec.Parser" $ do
--   let parse = foldResult (const Nothing) Just . parseString (parseMod <* eof) mempty

--   it "parseMod m1 4 - 3; 1 + 2" $ do
--     parse "4 - 3; 1 + 2" `shouldBe` Just m1

--   it "parseMod m2 x = 5; x / 2; y = x * 2; x + 1 - y" $ do
--     parse "x = 5; x / 2; y = x * 2; x + 1 - y" `shouldBe` Just m2

--   it "parseMod m3 x = 5; 3 * 2 / 4 - 6; y = 3; 3 - 4 + 6" $ do
--     parse "x = 5; 3 * 2 / 4 - 6; y = 3; 3 - 4 + 6" `shouldBe` Just m3

--   it "parseMod m4 x = 5 + 2; 3 * 2 / 4 - 6; y = 3 * 2; 3 - 4 + 6" $ do
--     parse "x = 5 + 2; 3 * 2 / 4 - 6; y = 3 * 2; 3 - 4 + 6" `shouldBe` Just m4

--   it "parseMod m5 x = 5; x * 2" $ do
--     parse "x = 5; x * 2" `shouldBe` Just m5

--   it "parseMod m6 x = 5 + 3; x = x * 2; x / 5" $ do
--     parse "x = 5 + 3; x = x * 2; x / 5" `shouldBe` Just m6

--   it "parseMod m7 f(z) { z * 2 }; x = f(4) * 2" $ do
--     parse "f(z) { z * 2 }; x = f(4) * 2" `shouldBe` Just m7
