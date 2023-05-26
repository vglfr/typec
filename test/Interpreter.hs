module Interpreter where

import Test.Hspec (Spec, describe, it, shouldBe)

import Typec.Example
  (
    b1, b2, b3, b4, b5, b6, b7
  , p1, p2, p3, p4, p5, p6, p7, p8, p9
  , v1, v2, v3
  )
import Typec.Interpreter (evaluate, interpret)

testEvaluateVal :: Spec
testEvaluateVal = describe "Typec.Interpreter" $ do
  let eval = evaluate mempty

  it "evaluate 5" $ do
    eval v1 `shouldBe` 5

  it "evaluate 5.5" $ do
    eval v2 `shouldBe` 5.5

  it "evaluate -5" $ do
    eval v3 `shouldBe` -5

testEvaluateBin :: Spec
testEvaluateBin = describe "Typec.Interpreter" $ do
  let eval = evaluate mempty

  it "evaluate 1 + 2" $ do
    eval b1 `shouldBe` 3

  it "evaluate 1 - 2" $ do
    eval b2 `shouldBe` -1

  it "evaluate 1 * 2" $ do
    eval b3 `shouldBe` 2

  it "evaluate 1 / 2" $ do
    eval b4 `shouldBe` 0.5

  it "evaluate -1 + 2" $ do
    eval b5 `shouldBe` 1

  it "evaluate 1 - -2" $ do
    eval b6 `shouldBe` 3

  it "evaluate 5 + 1 - 3 + 6 * 2 / 4" $ do
    eval b7 `shouldBe` 6

testInterpret :: Spec
testInterpret = describe "Typec.Interpreter" $ do
  it "interpret x = 5\\nmain = x * 2" $ do
    interpret p1 `shouldBe` 10

  it "interpret x = 5\\ny = x * 2\\nmain = x + 1 - y" $ do
    interpret p2 `shouldBe` -4

  it "interpret y = 5\\nf x = y * 2 - x\\nmain = f 3 - 2 * y" $ do
    interpret p3 `shouldBe` -3

  it "interpret z = 5\\n\\nf x = z * 2 - x\\n\\nu = 7 + f z\\n\\ng x y = f y * h x * 3 / z - 2 * w\\n where\\n  h x = f x / 3\\n  w = u + 2\\n\\nmain = f 3 - 2 * z + g u z - z" $ do
    interpret p4 `shouldBe` -38

  it "interpret main = 3 * 2" $ do
    interpret p5 `shouldBe` 6

  it "interpret main = (3 + 2) / ((3 - 2) * 4 + 6)" $ do
    interpret p6 `shouldBe` 0.5

  it "interpret f x = 2 - x\\nmain = f 3 - 2" $ do
    interpret p7 `shouldBe` -3

  it "interpret x = 5\\ny = 7 + 5 * x\\n\\nf u w = g u * 3 / y - 2 * x + g w\\n where\\n  g a = 5 * a / 3\\n\\nmain = f 3 y - 2 * x + f y 0 - y" $ do
    interpret p8 `shouldBe` 5165/96 - 57

  it "interpret a = x - 3\\nb = 5 * a - a\\nc = d + 4 - x\\nd = a\\nx = 5\\ny = x * 2\\nz = x + 4 * y + b\\nmain = x + 1 - y / a - 2 * z + b * 3 / c" $ do
    interpret p9 `shouldBe` -81
