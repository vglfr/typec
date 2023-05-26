module Compiler where

import Data.Graph.Inductive (buildGr)
import Test.Hspec (Spec, describe, it, shouldBe, xit)

import Typec.Example
  (
    p1, p2, p3, p4, p5, p6, p7, p8, p9
  )
import Typec.Compiler (graph)

testGraph :: Spec
testGraph = describe "Typec.Compiler" $ do
  it "graph x = 5\\nmain = x * 2" $ do
    graph p1 `shouldBe` buildGr
      [
        ([],0,"x",[])
      , ([],1,"main",[("x",0)])
      ]

  it "graph x = 5\\ny = x * 2\\nmain = x + 1 - y" $ do
    graph p2 `shouldBe` buildGr
      [
        ([],0,"x",[])
      , ([],1,"main",[("x",0),("y",2)])
      , ([],2,"y",[("x",0)])
      ]

  xit "graph y = 5\\nf x = y * 2 - x\\nmain = f 3 - 2 * y" $ do
    graph p3 `shouldBe` buildGr []

  xit "graph z = 5\\n\\nf x = z * 2 - x\\n\\nu = 7 + f z\\n\\ng x y = f y * h x * 3 / z - 2 * w\\n where\\n  h x = f x / 3\\n  w = u + 2\\n\\nmain = f 3 - 2 * z + g u z - z" $ do
    graph p4 `shouldBe` buildGr []

  it "graph main = 3 * 2" $ do
    graph p5 `shouldBe` buildGr
      [
        ([],0,"main",[])
      ]

  it "graph main = (3 + 2) / ((3 - 2) * 4 + 6)" $ do
    graph p6 `shouldBe` buildGr
      [
        ([],0,"main",[])
      ]

  xit "graph f x = 2 - x\\nmain = f 3 - 2" $ do
    graph p7 `shouldBe` buildGr []

  xit "graph x = 5\\ny = 7 + 5 * x\\n\\nf u w = g u * 3 / y - 2 * x + g w\\n where\\n  g a = 5 * a / 3\\n\\nmain = f 3 y - 2 * x + f y 0 - y" $ do
    graph p8 `shouldBe` buildGr []

  it "graph a = x - 3\\nb = 5 * a - a\\nc = d + 4 - x\\nd = a\\nx = 5\\ny = x * 2\\nz = x + 4 * y + b\\nmain = x + 1 - y / a - 2 * z + b * 3 / c" $ do
    graph p9 `shouldBe` buildGr
      [
        ([],0,"d",[("a",6)])
      , ([],1,"x",[])
      , ([],2,"b",[("a",6)])
      , ([],3,"main",[("x",1), ("y",4), ("a",6), ("z",7), ("b",2), ("c",5)])
      , ([],4,"y",[("x",1)])
      , ([],5,"c",[("d",0), ("x",1)])
      , ([],6,"a",[("x",1)])
      , ([],7,"z",[("x",1), ("y",4), ("b",2)])
      ]
