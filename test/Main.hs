module Main where

import Test.Hspec (hspec)

import AST (qtestComb, qtestExp, qtestId, qtestProg)
import Compiler (testGraph)
import Interpreter (testEvaluateBin, testEvaluateVal, testInterpret)
import Parser
  (
    testParseAss, testParseComb, testParseExe, testParseExp, testParseFun
  , testParseId, testParseProg, testParseVal, testParseVar
  )

main :: IO ()
main = do
  hspec $ do
    testParseId
    testParseVar
    testParseVal
    testParseExe
    testParseExp
    testParseAss
    testParseFun
    testParseComb
    testParseProg
  hspec $ do
    testEvaluateVal
    testEvaluateBin
    testInterpret
  hspec $ do
    testGraph
  qtestId
  qtestExp
  qtestComb
  qtestProg
