module Main where

import Test.Hspec (hspec)

import AST (qtestComb, qtestExp, qtestId, qtestProg)
-- import Compiler ()
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
  qtestId
  qtestExp
  qtestComb
  qtestProg
