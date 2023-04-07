module Main where

import Test.Hspec (hspec)

import AST (qtestComb, qtestExp, qtestId, qtestProg)
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
  qtestId
  qtestExp
  qtestComb
  qtestProg
