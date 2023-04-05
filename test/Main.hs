module Main where

import Test.Hspec (hspec)

-- import AST (qtestExpr)
import Parser (testParseAss, testParseExe, testParseExp, testParseFun, testParseId, testParseVal, testParseVar)

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
    -- testParseMod
  -- qtestExpr