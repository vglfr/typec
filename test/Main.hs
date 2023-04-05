module Main where

import Test.Hspec (hspec)

-- import AST (qtestExpr)
import Parser (testParseExe, testParseExp, testParseId, testParseVal, testParseVar)

main :: IO ()
main = do
  hspec $ do
    testParseId
    testParseVar
    testParseVal
    testParseExe
    testParseExp
    -- testParseAss
    -- testParseExe
    -- testParseFun
    -- testParseMod
  -- qtestExpr