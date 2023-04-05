{-# OPTIONS_GHC -Wno-orphans #-}

module AST where

import Control.Applicative (liftA2, liftA3)
import Data.List (intercalate)

import Test.QuickCheck
  (
    Arbitrary (arbitrary), Args (maxSize), elements, listOf, oneof, quickCheckWith, sized, stdArgs, vectorOf, within
  )
import Text.Trifecta (eof, foldResult, parseString)

import Typec.AST (Expr ((:+), (:-), (:*), (:/), (:=), Exe, Val, Var, Fun))
import Typec.Parser (parseMod)

instance Arbitrary Expr where
  arbitrary = sized expr
   where
    expr 0 = oneof
               [
                 val
               , var
               ]
    expr n = oneof
               [
                 val
               , var
               , ass n
               , bin n
               , exe n
               , fun n
               ]
    var = Var <$> liftA2 (:) start nexts
    val = Val <$> arbitrary
    bin n = oneof
              [
                liftA2 (:+) (sbin n) (sbin n)
              , liftA2 (:-) (sbin n) (sbin n)
              , liftA2 (:*) (sbin n) (sbin n)
              , liftA2 (:/) (sbin n) (sbin n)
              ]
    ass n = liftA2 (:=) var (bin n)
    exe n = liftA2 Exe var (vectorOf n $ sbin n)
    fun n = liftA3 Fun var (listOf var) (vectorOf n $ sbin n)

    sbin 0 = oneof [val, var, exe 1] -- to chooseInt
    sbin n = bin (n `div` 2)

    start = elements ['a'..'z']
    nexts = listOf $ elements $ ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> ['_', '\'']

qtestExpr :: IO ()
qtestExpr = quickCheckWith (stdArgs { maxSize = 6 }) (within 8000 readShow)
 where
  readShow es = parse (intercalate ";" . fmap show $ es) == Just es
  parse = foldResult (const Nothing) Just . parseString (parseMod <* eof) mempty