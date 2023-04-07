{-# OPTIONS_GHC -Wno-orphans #-}

module AST where

import Prelude hiding (exp)

import Control.Applicative (liftA2)
import Data.List.NonEmpty (fromList)

import Test.QuickCheck
  (
    Arbitrary (arbitrary), Args (maxSize)
  , elements, listOf, oneof, quickCheck, quickCheckWith, sized, stdArgs, vectorOf, within
  )
import Text.Trifecta (eof, foldResult, parseString)

import Typec.AST (Comb ((:=), Fun), Exp ((:+), (:-), (:*), (:/), Exe, Val, Var), Id (Id), Prog (Prog))
import Typec.Parser (parseComb, parseExp, parseId, parseProg)

instance Arbitrary Id where
  arbitrary = Id <$> liftA2 (:) start nexts
   where
    start = elements $ ['a'..'z'] <> ['A'..'Z'] <> ['_']
    nexts = listOf $ elements $ ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> ['_']

instance Arbitrary Exp where
  arbitrary = sized exp
   where
    exp 0 = oneof
              [
                var
              , val
              ]
    exp n = oneof
              [
                liftA2 (:+) (sex n) (sex n)
              , liftA2 (:-) (sex n) (sex n)
              , liftA2 (:*) (sex n) (sex n)
              , liftA2 (:/) (sex n) (sex n)
              , exe n
              , var
              , val
              ]
    exe n = liftA2 Exe arbitrary (fromList <$> vectorOf n (sex n))
    var = Var <$> arbitrary
    val = Val <$> arbitrary

    sex n = exp $ n `div` 2

instance Arbitrary Comb where
  arbitrary = sized comb
   where
    comb n = oneof
               [
                 ass
               , fun n
               ]
    ass = liftA2 (:=) arbitrary arbitrary
    fun n = let scomb = if n <= 1
                        then pure []
                        else vectorOf n (comb $ n `div` 2)
             in Fun <$> arbitrary <*> (fromList <$> vectorOf (max 1 n) arbitrary) <*> arbitrary <*> scomb

instance Arbitrary Prog where
  arbitrary = sized (\n -> Prog . fromList <$> vectorOf (max 1 n) arbitrary)

qtestId :: IO ()
qtestId = quickCheck readShow
 where
  readShow i = parse (show i) == Just i
  parse = foldResult (const Nothing) Just . parseString (parseId <* eof) mempty

qtestExp :: IO ()
qtestExp = quickCheckWith (stdArgs { maxSize = 4 }) (within 4000 readShow)
 where
  readShow e = parse (show e) == Just e
  parse = foldResult (const Nothing) Just . parseString (parseExp <* eof) mempty

qtestComb :: IO ()
qtestComb = quickCheckWith (stdArgs { maxSize = 3 }) (within 3000 readShow)
 where
  readShow c = parse (show c) == Just c
  parse = foldResult (const Nothing) Just . parseString (parseComb <* eof) mempty

qtestProg :: IO ()
qtestProg = quickCheckWith (stdArgs { maxSize = 4 }) (within 4000 readShow)
 where
  readShow c = parse (show c) == Just c
  parse = foldResult (const Nothing) Just . parseString (parseProg <* eof) mempty