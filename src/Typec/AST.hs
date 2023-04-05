{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Typec.AST where

import Prelude hiding (unwords)

import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty, intersperse)
import Data.String (IsString, fromString)
import GHC.Show (showSpace)

newtype Prog = Prog (NonEmpty Comb) deriving Eq

data Comb
  = Id := Exp
  | Fun Id (NonEmpty Id) Exp [Comb]
  deriving Eq

newtype Id = Id String deriving Eq

data Exp
  = Exp :+ Exp
  | Exp :- Exp
  | Exp :* Exp
  | Exp :/ Exp
  | Exe Id (NonEmpty Exp)
  | Var Id
  | Val Double
  deriving Eq

infixl 5 :+
infixl 5 :-
infixl 6 :*
infixl 6 :/
infixr 0 :=

instance Show Prog where
  show (Prog cs) = Typec.AST.intercalate "\n\n" . fmap show $ cs

instance Show Comb where
  show (i := e) = show i <> " = " <> show e
  show (Fun i as e ss) = show i <> " " <> unwords (fmap show as) <> " = " <> show e <> Data.List.intercalate "\n" (fmap show ss)

instance Show Id where
  show (Id s) = s

instance Show Exp where
  showsPrec n e = case e of
                    x :+ y -> showParen (n > 5) $ showsPrec 5 x . showString " + " . showsPrec 6 y
                    x :- y -> showParen (n > 5) $ showsPrec 5 x . showString " - " . showsPrec 6 y
                    x :* y -> showParen (n > 6) $ showsPrec 6 x . showString " * " . showsPrec 7 y
                    x :/ y -> showParen (n > 6) $ showsPrec 6 x . showString " / " . showsPrec 7 y
                    Exe f es -> shows f . showSpace . showsArg es
                    Var i -> shows i
                    Val v -> let i = round v
                              in if v == fromInteger i
                                 then shows i
                                 else shows v
   where showsArg = foldr (.) id . intersperse showSpace . fmap shows

instance Num Exp where
  fromInteger = Val . fromInteger
  negate (Val a) = Val (negate a)

instance Fractional Exp where
  fromRational = Val . fromRational

instance IsString Exp where
  fromString = Var . Id

instance IsString Id where
  fromString = Id

intercalate :: [a] -> NonEmpty [a] -> [a]
intercalate x = concat . intersperse x

unwords :: NonEmpty String -> String
unwords = Typec.AST.intercalate " "
