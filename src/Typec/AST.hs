{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

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
  show (Fun i as e cs) = show i <> " " <> unwords (fmap show as) <> " = " <> show e <> showCombs cs 
   where showCombs ss = if null ss
                        then ""
                        else "\n where\n  " <> Data.List.intercalate "\n  " (fmap show ss)

instance Show Id where
  show (Id s) = s

instance Show Exp where
  showsPrec n e = case e of
                    x :+ y -> showParen (n > 5) $ showsPrec 5 x . showString " + " . showsPrec 6 y
                    x :- y -> showParen (n > 5) $ showsPrec 5 x . showString " - " . showsPrec 6 y
                    x :* y -> showParen (n > 6) $ showsPrec 6 x . showString " * " . showsPrec 7 y
                    x :/ y -> showParen (n > 6) $ showsPrec 6 x . showString " / " . showsPrec 7 y
                    Exe f es -> shows f . showSpace . showArgs es
                    Var i -> shows i
                    Val v -> let i = round v
                              in if v == fromInteger i
                                 then shows i
                                 else shows v
   where
    showArgs = foldr (.) id . intersperse showSpace . fmap showArg
    showArg a = case a of
                  Val _ -> shows a
                  Var _ -> shows a
                  _ -> showParen True (shows a)

instance Num Exp where
  fromInteger = Val . fromInteger

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
