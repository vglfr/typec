{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Typec.Interpreter where

import Prelude hiding (zip)

import Data.List.NonEmpty (zip)

import Data.HashMap.Strict ((!?), HashMap, unions)

import Typec.AST
  (
    Comb ((:=), Fun)
  , Exp (Bin, Exe, Val, Var)
  , Id
  , Op (Add, Div, Mul, Sub)
  , Prog (Prog)
  )
import Typec.Parser (fromNonEmpty)

evaluate :: HashMap Id Comb -> Exp -> Double
evaluate c e = case e of
                 Bin o a b -> op o (evaluate c a) (evaluate c b)
                 Exe f as -> case c !? f of
                               Just (Fun _ ps e' c') -> if length ps == length as
                                                        then let as' = fmap (evaluate c) as
                                                              in evaluate (unions [c', args ps as', c]) e' -- subs, args, globals
                                                        else error $ "function " <> show f <> "\" must be called with " <> show (length ps) <> " arguments"
                               _ -> error $ "function \"" <> show f <> "\" not found"
                 Var v -> case c !? v of 
                            Just (_ := e') -> if e == e' then error "fook" else evaluate c e'
                            _ -> error $ "variable \"" <> show v <> "\" not found"
                 Val v -> v
 where
  args ps = fromNonEmpty . fmap (uncurry (:=)) . zip ps . fmap Val
  op o = case o of
           Add -> (+)
           Sub -> (-)
           Mul -> (*)
           Div -> (/)

interpret :: Prog -> Double
interpret (Prog c) = maybe (error "no main") (evaluate c . expr) $ c !? "main"
 where
  expr (_ := e) = e
