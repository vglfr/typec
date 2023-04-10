{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Typec.Interpreter where

import Prelude hiding (zip)

import Control.Applicative (liftA2)
import Data.List.NonEmpty (NonEmpty, zip)

import Data.HashMap.Strict ((!?), HashMap, unions)

import Typec.AST
  (
    Comb ((:=), Fun)
  , Exp ((:+), (:-), (:*), (:/), Exe, Val, Var)
  , Id
  , Prog (Prog)
  )
import Typec.Parser (fromNonEmpty)

evaluate :: HashMap Id Comb -> Exp -> IO Double
evaluate c e = case e of
                 a :+ b -> liftA2 (+) (evaluate c a) (evaluate c b)
                 a :- b -> liftA2 (-) (evaluate c a) (evaluate c b)
                 a :* b -> liftA2 (*) (evaluate c a) (evaluate c b)
                 a :/ b -> liftA2 (/) (evaluate c a) (evaluate c b)
                 Exe f as -> case c !? f of
                               Just (Fun _ ps e' c') -> if length ps /= length as
                                                        then error $ "function " <> show f <> "\" must be called with " <> show (length ps) <> " arguments"
                                                        else do
                                                          as' <- traverse (evaluate c) as -- eager b/c lazy errors
                                                          evaluate (unions [c', args ps as', c]) e' -- subs, args, globals
                               _ -> error $ "function \"" <> show f <> "\" not found"
                 Var v -> case c !? v of 
                            Just (_ := e') -> if e == e' then error "fook" else evaluate c e'
                            _ -> error $ "variable \"" <> show v <> "\" not found"
                 Val v -> pure v
 where
  args :: NonEmpty Id -> NonEmpty Double -> HashMap Id Comb
  args ps = fromNonEmpty . fmap (uncurry (:=)) . zip ps . fmap Val

interpret :: Prog -> IO Double
interpret (Prog c) = maybe (error "no main") (evaluate c . expr) $ c !? "main"
 where
  expr (_ := e) = e
