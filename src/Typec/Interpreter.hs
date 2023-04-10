{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Typec.Interpreter where

import Control.Applicative (liftA2)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty)

import Typec.AST
  (
    Comb ((:=), Fun)
  , Exp ((:+), (:-), (:*), (:/), Exe, Val, Var)
  , Id (Id)
  , Prog (Prog)
  )

evaluateF :: Prog -> Exp -> NonEmpty Exp -> IO Double
evaluateF = undefined
{-
evaluate e
if encountering Var, look it up in:
- subexpressions
- arguments
- globals
-}

evaluate :: Prog -> Exp -> IO Double
evaluate p@(Prog cs) e = case e of
                           a :+ b -> liftA2 (+) (evaluate p a) (evaluate p b)
                           a :- b -> liftA2 (-) (evaluate p a) (evaluate p b)
                           a :* b -> liftA2 (*) (evaluate p a) (evaluate p b)
                           a :/ b -> liftA2 (/) (evaluate p a) (evaluate p b)
                           Exe f as -> case find (fun f) cs of
                                         Just (Fun (Id i) ps e' _) -> if length ps /= length as
                                                                       then error $ "function " <> show i <> "\" must be called with " <> show (length ps) <> " arguments"
                                                                       else evaluateF p e' as
                                                                       -- construct new context
                                                                       -- pass it to evaluate
                                         Nothing -> error $ "function \"" <> show f <> "\" not found"
                           Var v -> case find (ass v) cs of 
                                      Just (_ := e') -> evaluate p e'
                                      Nothing -> error $ "variable \"" <> show v <> "\" not found"
                           Val v -> pure v
 where
  ass (Id i) c = case c of
                   Id i' := _ -> i == i'
                   _ -> False
  fun (Id i) c = case c of
                   Fun (Id i') _ _ _ -> i == i'
                   _ -> False

interpret :: Prog -> IO Double
interpret p@(Prog cs) = maybe (error "no main") (evaluate p . expr) $ find main cs
 where
  main c = case c of
             (Id "main" := _) -> True
             _ -> False
  expr (_ := e) = e
