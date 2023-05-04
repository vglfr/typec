{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Typec.Compiler where

import Prelude hiding (div)

import Data.List (elemIndex)
import Data.Maybe (fromJust)

-- import System.Process (readProcess)
import Typec.AST (Exp (Bin, Val), Op (Add, Div, Mul, Sub))

{-
declare :: String
declare = unlines
  [
    "global main"
  , "extern printf"
  ]

global :: String
global = unlines
  [
    "FSTR db \"%i\", 10, 0"
  ]

{-

(4 - 2 * 6 / 1) * 3 + 1 / ((2 - 3) * 3 / (1 + 2))

; 1 + 2 = 3
mov         rbx, 1
add         rbx, 2

-}

add :: Int -> Int -> String
add a b = unlines . fmap offset $
  [
    "; " <> show a <> " + " <> show b <> " = " <> show (a + b)
  , "mov         rax, " <> show a
  , "add         rax, " <> show b
  ]

main :: String
main = unlines
  [
    "section .text"
  , "main:"
  ] <> add 1 2 <> printf <> exit

printf :: String
printf = unlines . fmap offset $
  [
    "; printf"
  , "push        rbp"
  , "mov         rdi, FSTR"
  , "mov         rsi, -8"
  , "xor         rax, rax"
  , "call        printf"
  , "pop         rbp"
  , "xor         rax, rax"
  , "ret"
  ]

exit :: String
exit = unlines . fmap offset $
  [
    "; exit"
  , "mov         rax, 60"
  , "xor         rdi, rdi"
  , "syscall"
  ]

compile :: Prog -> String
compile _ = unlines [declare, global, main]

run :: String -> IO ()
run s = do
  putStrLn s
  writeFile "/tmp/temp.s" s
  readProcess "nasm" ["-f", "elf64", "-o", "/tmp/temp.o", "/tmp/temp.s"] mempty >>= putStrLn
  readProcess "gcc" ["-z", "noexecstack", "-o", "/tmp/temp", "-lc", "/tmp/temp.o"] mempty >>= putStrLn
  readProcess "/tmp/temp" mempty mempty >>= putStrLn
-}

data Ins
  = Two Op Val Val

data Val
  = Imm Double
  | Ref Int

instance Show Ins where
  show (Two o a b) = show a <> show o <> show b

instance Show Val where
  show (Imm x) = show x
  show (Ref x) = "[" <> show x <> "]"

offset :: String -> String
offset = ("        " <>)

spool :: Exp -> [Ins]
spool t = let es = flat t
           in fmap (ref es) es
 where
  flat e = case e of
             Bin _ a b -> flat a <> flat b <> [e]
             _ -> mempty
  ref es e = case e of
               Bin o a b -> Two o (val a es e) (val b es e)
               _ -> undefined
  val e es e' = case e of
                  Val x -> Imm x
                  _ -> Ref $ index e es - index e' es 
  index e es = fromJust $ elemIndex e es

compile :: [Ins] -> String
compile = unlines . fmap ins

ins :: Ins -> String
ins i@(Two o a b) = unlines . fmap offset $
     comm i
  <> stage a b
  <> invoke o b
  <> push

comm :: Ins -> [String]
comm i = pure $ "; " <> show i

stage :: Val -> Val -> [String]
stage a b = case a of
              Imm _ -> case b of
                         Imm _ -> mov "rax" a
                         Ref _ -> mov "rax" a <> pop "rbx"
              Ref r -> case b of
                         Imm _ -> pop "rax"
                         Ref r' -> if r > r'
                                   then pop "rax" <> pop "rbx"
                                   else pop "rbx" <> pop "rax"

invoke :: Op -> Val -> [String]
invoke o v = case o of
               Add -> add v
               Sub -> sub v
               Mul -> mul v
               Div -> div v

mov :: String -> Val -> [String]
mov r (Imm v) = pure $ "mov         " <> r <> ", " <> show v

pop :: String -> [String]
pop r = pure $ "pop         " <> r

add :: Val -> [String]
add v = pure $ case v of
                 Imm i -> "add         rax, " <> show i
                 Ref _ -> "add         rax, rbx"

sub :: Val -> [String]
sub v = pure $ case v of
                 Imm i -> "sub         rax, " <> show i
                 Ref _ -> "sub         rax, rbx"

mul :: Val -> [String]
mul v = case v of
          Imm i -> ["mul         rax, " <> show i, "cqo"]
          Ref _ -> ["mul         rax, rbx"       , "cqo"]

div :: Val -> [String]
div v = case v of
          Imm i -> ["div         rax, " <> show i, "cqo"]
          Ref _ -> ["div         rax, rbx"       , "cqo"]

push :: [String]
push = pure $ "push        rax"
