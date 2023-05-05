{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Typec.Compiler where

import Prelude hiding (div)

import Data.List (elemIndex)
import Data.Maybe (fromJust)

import System.Process (readProcess)

import Typec.AST (Exp (Bin, Val), Op (Add, Div, Mul, Sub))

type Line = String

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
compile is = unlines $
     global
  <> section ".data" (data' is)
  <> section ".bss"  (bss is)
  <> section ".text" (text is)

global :: [Line]
global =
  [
    "global main"
  , "extern printf"
  ]

section :: String -> [Line] -> [Line]
section s is = "section " <> s : fmap offset is
 where offset = ("        " <>)

  -- <> (instrs is <> printf <> exit)

data' :: [Ins] -> [Line]
data' = undefined

bss :: [Ins] -> [Line]
bss = undefined

text :: [Ins] -> [Line]
text = undefined

instrs :: [Ins] -> [Line]
instrs = concatMap instr

instr :: Ins -> [Line]
instr i@(Two o a b)
  =  comm i
  <> stage a b
  <> invoke o b
  <> push

comm :: Ins -> [Line]
comm i = pure $ "; " <> show i

stage :: Val -> Val -> [Line]
stage a b = case a of
              Imm _ -> case b of
                         Imm _ -> mov "rax" a
                         Ref _ -> mov "rax" a <> pop "rbx"
              Ref r -> case b of
                         Imm _ -> pop "rax"
                         Ref r' -> if r > r'
                                   then pop "rax" <> pop "rbx"
                                   else pop "rbx" <> pop "rax"

invoke :: Op -> Val -> [Line]
invoke o v = case o of
               Add -> add v
               Sub -> sub v
               Mul -> mul v
               Div -> div v

mov :: String -> Val -> [Line]
mov r (Imm v) = pure $ "mov         " <> r <> ", " <> show (fromEnum v)

pop :: String -> [Line]
pop r = pure $ "pop         " <> r

add :: Val -> [Line]
add v = pure $ case v of
                 Imm i -> "add         rax, " <> show (fromEnum i)
                 Ref _ -> "add         rax, rbx"

sub :: Val -> [Line]
sub v = pure $ case v of
                 Imm i -> "sub         rax, " <> show (fromEnum i)
                 Ref _ -> "sub         rax, rbx"

mul :: Val -> [Line]
mul v = case v of
          Imm _ -> mov "rbx" v <> ["imul        rbx"]
          Ref _ ->                ["imul        rbx"]

div :: Val -> [Line]
div v = case v of
          Imm _ -> mov "rbx" v <> ["cqo", "idiv        rbx"]
          Ref _ ->                ["cqo", "idiv        rbx"]

push :: [Line]
push = pure $ "push        rax"

-- global :: [Line]
-- global =
--   [
--     "FST: db \"%i\", 10, 0"
--   ]

main :: [Line]
main =
  [
    -- "section .text"
    "main:"
  ]

printf :: [Line]
printf =
  [
    "; printf"
  , "push        rbp"
  , "movsd       xmm0, qword [RES]"
  , "mov         rdi, FST"
  , "mov         rax, 1"
  , "call        printf"
  , "pop         rbp"
  , "xor         rax, rax"
  , "ret"
  ]

exit :: [Line]
exit =
  [
    "; exit"
  , "mov         rax, 60"
  , "xor         rdi, rdi"
  , "syscall"
  ]

run :: String -> IO ()
run s = do
  putStrLn s
  writeFile "/tmp/temp.s" s
  readProcess "nasm" ["-f", "elf64", "-o", "/tmp/temp.o", "/tmp/temp.s"] mempty >>= putStrLn
  readProcess "gcc" ["-z", "noexecstack", "-o", "/tmp/temp", "-lc", "/tmp/temp.o"] mempty >>= putStrLn
  readProcess "/tmp/temp" mempty mempty >>= putStrLn
