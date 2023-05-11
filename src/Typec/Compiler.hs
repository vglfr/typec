{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Typec.Compiler where

import Prelude hiding (div, lookup)

import Data.List (elemIndex, intercalate)
import Data.Maybe (fromJust)

import Data.HashMap.Strict (HashMap, insert, lookup, size, toList)
import System.Process (readProcess)

import Typec.AST (Exp (Bin, Val), Op (Add, Div, Mul, Sub))

type Line = String
type Tape = ([Ins], HashMap Double Int)

data Ins
  = Two Op Val Val

data Val
  = Con Int
  | Ref Int

instance Show Ins where
  show (Two o a b) = show a <> show o <> show b

instance Show Val where
  show (Con x) = "C" <> show x
  show (Ref x) = "[" <> show x <> "]"

isCon :: Val -> Bool
isCon v = case v of
            Con _ -> True
            _ -> False

ref :: Val -> Int
ref (Ref x) = x

spool :: Exp -> Tape
spool t = let es = flat t
           in foldr (acc es) mempty es
 where
  flat e = case e of
             Bin _ a b -> flat a <> flat b <> [e]
             _ -> mempty
  acc es e (is, m) = case e of
                       Bin o a b -> let (v1, m1) = val a m  es e
                                        (v2, m2) = val b m1 es e
                                     in (Two o v1 v2 : is, m2)
                       _ -> undefined
  val e m es e' = case e of
                    Val v -> let (c, m') = case lookup v m of
                                             Just x -> (x, m)
                                             Nothing -> let s = size m
                                                         in (s, insert v s m)
                              in (Con c, m')
                    _ -> (Ref $ index e es - index e' es, m)
  index e es = fromJust $ elemIndex e es

compile :: Tape -> String
compile (is, m) = unlines $ intercalate (pure mempty)
  [
    global
  , section ".data" (data' m)
  , section ".bss"   bss
  , section ".text" (text is)
  ]

global :: [Line]
global =
  [
    "global main"
  , "extern printf"
  ]

section :: String -> [Line] -> [Line]
section s is = "section " <> s : fmap offset is
 where
  offset cs = if last cs == ':'
              then cs
              else "        " <> cs

data' :: HashMap Double Int -> [Line]
data' m = "FST:        db \"%.2f\", 10, 0" : fmap (uncurry fconst) (toList m)
 where
  fconst k v = "C" <> show v <> ":         dq " <> show k

bss :: [Line]
bss = pure "RES:        resq 1"

text :: [Ins] -> [Line]
text is = main <> concatMap block is <> fstp <> printf <> exit
 where
  block i = comment (show i) : instr i
  instr (Two o a b)
    | isCon a && isCon b = [fld a, op1 o b]
    | isCon a = [fld a, fxch, op0 o]
    | isCon b = pure $ op1 o b
    | ref a < ref b = [op0 o]
    | ref a > ref b = [fxch, op0 o]
  fld v = "fld         qword [" <> show v <> "]"
  op1 o v = case o of
              Add -> "fadd        qword [" <> show v <> "]"
              Sub -> "fsub        qword [" <> show v <> "]"
              Mul -> "fmul        qword [" <> show v <> "]"
              Div -> "fdiv        qword [" <> show v <> "]"
  fxch = "fxch"
  op0 o = case o of
            Add -> "faddp"
            Sub -> "fsubp"
            Mul -> "fmulp"
            Div -> "fdivp"

comment :: String -> Line
comment s = "; " <> s

main :: [Line]
main = pure "main:"

fstp :: [Line]
fstp =
  [
    comment "fstp"
  , "fstp        qword [RES]"
  ]

printf :: [Line]
printf =
  [
    comment "printf"
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
    comment "exit"
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
