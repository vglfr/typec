{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Typec.Compiler where

import Prelude hiding (div)

import Data.List (elemIndex, intercalate, lookup, nub)
import Data.Maybe (fromJust)

import Data.Graph.Inductive (Adj, Context, Gr, buildGr, postorder, dff, labNodes, lab)
import Data.HashMap.Strict ((!?), HashMap, insert, lookup, size, toList)
import System.Process (readProcess)

import Typec.AST
  (
    Comb ((:=), Fun)
  , Exp (Bin, Val, Var)
  , Id (Id)
  , Op (Add, Div, Mul, Sub)
  , Prog (Prog)
  )
import Data.Tuple (swap)

type Line = String
type Cons = HashMap Double Int
type Vars = [String]
type Tape = ([Ins], Cons, Vars)

data Ins
  = Two Op Val Val
  | Loa Val
  | Sav String

data Val
  = Con Int
  | Ref Int

instance Show Ins where
  show (Two o a b) = show a <> show o <> show b
  show (Loa v) = show v
  show (Sav v) = "$" <> v

instance Show Val where
  show (Con x) = "C" <> show x
  show (Ref x) = "[" <> show x <> "]"

class Spool a where
  spool :: a -> Tape

instance Spool Exp where
  spool t = let es = flat t
             in foldr (acc es) mempty es
   where
    flat e = case e of
               Bin _ a b -> flat' a <> flat' b <> [e]
               Val _ -> [e]
               _ -> mempty
    flat' e = case e of
                Bin _ _ _ -> flat e
                _ -> mempty
    acc es e (is, cs, vs) = case e of
                              Bin o a b -> let (v1, cs1) = val a cs  es e
                                               (v2, cs2) = val b cs1 es e
                                            in (Two o v1 v2 : is, cs2, vs)
                              Val v -> let (c, cs') = lookupd v cs
                                        in (Loa (Con c) : is, cs', vs)
                              _ -> undefined
    val e cs es e' = case e of
                       Val v -> let (c, cs') = lookupd v cs
                                 in (Con c, cs')
                       _ -> (Ref $ index e es - index e' es, cs)
    index e es = fromJust $ elemIndex e es
    lookupd k m = case Data.HashMap.Strict.lookup k m of
                    Just v  -> (v, m)
                    Nothing -> let s = size m in (s, insert k s m)

instance Spool Comb where
  spool ((Id i) := e) = let (is, cs, _) = spool e
                         in if i == "main"
                            then (is,cs,mempty)
                            else (is <> [Sav i], cs, [i])

instance Spool Prog where
  spool p@(Prog c) = let cs' = flatten . graph $ p
                       in foldr acc mempty cs'
   where
    acc s (is,cs,vs) = let (is',cs',vs') = spool . fromJust $ c !? Id s
                        in (is' <> is,cs' <> cs,vs' <> vs)

graph :: Prog -> Gr String String
graph (Prog cs) = let (ks,vs) = unzip . toList $ cs
                   in buildGr . foldr (acc ks) mempty . zip [0..size cs - 1] $ vs
 where
  acc :: [Id] -> (Int,Comb) -> [Context String String] -> [Context String String]
  acc is (n,c) a = case c of
                     Id i := e        -> ([],n,i,vars e is) : a
                     Fun (Id i) _ e _ -> ([],n,i,vars e is) : a
  vars :: Exp -> [Id] -> Adj String
  vars e is = case e of
                Bin _ e1 e2  -> nub $ vars e1 is <> vars e2 is
                Var i@(Id v) -> [(v,fromJust $ elemIndex i is)]
                Val _ -> mempty
  -- throw "undefined reference" on lookup error

flatten :: Gr String String -> [String]
flatten g = fmap (fromJust . lab g) . postorder . head . dff [main'] $ g
 where
  main' = fromJust . Data.List.lookup "main" . fmap swap . labNodes $ g
  -- throw "cyclic reference" on bidirectioned edge

compile :: Tape -> String
compile (is, cs, vs) = unlines $ intercalate (pure mempty)
  [
    global
  , section ".data" (data' cs)
  , section ".bss"  (bss vs)
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

data' :: Cons -> [Line]
data' cs = "FST:        db \"%.2f\", 10, 0" : fmap (uncurry fconst) (toList cs)
 where
  fconst k v = "C" <> show v <> ":         dq " <> show k

bss :: Vars -> [Line]
bss vs = "RES:        resq 1" : fmap fvar vs
 where
  fvar v = v <> ":          resq 1"

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
  instr (Loa c) = pure $ fld c
  instr (Sav v) = pure ("fstp        qword [" <> v <> "]")
  isCon v = case v of
              Con _ -> True
              _ -> False
  ref (Ref x) = x
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
