{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Typec.Compiler where

import Prelude hiding (div)

import Data.List (elemIndex, intercalate, lookup, nub)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Numeric (showHex)

import Data.Graph.Inductive (Adj, Context, Gr, buildGr, postorder, dff, labNodes, lab)
import Data.HashMap.Strict ((!?), HashMap, insert, lookup, size, toList)
import Data.Hashable (hash)
import System.Directory (createDirectoryIfMissing)
import System.Process (readProcess)

import Typec.AST
  (
    Comb ((:=), Fun)
  , Exp (Bin, Val, Var)
  , Id (Id)
  , Op (Add, Div, Mul, Sub)
  , Prog (Prog)
  )

type Module = (String, String)

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
  | Tem String

instance Show Ins where
  show (Two o a b) = show a <> show o <> show b
  show (Loa v) = show v
  show (Sav v) = v

instance Show Val where
  show (Con x) = "C" <> show x
  show (Ref x) = "[" <> show x <> "]"
  show (Tem x) = x

class Spool a where
  spool :: Cons -> a -> Tape

instance Spool Exp where
  spool cs t = let es = flat t
                in foldr (acc es) (mempty,cs,mempty) es
   where
    flat e = case e of
               Bin _ a b -> flat' a <> flat' b <> [e]
               Val _ -> [e]
               Var _ -> [e]
               _ -> mempty
    flat' e = case e of
                Bin {} -> flat e
                _ -> mempty
    acc es e (is, cs', vs) = case e of
                              Bin o a b -> let (v1, cs1) = val a cs' es e
                                               (v2, cs2) = val b cs1 es e
                                            in (Two o v1 v2 : is, cs2, vs)
                              Val v -> let (c, cs1) = lookupd v cs'
                                        in (Loa (Con c) : is, cs1, vs)
                              Var (Id v) -> (Loa (Tem v) : is, cs', vs)
                              _ -> undefined
    val e cs' es e' = case e of
                        Val v -> let (c, cs1) = lookupd v cs'
                                  in (Con c, cs1)
                        Var (Id i) -> (Tem i, cs')
                        _ -> (Ref $ index e es - index e' es, cs')
    index e es = fromJust $ elemIndex e es
    lookupd k m = case Data.HashMap.Strict.lookup k m of
                    Just v  -> (v, m)
                    Nothing -> let s = size m in (s, insert k s m)

instance Spool Comb where
  spool cs ((Id i) := e) = let (is, cs', _) = spool cs e
                            in if i == "main"
                               then (is,cs',mempty)
                               else (is <> [Sav i], cs', [i])

instance Spool Prog where
  spool _ p@(Prog c) = let cs' = flatten . graph $ p
                        in foldr acc mempty cs'
   where
    acc s (is,cs,vs) = let (is',cs',vs') = spool cs . fromJust $ c !? Id s
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
flatten g = fmap (fromJust . lab g) . postorder . head . dff [main''] $ g
 where
  main'' = fromJust . Data.List.lookup "main" . fmap swap . labNodes $ g
  -- throw "cyclic reference" on bidirectioned edge

section :: String -> [Line] -> [Line]
section s is = "section " <> s : fmap offset is
 where
  offset cs = if last cs == ':'
              then cs
              else "        " <> cs

data' :: Cons -> [Line]
data' cs = fmap (uncurry fconst) (toList cs)
 where
  fconst k v = "C" <> show v <> ":         dq " <> show k

bss :: Vars -> [Line]
bss vs = "RES:        resq 1" : fmap fvar vs
 where
  fvar v = v <> ":          resq 1"

text :: [Ins] -> [Line]
text is = main' <> concatMap block is
 where
  block i = comment (show i) : instr i
  instr (Two o a b)
    | notRef a && notRef b = [fld a, op1 o b]
    | notRef a = [fld a, fxch, op0 o]
    | notRef b = pure $ op1 o b
    | ref a < ref b = [op0 o]
    | ref a > ref b = [fxch, op0 o]
  instr (Loa c) = pure $ fld c
  instr (Sav v) = pure ("fstp        qword [" <> v <> "]")
  notRef v = case v of
               Ref _ -> False
               _ -> True
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
  main' = pure "main:"

comment :: String -> Line
comment s = "; " <> s

global :: String -> [Line]
global s = pure $ "global " <> s

extern :: String -> [Line]
extern s = pure $ "extern " <> s

printf64 :: Module
printf64 = ("_printf_f64",) . unlines . intercalate (pure mempty) $
  [
    global "_printf_f64"
  , extern "printf"
  , data''
  , text'
  ]
 where
  data'' =
    [
      "section .data"
    , "        FST:        db \"%.2f\", 10, 0"
    ]
  text' =
    [
      "section .text"
    , "_printf_f64:"
    , "        push        rbp"
    , "        mov         rbp, rsp"
    , ""
    , "        mov         rdi, FST"
    , "        mov         rax, 1"
    , "        movsd       xmm0, qword [rbp+16]"
    , "        call        printf"
    , ""
    , "        pop         rbp"
    , "        xor         rax, rax"
    , "        ret"
    ]

main :: Tape -> Module
main (is, cs, vs) = ("main",) . unlines . intercalate (pure mempty) $
  [
    global "main"
  , extern "_printf_f64"
  , section ".data" (data' cs)
  , section ".bss"  (bss vs)
  , section ".text" (text is) <> print'
  ]
 where
  print' =
    [
      ""
    , "        fstp        qword [RES]"
    , "        mov         rax, [RES]"
    , ""
    , "        push        rax"
    , "        call        _printf_f64"
    , ""
    , "        add         rsp, 8"
    , "        ret"
    ]

compile :: Tape -> [Module]
compile t = [printf64, main t]

run :: [Module] -> String -> IO ()
run ms s = do
  mapM_ (createDirectoryIfMissing True) [aDir, oDir]
  mapM_ (\(_,c) -> putStrLn c) ms
  mapM_ (\(n,c) -> writeFile (aDir <> n <> ".s") c) ms
  mapM_ (\(n,_) -> readProcess "nasm" ["-g", "-f", "elf64", aDir <> n <> ".s", "-o", oDir <> n <> ".o"] mempty) ms
  readProcess "gcc" (["-z", "noexecstack", "-o", hDir <> "a.out"] <> map (\(n,_) -> oDir <> n <> ".o") ms) mempty >>= putStrLn
  readProcess (hDir <> "a.out") mempty mempty >>= putStrLn
 where
  aDir = hDir <> "/asm/"
  oDir = hDir <> "/obj/"
  hDir = "/tmp/typec/" <> s <> "-" <> showHex (abs . hash . concatMap snd $ ms) mempty <> "/"
