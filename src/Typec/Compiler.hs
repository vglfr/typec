module Typec.Compiler where

import System.Process (readProcess)
import Typec.AST (Exp, Prog)

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

offset :: String -> String
offset = ("        " <>)

compile :: Prog -> String
compile _ = unlines [declare, global, main]

spool :: Exp -> [Exp]
spool = undefined

run :: String -> IO ()
run s = do
  putStrLn s
  writeFile "/tmp/temp.s" s
  readProcess "nasm" ["-f", "elf64", "-o", "/tmp/temp.o", "/tmp/temp.s"] mempty >>= putStrLn
  readProcess "gcc" ["-z", "noexecstack", "-o", "/tmp/temp", "-lc", "/tmp/temp.o"] mempty >>= putStrLn
  readProcess "/tmp/temp" mempty mempty >>= putStrLn
