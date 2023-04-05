-- {-# LANGUAGE TypeApplications #-}

module Main where

-- import Control.Exception (try)
-- import Data.List (nub)
-- import System.Exit (exitSuccess)
-- import System.IO (hFlush, stdout)

-- import Text.Trifecta (Result (Failure, Success))

-- import Numc.AST (Expr ((:=), Val))
-- import Numc.Compiler (compile, isVoid, vars)
-- import Numc.JIT (boilerplate, jit)
-- import Numc.Parser (parse)

main :: IO ()
main = putStrLn "typei"
-- main :: IO [Expr]
-- main = main' []
--  where
--   main' c = do
--     s <- ps1 >> try @IOError getLine >>= handleEOF
--     f <- case parse s of
--            Success e -> pure e
--            Failure e -> print e >> main' c
--     let m  = boilerplate $ compile (c <> f)
--         vs = vars m
--     p <- jit m $ length vs
--     case p of
--       Right (r, vs') -> let c' = nub . fmap (uncurry (:=)) . zip vs $ fmap Val vs'
--                          in if isVoid m
--                             then main' c'
--                             else print r >> main' c'
--       Left e -> print e >> main' c
--   ps1 = putStr "> " >> hFlush stdout
--   handleEOF = either (const exitSuccess) pure