module Main where

main :: IO ()
main = putStrLn "typec"
-- import System.Environment (getArgs)
-- import System.Exit (die)
-- import System.FilePath (takeBaseName)
-- import System.Process (readProcess)
-- import Text.Trifecta (Result (Failure, Success))

-- import Numc.Codegen (boilerplate, writeBin, writeLL)
-- import Numc.Compiler (compile)
-- import Numc.Parser (parse)

-- main :: IO ()
-- main = do
--   as <- getArgs
--   p <- if null as
--          then die "Path to Num file needs to be provided as a first argument"
--          else pure $ head as
--   s <- readFile p
--   f <- case parse s of
--           Success e -> pure e
--           Failure e -> die $ show e
--   let m = boilerplate $ compile f
--       b = takeBaseName p
--   writeLL  m ("ll/" <> b <> ".ll")
--   writeBin m ("bin/" <> b)

--   readProcess "bat" [p] mempty >>= putStrLn . ("---\n" <>) . init
--   readProcess "bat" ["--color=always", "ll/" <> b <> ".ll"] mempty >>= putStrLn . ("---" <>) . init
--   readProcess ("./bin/" <> b) [] mempty >>= putStrLn . ("---\n" <>) . (<> "\n---")