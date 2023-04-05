module Typec.Parser where

import Control.Applicative ((<|>), some)
import Data.List.NonEmpty (fromList)
import Text.Trifecta
  (
    IdentifierStyle (IdentifierStyle), Parser, Result
  , alphaNum, chainl1, char, ident, integerOrDouble, letter, parens, parseString, symbol, token, try
  )

import Typec.AST (Prog, Comb ((:=)), Id (Id), Exp ((:+), (:-), (:*), (:/), Exe, Val, Var))

parseId :: Parser Id
parseId = Id <$> ident style
 where
  style = IdentifierStyle "id" (letter <|> char '_') (alphaNum <|> char '_') mempty minBound maxBound

parseVar :: Parser Exp
parseVar = Var <$> parseId

parseVal :: Parser Exp
parseVal = Val . either fromInteger id <$> integerOrDouble

parseExe :: Parser Exp
parseExe = do
  i <- parseId
  as <- some parseExp
  pure $ Exe i (fromList as)

parseExp :: Parser Exp
parseExp = expr
 where
  expr = chainl1 term addop
  term = chainl1 fact mulop
  fact = parens expr <|> try parseExe <|> try parseVal <|> parseVar
  addop = (:+) <$ symbol "+"
      <|> (:-) <$ symbol "-"
  mulop = (:*) <$ symbol "*"
      <|> (:/) <$ symbol "/"

parseAss :: Parser Comb
parseAss = do
  v <- parseId 
  _ <- token $ char '=' 
  e <- parseExp
  pure $ v := e

parseFun :: Parser Comb
parseFun = undefined
-- parseFun = do
--   f <- parseVar
--   as <- parens $ sepBy parseVar comma
--   es <- braces $ semiSep parseBin
--   pure $ Fun f as es

parseComb :: Parser Comb
parseComb = undefined

parseProg :: Parser Prog
parseProg = undefined
-- parseProg = semiSep (try parseFun <|> try parseAss <|> parseBin) <* eof

parse :: String -> Result Prog
parse = parseString parseProg mempty
