module Typec.Parser where

import Control.Applicative ((<|>), some)
import Data.List.NonEmpty (fromList, toList)

import Data.HashSet (singleton)
import Text.Trifecta
  (
    CharParsing, IdentifierStyle (IdentifierStyle), Parser, Result, TokenParsing
  , alphaNum, chainl1, char, eof, ident, integerOrDouble, letter, newline, optional
  , parens, parseString, reserve, runUnlined, sepEndByNonEmpty, spaces, symbol, try
  )

import Typec.AST (Comb ((:=), Fun), Exp ((:+), (:-), (:*), (:/), Exe, Val, Var), Id (Id), Prog (Prog))

idStyle :: CharParsing a => IdentifierStyle a
idStyle = IdentifierStyle "id" (letter <|> char '_') (alphaNum <|> char '_') (singleton "where") minBound maxBound

parseId :: (Monad m, TokenParsing m) => m Id
parseId = Id <$> runUnlined (ident idStyle)

parseVar :: (Monad m, TokenParsing m) => m Exp
parseVar = Var <$> parseId

parseVal :: (Monad m, TokenParsing m) => m Exp
parseVal = Val . either fromInteger id <$> runUnlined integerOrDouble

parseExe :: (Monad m, TokenParsing m) => m Exp
parseExe = do
  i <- parseId
  as <- some $ try parseVal <|> try parseVar <|> parens (try parseExe <|> parseExp)
  pure $ Exe i (fromList as)

parseExp :: (Monad m, TokenParsing m) => m Exp
parseExp = runUnlined expr
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
  _ <- runUnlined $ symbol "="
  e <- parseExp
  pure $ v := e

parseFun :: Parser Comb
parseFun = do
  i <- parseId
  ps <- some parseId
  _ <- runUnlined $ symbol "="
  e <- parseExp
  cs <- optional . try $ newline *> some (char ' ') *> runUnlined (reserve idStyle "where")
                      *> newline *> sepEndByNonEmpty (some (char ' ') *> parseComb) newline
  pure $ Fun i (fromList ps) e (maybe [] toList cs)

parseComb :: Parser Comb
parseComb = try parseFun <|> parseAss

parseProg :: Parser Prog
parseProg = Prog <$> sepEndByNonEmpty parseComb spaces <* eof

parse :: String -> Result Prog
parse = parseString parseProg mempty
