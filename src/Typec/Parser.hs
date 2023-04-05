module Typec.Parser where

import Control.Applicative ((<|>), some)
import Data.List.NonEmpty (fromList)
import Data.Maybe (fromMaybe)

import Data.HashSet (singleton)
import Text.Trifecta
  (
    CharParsing, IdentifierStyle (IdentifierStyle), Parser, Result
  , alphaNum, chainl1, char, ident, integerOrDouble, letter, optional, parens, parseString, reserve, symbol, token, try, semiSep1, eof
  )

import Typec.AST (Prog (Prog), Comb ((:=), Fun), Id (Id), Exp ((:+), (:-), (:*), (:/), Exe, Val, Var))

idStyle :: CharParsing a => IdentifierStyle a
idStyle = IdentifierStyle "id" (letter <|> char '_') (alphaNum <|> char '_') (singleton "where") minBound maxBound

parseId :: Parser Id
parseId = Id <$> ident idStyle

parseVar :: Parser Exp
parseVar = Var <$> parseId

parseVal :: Parser Exp
parseVal = Val . either fromInteger id <$> integerOrDouble

parseExe :: Parser Exp
parseExe = do
  i <- parseId
  as <- some $ try parseVal <|> try parseVar <|> parens (try parseExe <|> parseExp)
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
parseFun = do
  i <- parseId
  ps <- some parseId
  _ <- token $ char '='
  e <- parseExp
  cs <- optional $ reserve idStyle "where" *> some parseComb
  pure $ Fun i (fromList ps) e (fromMaybe [] cs)

parseComb :: Parser Comb
parseComb = try parseFun <|> parseAss

parseProg :: Parser Prog
parseProg = Prog . fromList <$> (semiSep1 parseComb <* eof)

parse :: String -> Result Prog
parse = parseString parseProg mempty
