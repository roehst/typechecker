{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Term
import Text.Parsec
import Ty

-- Type signature parser
parseTyConst :: Monad m => ParsecT String () m Ty
parseTyConst = do
  c <- many1 letter
  spaces
  return $ TyConst c

parseTyArr :: Monad m => ParsecT String () m (Ty -> Ty -> Ty)
parseTyArr = do
  string "->"
  spaces
  return TyArr

parens :: Monad m => ParsecT String () m a -> ParsecT String () m a
parens = between (char '(') (char ')')

parseTy :: Monad m => ParsecT String () m Ty
parseTy = parseTyConst <|> parens parseTyConst `chainl1` parseTyArr

-- Term parser
parseVar :: Monad m => ParsecT String () m Term
parseVar = do
  c <- many1 letter
  spaces
  return $ Var c

parseLam :: Monad m => ParsecT String () m Term
parseLam = do
  string "\\"
  spaces
  c <- many1 letter
  spaces
  string ":"
  spaces
  t <- parseTy
  spaces
  string "."
  spaces
  e <- parseTerm
  spaces
  return $ Lam c t e

parseApp :: Monad m => ParsecT String () m (Term -> Term -> Term)
parseApp = do
  spaces
  return App

parseTerm :: Monad m => ParsecT String () m Term
parseTerm = parseVar <|> parseLam <|> parens parseTerm `chainl1` parseApp