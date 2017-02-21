{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module YANG.Internal where

import Control.Monad.Identity (Identity)
-- import Data.Char (digitToInt)
-- import Data.List (foldl')
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text


namespace :: Parsec T.Text () T.Text
namespace = do
  spaces
  string "namespace"
  spaces
  char '"'
  ns <- manyTill anyChar $ char '"'
  char ';'
  return $ T.pack ns


prefix :: Parsec T.Text () T.Text
prefix = stringKeyword "prefix"

revisionDate :: Parsec T.Text () T.Text
revisionDate = stringKeyword "revision-date"

organisation :: Parsec T.Text () T.Text
organisation = stringKeyword "organisation"

include :: Parsec T.Text () (T.Text, T.Text)
include = do
  mod <- stringKeyword "include"
  spaces
  char '{'
  rd <- revisionDate
  spaces
  char '}'
  return (mod, rd)


stringKeyword :: String -> Parsec T.Text () T.Text
stringKeyword keyword = do
  optional spaces
  string keyword
  spaces
  str <- quotedString <|> nonQuotedString
  optional $ char ';'
  return str

quotedString :: Parsec T.Text () T.Text
quotedString = do
  char '"'
  str <- manyTill anyChar $ char '"'
  return $ T.pack str

nonQuotedString :: Parsec T.Text () T.Text
nonQuotedString = T.pack <$> many1 (noneOf " ;")

parse :: T.Text -> T.Text
parse = id

