{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Parse a log message
module Utility.TSLogAnalyzer.MsgParse where

import           ClassyPrelude

import           Data.Attoparsec.Text       (Parser, decimal, string, takeTill)
import qualified Data.Attoparsec.Text       as A

import           Utility.TSLogAnalyzer.Log
import           Utility.TSLogAnalyzer.Util

connParse :: Text -> Maybe Connection
connParse = A.maybeResult . A.parse connectionParser

connectionParser :: Parser Connection
connectionParser = conParser <|> dcnParser

conParser :: Parser Connection
conParser = string "client connected " >>
            mkCON <$> nameParser
                  <*> uidParser
                  <*  string " from "
                  <*> addrParser
                  <*  semicolon

dcnParser :: Parser Connection
dcnParser = string "client disconnected " >>
            mkDCN <$> nameParser
                  <*> uidParser
                  <*  string " reason "
                  <*> rsnParser
                  <*  semicolon

mkCON :: UserName -> UserID -> TLAddr -> Connection
mkCON name uid ip  = Connection CON name uid (Just ip) Nothing

mkDCN :: UserName -> UserID -> Text -> Connection
mkDCN name uid rsn = Connection DCN name uid Nothing (Just rsn)

inParens :: Parser a -> Parser a
inParens p = openParen *> p <* closeParen

untilSingleQuote :: Parser Text
untilSingleQuote = takeTill (== '\'')

nameParser :: Parser UserName
nameParser = UserName <$> (singleQuote *> untilSingleQuote <* singleQuote)

uidParser :: Parser UserID
uidParser = UserID <$> inParens (string "id" *> colon *> decimal)

rsnParser :: Parser Text
rsnParser = do
  singleQuote
  string "reasonmsg="
  initDef <$> A.scan ' ' scanner
  where
    scanner '\'' ';' = Nothing
    scanner _    c   = Just c

addrParser :: Parser TLAddr
addrParser = TLAddr <$> octetParser
                    <*  colon
                    <*> decimal

octetParser :: Parser IP
octetParser = do
  o1 <- decimal
  period
  o2 <- decimal
  period
  o3 <- decimal
  period
  o4 <- decimal
  pure (fromOctets (o1, o2, o3, o4))
