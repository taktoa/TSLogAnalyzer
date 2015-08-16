{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}

-- | Parse a log message
module Utility.TSLogAnalyzer.MsgParse (connParse) where

import           Data.Attoparsec.Text       (Parser, decimal, string, takeTill)
import qualified Data.Attoparsec.Text       as A

import           ClassyPrelude

import           Utility.TSLogAnalyzer.Log
import           Utility.TSLogAnalyzer.Util

import           Prelude.Unicode

connParse ∷ Text → Maybe Connection
connParse = A.maybeResult . A.parse connectionParser

connectionParser ∷ Parser Connection
connectionParser = conParser <|> dcnParser

conParser ∷ Parser Connection
conParser = string "client connected " >>
            mkCON <$> nameParser
                  <*> uidParser
                  <*  string " from "
                  <*> ipParser
                  <*  semicolon

dcnParser ∷ Parser Connection
dcnParser = string "client disconnected " >>
            mkDCN <$> nameParser
                  <*> uidParser
                  <*  string " reason "
                  <*> rsnParser
                  <*  semicolon

mkCON ∷ Text → UserID → IP → Connection
mkCON name uid ip  = Connection CON name uid (Just ip) Nothing

mkDCN ∷ Text → UserID → Text → Connection
mkDCN name uid rsn = Connection DCN name uid Nothing   (Just rsn)

inParens ∷ Parser a → Parser a
inParens p = openParen *> p <* closeParen

untilSingleQuote ∷ Parser Text
untilSingleQuote = takeTill (≡ '\'')

nameParser ∷ Parser Text
nameParser = singleQuote *> untilSingleQuote <* singleQuote

uidParser ∷ Parser UserID
uidParser = UserID <$> inParens (string "id" <* colon *> decimal)

rsnParser ∷ Parser Text
rsnParser =     singleQuote
            <*  string "reasonmsg="
             *> untilSingleQuote
            <*  singleQuote

ipParser ∷ Parser IP
ipParser = mkIP <$> octetParser
                <*  colon
                <*> decimal

octetParser ∷ Parser (Int, Int, Int, Int)
octetParser = (,,,) <$> decimal
                    <*  period
                    <*> decimal
                    <*  period
                    <*> decimal
                    <*  period
                    <*> decimal
