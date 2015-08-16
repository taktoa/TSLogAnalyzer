{-# LANGUAGE TupleSections #-}

-- | Parse a log message
module Utility.TSLogAnalyzer.MsgParse (connParse) where

import           Data.Attoparsec.Text      (Parser, char, decimal, string,
                                            takeTill)
import qualified Data.Attoparsec.Text      as A
import           Data.Text                 (Text)
import           Utility.TSLogAnalyzer.Log

import           Control.Monad             (void)

import           Prelude.Unicode

singleQuote, doubleQuote ∷ Parser ()
openParen, closeParen    ∷ Parser ()
colon, semicolon, period ∷ Parser ()

singleQuote = void $ char '\''
doubleQuote = void $ char '"'
openParen   = void $ char '('
closeParen  = void $ char ')'
colon       = void $ char ':'
semicolon   = void $ char ';'
period      = void $ char '.'

inParens ∷ Parser a → Parser a
inParens p = openParen *> p <* closeParen

untilSingleQuote ∷ Parser Text
untilSingleQuote = takeTill (≡ '\'')

ipParse ∷ Parser IP
ipParse = mkIP <$> octetParse
               <*  colon
               <*> decimal

octetParse ∷ Parser (Int, Int, Int, Int)
octetParse = (,,,) <$> decimal
                   <*  period
                   <*> decimal
                   <*  period
                   <*> decimal
                   <*  period
                   <*> decimal

nameParse ∷ Parser Text
nameParse = singleQuote *> untilSingleQuote <* singleQuote

uidParse ∷ Parser UserID
uidParse = UserID <$> inParens (string "id" <* colon *> decimal)

rsnParse ∷ Parser Text
rsnParse =     singleQuote
           <*  string "reasonmsg="
            *> untilSingleQuote
           <*  singleQuote

mkDCN ∷ Text → UserID → Text → Connection
mkDCN name uid rsn = Connection DCN name uid Nothing   (Just rsn)

mkCON ∷ Text → UserID → IP → Connection
mkCON name uid ip  = Connection CON name uid (Just ip) Nothing

dcnParser ∷ Parser Connection
dcnParser = string "client disconnected " >>
            mkDCN <$> nameParse
                  <*> uidParse
                  <*  string " reason "
                  <*> rsnParse
                  <*  semicolon

conParser ∷ Parser Connection
conParser = string "client connected " >>
            mkCON <$> nameParse
                  <*> uidParse
                  <*  string " from "
                  <*> ipParse
                  <*  semicolon

connectionParser ∷ Parser Connection
connectionParser = A.choice [conParser, dcnParser]

connParse ∷ Text → Maybe Connection
connParse = A.maybeResult . A.parse connectionParser
