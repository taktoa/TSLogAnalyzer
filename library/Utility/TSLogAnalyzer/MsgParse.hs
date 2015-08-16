{-# LANGUAGE TupleSections #-}

-- | Parse a log message
module Utility.TSLogAnalyzer.MsgParse (connParse) where

import           Data.Attoparsec.Text      (Parser)
import qualified Data.Attoparsec.Text      as A
import           Data.Text                 (Text)
import           Utility.TSLogAnalyzer.Log


-- octetParse :: Parser (Int, Int, Int, Int)
-- octetParse = do
--         o1 <- A.decimal
--         _ <-  A.char '.'
--         o2 <- A.decimal
--         _ <-  A.char '.'
--         o3 <- A.decimal
--         _ <-  A.char '.'
--         o4 <- A.decimal
--         return (o1, o2, o3, o4)

ipParse ∷ Parser IP
ipParse = do
        (o1, o2, o3, o4) <- octetParse
        _ <- A.char ':'
        p <- A.decimal
        _ <- A.char ';'
        return (mkIP (o1, o2, o3, o4) p)

octetParse ∷ Parser (Int, Int, Int, Int)
octetParse = (,,,) <$> A.decimal
                   <*  A.char '.'
                   <*> A.decimal
                   <*  A.char '.'
                   <*> A.decimal
                   <*  A.char '.'
                   <*> A.decimal

nameParse ∷ Parser Text
nameParse = do
        _ <- A.char '\''
        uname <- A.takeTill (== '\'')
        _ <- A.char '\''
        return uname

uidParse ∷ Parser Int
uidParse = do
        _ <- A.string "(id:"
        uid <- A.decimal
        _ <- A.string ")"
        return  uid

unidParse ∷ Parser (Text, UserID)
unidParse = do
        nm <- nameParse
        uid <- uidParse
        return (nm, UserID uid)

rsnParse ∷ Parser Text
rsnParse = do
        _ <- A.char '\''
        _ <- A.string "reasonmsg="
        r <- A.takeTill (== '\'')
        _ <- A.char '\''
        _ <- A.char ';'
        return r

dcnParser ∷ Parser Connection
dcnParser = do
        _ <- A.string "client disconnected "
        (nm, uid) <- unidParse
        _ <- A.string " reason "
        rsn <- rsnParse
        return (Connection DCN nm uid Nothing (Just rsn))

conParser ∷ Parser Connection
conParser = do
        _ <- A.string "client connected "
        (nm, uid) <- unidParse
        _ <- A.string " from "
        ip <- ipParse
        return (Connection CON nm uid (Just ip) Nothing)

cParser ∷ Parser Connection
cParser = A.choice [conParser, dcnParser]

connParse ∷ Text → Maybe Connection
connParse = A.maybeResult . A.parse cParser
