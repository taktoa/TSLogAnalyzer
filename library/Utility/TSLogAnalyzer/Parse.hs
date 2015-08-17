{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}

--module Utility.TSLogAnalyzer.Parse ( logParse
--                                   , parseConns
--                                   , mergeLogins
--                                   ) where
module Utility.TSLogAnalyzer.Parse where

import           ClassyPrelude

import           Data.Attoparsec.Text            (Parser, parse)
import qualified Data.Attoparsec.Text            as A
import           Data.Char                       (isSpace)
import           Data.Maybe                      (maybeToList)
import           Data.Ord                        (comparing)
import           Prelude.Unicode
import           Text.Read

import           Utility.TSLogAnalyzer.Log
import           Utility.TSLogAnalyzer.MsgParse
import           Utility.TSLogAnalyzer.TimeParse

-- delimiter ∷ Eq α ⇒ α → [α] → [[α]]
-- delimiter _ [] = []
-- delimiter d xs = a : delimiter d (drop 1 b)
--     where
--     (a, b) = span (≢ d) xs

whiteSpace ∷ Char → Bool
whiteSpace = (∈ " \t")

stripSpaces ∷ String → String
stripSpaces = reverse ∘ dropWhile whiteSpace ∘ reverse

toPairs ∷ [α] → [(α, α)]
toPairs []       = []
toPairs [_]      = []
toPairs (x:y:xs) = (x, y) : toPairs xs

mergeLogins ∷ [(α, ConnectType)] → [(α, α)]
mergeLogins = toPairs ∘ merge 0
  where
    merge _ [] = []
    merge c ((t, CON) : xs)
      | c ≡ 0     = t : merge 1 xs
      | otherwise =     merge (c + 1) xs
    merge c ((t, DCN) : xs)
      | c ≡ 0     =     merge 0 xs
      | c ≡ 1     = t : merge 0 xs
      | otherwise =     merge (c - 1) xs

entryParser ∷ Parser LogEntry
entryParser = LogEntry <$> timeParser
                       <*  delimiter
                       <*> logLevelParser
                       <*  delimiter
                       <*> logSourceParser
                       <*  delimiter
                       <*  A.digit
                       <*  delimiter
                       <*> msgParser
  where
    delimiter = whitespace >> A.char '|' >> whitespace
    whitespace = A.takeWhile isSpace

msgParser ∷ Parser Text
msgParser = A.takeTill A.isEndOfLine <* A.endOfLine

--tsParseLine ∷ String → Maybe LogEntry
--tsParseLine s = do
--       let ds = delimiter '|' s
--       es <- if length ds ≡ 5 then Just (map stripSpaces ds) else Nothing
--       let [timStr, levStr, srcStr, _, _:msgStr] = es
--       tim <- undefined timeParse timStr
--       lev <- readMaybe levStr
--       src <- readMaybe srcStr
--       msg <- Just (pack (msgStr ++ ";"))
--       return (LogEntry tim lev src msg)

tsParseLines ∷ Text → [LogEntry]
tsParseLines = sortBy (comparing entryTime)
             ∘ concat
             ∘ map (maybeToList ∘ A.maybeResult ∘ A.parse entryParser)
             ∘ map (<> "\n")
             ∘ lines


parseConns ∷ [LogEntry] → [(Time, Connection)]
parseConns = concatMap $ maybeToList ∘ parseConn

parseConn (LogEntry t INFO VirtualServerBase m) = (t,) <$> connParse (m <> ";")
parseConn _                                     = Nothing

logParse ∷ FilePath → IO [LogEntry]
logParse fp = tsParseLines <$> readFile fp


logLevelParser ∷ Parser LogLevel
logLevelParser = "DEVELOP"  $> DEVELOP
             <|> "INFO"     $> INFO
             <|> "WARNING"  $> WARNING
             <|> "ERROR"    $> ERROR
             <|> "CRITICAL" $> CRITICAL

logSourceParser ∷ Parser LogSource
logSourceParser = "VirtualServerBase" $> VirtualServerBase
              <|> "Accounting"        $> Accounting
              <|> "BanManager"        $> BanManager
              <|> "CIDRManager"       $> CIDRManager
              <|> "ChanClients"       $> ChanClients
              <|> "DatabaseQuery"     $> DatabaseQuery
              <|> "FileHelp"          $> FileHelp
              <|> "FileManager"       $> FileManager
              <|> "ParamParser"       $> ParamParser
              <|> "PermGroupMgr"      $> PermGroupMgr
              <|> "PktHandler"        $> PktHandler
              <|> "Query"             $> Query
              <|> "SQL"               $> SQL
              <|> "ServerLibPriv"     $> ServerLibPriv
              <|> "ServerMain"        $> ServerMain
              <|> "StringHelp"        $> StringHelp
              <|> "VirtualServer"     $> VirtualServer
              <|> "VirtualSvrMgr"     $> VirtualSvrMgr
