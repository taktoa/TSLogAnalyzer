{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | Parse a log file
module Utility.TSLogAnalyzer.Parse where -- (logParse, parseConns) where

import           ClassyPrelude                   hiding (optional)

import           Control.Monad                   (join, void)

import qualified Data.Text.Encoding              as Text

import           Data.Attoparsec.Text            (Parser, parse)
import qualified Data.Attoparsec.Text            as A

import           Utility.TSLogAnalyzer.Log
import           Utility.TSLogAnalyzer.MsgParse
import           Utility.TSLogAnalyzer.TimeParse
import           Utility.TSLogAnalyzer.Util      (optional, whitespace')

-- | Parse the log in the given file
logParse :: FilePath -> IO [LogEntry]
logParse = (pure . parseLogs . Text.decodeUtf8) <=< readFile

-- | Parse the log entries in the given string
parseLogs :: Text -> [LogEntry]
parseLogs = sortBy (comparing entryTime)
          . mapMaybe (A.maybeResult . A.parse entryParser . (<> "\n"))
          . lines

-- | Parse many connections
parseConns :: [LogEntry] -> [(Time, Connection)]
parseConns = mapMaybe parseConn

-- | Parse a connection
parseConn :: LogEntry -> Maybe (Time, Connection)
parseConn (LogEntry t INFO VirtualServerBase m) = (t,) <$> connParse (m <> ";")
parseConn _                                     = Nothing

-- | Parse a 'LogEntry'
entryParser :: Parser LogEntry
entryParser = LogEntry <$> timeParser
                       <*  delimiter
                       <*> logLevelParser
                       <*  delimiter
                       <*> logSourceParser
                       <*  delimiter
                       <*  optional A.digit
                       <*  delimiter
                       <*> msgParser
  where
    delimiter = whitespace' >> A.char '|' >> whitespace'

-- | Parse a reason message
msgParser :: Parser Text
msgParser = A.takeTill A.isEndOfLine

-- | Parse a 'LogLevel'
logLevelParser :: Parser LogLevel
logLevelParser = "DEVELOP"  $> DEVELOP
             <|> "INFO"     $> INFO
             <|> "WARNING"  $> WARNING
             <|> "ERROR"    $> ERROR
             <|> "CRITICAL" $> CRITICAL

-- | Parse a 'LogSource'
logSourceParser :: Parser LogSource
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
