{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Utility.TSLogAnalyzer.Log ( LogEntry    (..)
                                 , Connection  (..)
                                 , Session     (..)
                                 , LogLevel    (..)
                                 , LogSource   (..)
                                 , ConnectType (..)
                                 , Time        (..)
                                 , UserID      (..)
                                 , IP          (..)
                                 , mkIP, getOctets
                                 ) where

import           Data.Text (Text)

data LogEntry = LogEntry { entryTime    :: Time
                         , entryLevel   :: LogLevel
                         , entrySource  :: LogSource
                         , entryMessage :: Text
                         } deriving (Eq, Show, Read)

data Connection = Connection { connType   :: ConnectType
                             , connName   :: Text
                             , connUID    :: UserID
                             , connIP     :: Maybe IP
                             , connReason :: Maybe Text
                             } deriving (Eq, Show, Read)

data Session = Session { sessStart  :: Time
                       , sessEnd    :: Time
                       , sessName   :: Text
                       , sessUID    :: UserID
                       , sessIP     :: IP
                       , sessReason :: Text
                       } deriving (Eq, Show, Read)

data LogLevel   = DEVELOP
                | INFO
                | WARNING
                | ERROR
                | CRITICAL
                deriving (Eq, Ord, Enum, Show, Read)

data LogSource  = Accounting
                | BanManager
                | CIDRManager
                | ChanClients
                | DatabaseQuery
                | FileHelp
                | FileManager
                | ParamParser
                | PermGroupMgr
                | PktHandler
                | Query
                | SQL
                | ServerLibPriv
                | ServerMain
                | StringHelp
                | VirtualServer
                | VirtualServerBase
                | VirtualSvrMgr
                deriving (Eq, Ord, Enum, Show, Read)


data ConnectType = DCN
                 | CON
                 deriving (Eq, Ord, Enum, Show, Read)

newtype Time = Time { getUnixTime :: Int }
             deriving (Eq, Ord, Show, Read)

newtype UserID = UserID { getUID :: Int }
               deriving (Eq, Enum, Ord, Show, Read)

data IP = IP { getAddr :: Int
             , getPort :: Int
             } deriving (Read, Show, Eq)

-- | Make an IP
mkIP ∷ (Int, Int, Int, Int) → Int → IP
mkIP (o1, o2, o3, o4) = IP $ o1 * (256^3) + o2 * (256^2) + o3 * 256 + o4

-- | Get the octets for a given IP
getOctets ∷ IP → (Int, Int, Int, Int)
getOctets (IP a _) = (o1, o2, o3, o4)
    where
    o4 = a `mod` 256
    o3 = ((a - o4) `div` 256) `mod` 256
    o2 = ((((a - o4) `div` 256) - o3) `div` 256) `mod` 256
    o1 = ((((((a - o4) `div` 256) - o3) `div` 256) - o2) `div` 256) `mod` 256
