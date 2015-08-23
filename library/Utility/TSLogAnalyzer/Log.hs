{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}

-- | Types for log parsing
module Utility.TSLogAnalyzer.Log ( LogEntry    (..)
                                 , Connection  (..)
                                 , Session     (..)
                                 , LogLevel    (..)
                                 , LogSource   (..)
                                 , ConnectType (..)
                                 , UserName    (..)
                                 , UserID      (..)
                                 , IP          (..)
                                 , mkIP, getOctets
                                 ) where

import           ClassyPrelude
import           Prelude.Unicode

import           Utility.TSLogAnalyzer.TimeParse (Time, mkTime)

data LogEntry = LogEntry { entryTime    ∷ !Time
                         , entryLevel   ∷ !LogLevel
                         , entrySource  ∷ !LogSource
                         , entryMessage ∷ !Text
                         } deriving (Eq, Show, Read, Generic)

data Connection = Connection { connType   ∷ !ConnectType
                             , connName   ∷ !UserName
                             , connUID    ∷ !UserID
                             , connIP     ∷ !(Maybe IP)
                             , connReason ∷ !(Maybe Text)
                             } deriving (Eq, Show, Read, Generic)

data Session = Session { sessStart  ∷ !Time
                       , sessEnd    ∷ !Time
                       , sessName   ∷ !UserName
                       , sessUID    ∷ !UserID
                       , sessIP     ∷ !IP
                       , sessReason ∷ !Text
                       } deriving (Eq, Show, Read, Generic)

data LogLevel   = DEVELOP
                | INFO
                | WARNING
                | ERROR
                | CRITICAL
                deriving (Eq, Ord, Enum, Show, Read, Generic)

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
                deriving (Eq, Ord, Enum, Show, Read, Generic)

data ConnectType = DCN
                 | CON
                 deriving (Eq, Ord, Enum, Show, Read, Generic)

newtype UserName = UserName { getName ∷ Text }
                 deriving (Eq, Ord, Show, Read, Generic)

newtype UserID = UserID { getUID ∷ Int }
               deriving (Eq, Enum, Ord, Show, Read, Generic)

data IP = IP { getAddr ∷ !Int
             , getPort ∷ !Int
             } deriving (Eq, Ord, Show, Read, Generic)

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

instance Hashable LogEntry
instance Hashable Connection
instance Hashable Session
instance Hashable ConnectType
instance Hashable UserName
instance Hashable UserID
instance Hashable LogLevel
instance Hashable LogSource
instance Hashable IP
