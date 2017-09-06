{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}

-- | Types for log parsing
module Utility.TSLogAnalyzer.Log
  ( LogEntry    (..)
  , Connection  (..)
  , Session     (..)
  , LogLevel    (..)
  , LogSource   (..)
  , ConnectType (..)
  , UserName    (..)
  , UserID      (..)
  , IP          (..)
  , fromOctets, getOctets
  ) where

import           ClassyPrelude

import           Utility.TSLogAnalyzer.TimeParse (Time, mkTime)

import           Flow

--------------------------------------------------------------------------------

data LogEntry
  = LogEntry
    { entryTime    :: !Time
    , entryLevel   :: !LogLevel
    , entrySource  :: !LogSource
    , entryMessage :: !Text
    }
  deriving (Eq, Show, Read, Generic)

instance Hashable LogEntry

--------------------------------------------------------------------------------

data Connection
  = Connection
    { connType   :: !ConnectType
    , connName   :: !UserName
    , connUID    :: !UserID
    , connIP     :: !(Maybe IP)
    , connReason :: !(Maybe Text)
    }
  deriving (Eq, Show, Read, Generic)

instance Hashable Connection

--------------------------------------------------------------------------------

data Session
  = Session
    { sessStart  :: !Time
    , sessEnd    :: !Time
    , sessName   :: !UserName
    , sessUID    :: !UserID
    , sessIP     :: !IP
    , sessReason :: !Text
    }
  deriving (Eq, Show, Read, Generic)

instance Hashable Session

--------------------------------------------------------------------------------

data LogLevel
  = DEVELOP
  | INFO
  | WARNING
  | ERROR
  | CRITICAL
  deriving (Eq, Ord, Enum, Show, Read, Generic)

instance Hashable LogLevel

--------------------------------------------------------------------------------

data LogSource
  = Accounting
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

instance Hashable LogSource

--------------------------------------------------------------------------------

data ConnectType
  = DCN
  | CON
  deriving (Eq, Ord, Enum, Show, Read, Generic)

instance Hashable ConnectType

--------------------------------------------------------------------------------

newtype UserName
  = UserName { getName :: Text }
  deriving (Eq, Ord, Show, Read, Generic)

instance Hashable UserName

--------------------------------------------------------------------------------

newtype UserID
  = UserID { getUID :: Int }
  deriving (Eq, Enum, Ord, Show, Read, Generic)

instance Hashable UserID

--------------------------------------------------------------------------------

type Octets = (Word8, Word8, Word8, Word8)

--------------------------------------------------------------------------------

newtype IP
  = IP { fromIP :: Int }
  deriving (Eq, Enum, Show, Read, Generic)

instance Hashable IP

-- | Make an IP from a tuple of octets
fromOctets :: Octets -> IP
fromOctets (o1, o2, o3, o4) = [ fi o1 * 16777216
                              , fi o2 * 65536
                              , fi o3 * 256
                              , fi o4
                              ] |> sum |> IP
  where
    fi = fromIntegral :: Word8 -> Int

-- | Get the octets for a given IP
toOctets :: IP -> Octets
toOctets (IP a) = (fi o1, fi o2, fi o3, fi o4)
  where
    o1 = ((((((a - o4) `div` 256) - o3) `div` 256) - o2) `div` 256) `mod` 256
    o2 = ((((a - o4) `div` 256) - o3) `div` 256) `mod` 256
    o3 = ((a - o4) `div` 256) `mod` 256
    o4 = a `mod` 256
    -- this is safe because we always mod out by 256 anyway
    fi = fromIntegral :: Int -> Word8

--------------------------------------------------------------------------------

-- | Transport-layer protocol (TCP/UDP) address.
data TLAddr
  = TLAddr
    { tlAddr :: !IP
    , tlPort :: !Int
    }
  deriving (Eq, Show, Read, Generic)

instance Hashable UDPAddr

--------------------------------------------------------------------------------
