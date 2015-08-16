module Utility.TSLogAnalyzer.Log where
import           Data.Text       (Text)
import           Prelude.Unicode

data LogEntry = LogEntry {
                    time    :: Time,
                    level   :: LogLevel,
                    source  :: LogSource,
                    message :: Text
                } deriving (Eq, Show, Read)

data Connection = Connection {
                    connType :: ConnectType,
                    userName :: Text,
                    userID   :: Int,
                    userIP   :: Maybe IP,
                    reason   :: Maybe Text
                } deriving (Eq, Show, Read)

data Session = Session {
                    times :: (Time, Time),
                    name  :: Text,
                    id    :: Int,
                    ip    :: IP,
                    rsn   :: Text
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

data IP = IP { addr :: Int
             , port :: Int
             } deriving (Read, Show, Eq)

type Time = Int

type UserID = Int

genIP :: Int -> Int -> Int -> Int -> Int -> IP
genIP o1 o2 o3 o4 = IP address
    where
    address = (o1 * (256^3)) + (o2 * (256^2)) + (o3 * 256) + o4

genOctets :: IP -> (Int, Int, Int, Int)
genOctets (IP a _) = (o1, o2, o3, o4)
    where
    o4 = a `mod` 256
    o3 = ((a - o4) `div` 256) `mod` 256
    o2 = ((((a - o4) `div` 256) - o3) `div` 256) `mod` 256
    o1 = ((((((a - o4) `div` 256) - o3) `div` 256) - o2) `div` 256) `mod` 256
