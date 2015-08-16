{-# NoMonomorphismRestriction #-}

module  Utility.TSLogAnalyzer.TimeParse ( timeParse
                                        , unixTSDate
                                        , timeParseCheck
                                        ) where

import           Control.Applicative       ((<$>))
import           Data.Maybe                (mapMaybe, maybe, maybeToList)
import           Data.Text                 (Text, pack, unpack)
import           Data.Time.Format          (FormatTime, ParseTime, formatTime,
                                            parseTime)
import           System.Locale             (TimeLocale, defaultTimeLocale)
import           System.Process            (readProcess)
import           Text.Read                 (readMaybe)

import           Utility.TSLogAnalyzer.Log

data TSDate = TSDate { year  :: Int
                     , month :: Int
                     , day   :: Int
                     } deriving (Eq, Show, Read)

data TSTime = TSTime { hour   :: Int
                     , minute :: Int
                     , second :: Int
                     } deriving (Eq, Show, Read)

timeFormat :: String
timeFormat = "%Y-%m-%d %k:%M:%S%Q"

timeParse :: Text -> Maybe Time
timeParse t = (rawTimeParse $ unpack t) >>= readMaybe

rawTimeParse :: String -> Maybe String
rawTimeParse = castTime defaultTimeLocale timeFormat "%s"

castTime :: TimeLocale -> String -> String -> String -> Maybe String
castTime l i o t = format <$> parse
        where
        format = formatTime l o :: (ParseTime t, FormatTime t) => t -> String
        parse = parseTime l i t :: ParseTime t => Maybe t

unixTSDate :: IO (Text, Int)
unixTSDate = do
        r <- readProcess "date" ["+%Y-%m-%d %k:%M:%S|%s"] []
        let (t, u) = span (/= '|') r
        return (pack (t ++ ".00000"), read $ tail u)

renderTSDate :: Int -> IO Text
renderTSDate u = pack <$> readProcess "date" [p, ps] []
        where
        ps = "+%Y-%m-%d %k:%M:%S.00000"
        p = "--date='" ++ (show u) ++ "'"

timeParseCheck :: Int -> IO ()
timeParseCheck i = do
        u <- renderTSDate i
        let f = timeParse u
        print i
        print u
        print f
        print ((\a -> a - i) <$> f)

{-
tsDateParser :: Parser TSDate
tsDateParser = do
        year <- A.decimal
        _    <- A.char '-'
        mon  <- A.decimal
        _    <- A.char '-'
        day  <- A.decimal
        return (TSDate year mon day)

tsTimeParser :: Parser TSTime
tsTimeParser = do
        hour <- A.decimal
        _    <- A.char ':'
        min  <- A.decimal
        _    <- A.char ':'
        sec  <- A.decimal
        return (TSTime hour min sec)

tParser :: Parser Time
tParser = do
        dt <- tsDateParser
        _  <- A.char ' '
        tm <- tsTimeParser
        return (convertTSTD dt tm)

timeToSeconds :: TSTime -> Int
timeToSeconds (TSTime hr mn sc) = (hr*3600) + (mn*60) + sc

dateToSeconds :: TSDate -> Int
dateToSeconds (TSDate yr mo dy) = yrsc + ((mody + dy) * 86400)
        where
        yrsc = (yr - 1970)*31557600
        mody = sum $ mapMaybe monthDays [1..mo]

convertTSTD :: TSDate -> TSTime -> Int
convertTSTD dt tm = (timeToSeconds tm) + (dateToSeconds dt)

tsTimeChecker :: TSTime -> Bool
tsTimeChecker tm = and [hrT, mnT, scT]
        where
        TSTime hr mn sc = tm
        hrT = hr ∈ [0..24]
        mnT = mn ∈ [0..60]
        scT = sc ∈ [0..60]

monthDays :: Int -> Maybe Int
monthDays m
        | m == 2                    = Just 28
        | m ∈ [4,6,9,11]            = Just 30
        | m ∈ [1,3,5,7,8,10,12]     = Just 31
        | otherwise                 = Nothing

tsDateChecker :: TSDate -> Bool
tsDateChecker td = and [yrT, moT, dyT]
        where
        TSDate yr mo dy = td
        yrT = yr ∈ [0..9999]
        moT = mo ∈ [1..12]
        dyT
            | dy ∈ [1..28]      = True
            | mo == 2           = dy ∈ [29]
            | mo ∈ [4,6,9,11]   = dy ∈ [29, 30]
            | otherwise         = dy ∈ [29, 30, 31]
-}
