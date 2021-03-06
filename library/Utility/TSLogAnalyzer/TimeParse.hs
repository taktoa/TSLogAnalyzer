{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}

-- | Parse a time from the log
module  Utility.TSLogAnalyzer.TimeParse where

import           ClassyPrelude
import           Prelude                      (Read (..), read)

import           Data.Attoparsec.Text         (Parser, decimal)
import           Data.Char                    (isDigit)
import           Data.Maybe                   (fromJust)
import           Data.Time.Clock.POSIX        (utcTimeToPOSIXSeconds)

import           Text.ParserCombinators.ReadP

import           Utility.TSLogAnalyzer.Util

newtype Time = Time { getUnixNanoTime :: Int }
             deriving (Eq, Ord, Show, Read, Generic)

mkTime :: Int -> Time
mkTime = Time

newtype DiffTime = DiffTime { getNanoDiff :: Int }
             deriving (Eq, Ord, Show, Read, Generic)

mkDiffTime :: Int -> DiffTime
mkDiffTime = DiffTime

-- Shamelessly stolen from Conal Elliot's vector-space package
class AffineSpace ρ where
  type Diff ρ
  (.-.) :: ρ -> ρ -> Diff ρ
  (.+^) :: ρ -> Diff ρ -> ρ
  (.-^) :: ρ -> Diff ρ -> ρ

instance AffineSpace Time where
  type Diff Time = DiffTime
  (Time a) .-. (Time b)     = DiffTime (a - b)
  (Time a) .+^ (DiffTime b) = Time (a + b)
  (Time a) .-^ (DiffTime b) = Time (a - b)

data TSDate = TSDate { tsYear       :: Int
                     , tsMonth      :: Int
                     , tsDay        :: Int
                     , tsHour       :: Int
                     , tsMinute     :: Int
                     , tsSecond     :: Int
                     , tsFractional :: Int
                     } deriving (Eq)

instance Show TSDate where
  show (TSDate yr mo dy hr mn sc fr) = unpack $ date <> " " <> time
    where
      date = sh 4 yr <> "-" <> sh 2 mo <> "-" <> sh 2 dy
      time = sh 2 hr <> ":" <> sh 2 mn <> ":" <> sh 2 sc <> "." <> tshow fr
      sh i k = let t = tshow k in replicate (i - length t) '0' <> t

instance Read TSDate where
  readsPrec _ = rd $ TSDate <$> numP <* hyphenP <*> numP <* hyphenP <*> numP
                            <*  spaceP
                            <*> numP <* colonP  <*> numP <* colonP  <*> numP
                            <*  periodP
                            <*> numP
    where
      rd = readP_to_S
      hyphenP = char '-'
      spaceP  = char ' '
      colonP  = char ':'
      periodP = char '.'
      numP = read <$> munch1 isDigit

timeParser :: Parser Time
timeParser = toUnix . readUTC . tshow <$> tsDateParser

tsDateParser :: Parser TSDate
tsDateParser = mkTSDate <$> ((,,) <$> num <* hyphen <*> num <* hyphen <*> num)
                        <*  space
                        <*> ((,,) <$> num <* colon  <*> num <* colon  <*> num)
                        <*  period
                        <*> num
  where
    num  = decimal :: Parser Int
    mkTSDate (yr, mo, dy) (hr, mn, sc) = TSDate yr mo dy hr mn sc

readUTC :: Text -> UTCTime
readUTC = fromJust . readMay . unpack

toUnix :: UTCTime -> Time
toUnix = mkTime . truncate . toNanos . utcTimeToPOSIXSeconds
  where
    toNanos :: Num n => n -> n
    toNanos s = s * 1000000000

instance Hashable Time
instance Hashable DiffTime
