{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- module  Utility.TSLogAnalyzer.TimeParse ( timeParse
--                                         , unixTSDate
--                                         , timeParseCheck
--                                         , TSDate (..)
--                                         , TSTime (..)
--                                         ) where

module  Utility.TSLogAnalyzer.TimeParse where


import           ClassyPrelude
import           Prelude                      (Read (..), read)

import           Data.Attoparsec.Text         (Parser, decimal, parse)
import           Data.Char                    (isDigit)
import           Data.Maybe                   (fromJust)
--import           Data.Text                  (Text, pack, unpack)
import           Data.Time.Clock.POSIX        (utcTimeToPOSIXSeconds)
import           Prelude.Unicode

import           Text.ParserCombinators.ReadP

import           Utility.TSLogAnalyzer.Log
import           Utility.TSLogAnalyzer.Util

data TSDate = TSDate { tsYear       ∷ Int
                     , tsMonth      ∷ Int
                     , tsDay        ∷ Int
                     , tsHour       ∷ Int
                     , tsMinute     ∷ Int
                     , tsSecond     ∷ Int
                     , tsFractional ∷ Int
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

timeParser ∷ Parser Time
timeParser = toUnix ∘ readUTC ∘ tshow <$> tsDateParser

tsDateParser ∷ Parser TSDate
tsDateParser = mkTSDate <$> ((,,) <$> num <* hyphen <*> num <* hyphen <*> num)
                        <*  space
                        <*> ((,,) <$> num <* colon  <*> num <* colon  <*> num)
                        <*  period
                        <*> num
  where
    num  = decimal :: Parser Int
    mkTSDate (yr, mo, dy) (hr, mn, sc) = TSDate yr mo dy hr mn sc

readUTC ∷ Text → UTCTime
readUTC = fromJust ∘ readMay ∘ unpack

toUnix ∷ UTCTime → Time
toUnix = mkTime ∘ truncate ∘ toNanos ∘ utcTimeToPOSIXSeconds
  where
    toNanos ∷ Num n ⇒ n → n
    toNanos s = s * 1000000000
