module Utility.TSLogAnalyzer.Parse ( logParse
                                   , parseLogs
                                   , mergeLogins
                                   ) where

import           Data.List                       (sortBy)
import           Data.Maybe                      (maybeToList)
import           Data.Ord                        (comparing)
import           Data.Text                       (pack)
import           Text.Read

import           Prelude.Unicode

import           Utility.TSLogAnalyzer.Log
import           Utility.TSLogAnalyzer.MsgParse
import           Utility.TSLogAnalyzer.TimeParse

delimiter ∷ Eq α ⇒ α → [α] → [[α]]
delimiter _ [] = []
delimiter d xs = a : delimiter d (drop 1 b)
    where
    (a, b) = span (≢ d) xs

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

tsParseLine ∷ String → Maybe LogEntry
tsParseLine s = do
        let ds = delimiter '|' s
        es <- if length ds ≡ 5 then Just (map stripSpaces ds) else Nothing
        let [timStr, levStr, srcStr, _, _:msgStr] = es
        tim <- timeParse (pack timStr)
        lev <- readMaybe levStr
        src <- readMaybe srcStr
        msg <- Just (pack (msgStr ++ ";"))
        return (LogEntry tim lev src msg)

tsParseLines ∷ String → [LogEntry]
tsParseLines = sortBy (comparing entryTime)
             ∘ concat
             ∘ map (maybeToList ∘ tsParseLine)
             ∘ lines

parseLog ∷ LogEntry → Maybe (Time, Connection)
parseLog (LogEntry t _ _ m) = do
        c <- connParse m
        return (t, c)

parseLogs ∷ [LogEntry] → [(Time, Connection)]
parseLogs = concat ∘ map (maybeToList ∘ parseLog)

logParse ∷ FilePath → IO [LogEntry]
logParse fp = tsParseLines <$> readFile fp
