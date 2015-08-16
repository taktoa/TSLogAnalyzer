module Parse (logParse, parseLogs, mergeLogins) where
import           Data.List       (sortBy)
import           Data.Maybe      (maybeToList)
import           Data.Ord        (comparing)
import           Data.Text       (pack)
import           Log
import           MsgParse
import           Prelude.Unicode
import           Text.Read
import           TimeParse

delimiter :: Eq α ⇒ α → [α] → [[α]]
delimiter _ [] = []
delimiter d xs = a : delimiter d (drop 1 b)
    where
    (a, b) = span (≢ d) xs

whiteSpace :: Char → Bool
whiteSpace = (∈ " \t")

stripSpaces :: String → String
stripSpaces = reverse ∘ dropWhile whiteSpace ∘ reverse

mergeLoginsCore :: Int → [(α, ConnectType)] → [α]
mergeLoginsCore _ [] = []
mergeLoginsCore c ((t, CON) : xs)
    | c ≡ 0                       = t : mergeLoginsCore 1 xs
    | otherwise                   =     mergeLoginsCore (c + 1) xs
mergeLoginsCore c ((t, DCN) : xs)
    | c ≡ 0                       =     mergeLoginsCore 0 xs
    | c ≡ 1                       = t : mergeLoginsCore 0 xs
    | otherwise                   =     mergeLoginsCore (c - 1) xs

tuplify :: [α] → [(α, α)]
tuplify [] = []
tuplify (_:[]) = []
tuplify (x:y:xs) = (x, y) : tuplify xs

mergeLogins :: [(α, ConnectType)] → [(α, α)]
mergeLogins = tuplify ∘ mergeLoginsCore 0

tsParseLine :: String → Maybe LogEntry
tsParseLine s = do
        let ds = delimiter '|' s
        es ← if length ds ≡ 5 then Just (map stripSpaces ds) else Nothing
        let [timStr, levStr, srcStr, _, _:msgStr] = es
        tim ← timeParse (pack timStr)
        lev ← readMaybe levStr :: Maybe LogLevel
        src ← readMaybe srcStr :: Maybe LogSource
        msg ← Just (pack (msgStr ++ ";"))
        return (LogEntry tim lev src msg)

tsParseLines :: String → [LogEntry]
tsParseLines = sortBy (comparing time) ∘ concat ∘ map (maybeToList ∘ tsParseLine) ∘ lines

parseLog :: LogEntry → Maybe (Time, Connection)
parseLog (LogEntry t _ _ m) = do
        c ← connParse m
        return (t, c)

parseLogs :: [LogEntry] → [(Time, Connection)]
parseLogs = concat ∘ map (maybeToList ∘ parseLog)

logParse :: FilePath → IO [LogEntry]
logParse fp = do
        fc ← readFile fp
        return (tsParseLines fc)
