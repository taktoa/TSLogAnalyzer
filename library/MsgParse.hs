module MsgParse (connParse) where
import qualified Data.Attoparsec.Text as A
import Data.Text (Text, pack, unpack)
import Data.Attoparsec.Text (Parser)
import Log
import Prelude.Unicode

octetParse ∷ Parser (Int, Int, Int, Int)
octetParse = do
        o1 ← A.decimal
        _ ←  A.char '.'
        o2 ← A.decimal
        _ ←  A.char '.'
        o3 ← A.decimal
        _ ←  A.char '.'
        o4 ← A.decimal
        return (o1, o2, o3, o4)

ipParse ∷ Parser IP
ipParse = do
        (o1, o2, o3, o4) ← octetParse
        _ ← A.char ':'
        p ← A.decimal
        _ ← A.char ';'
        return (genIP o1 o2 o3 o4 p)

nameParse ∷ Parser Text
nameParse = do
        _ ← A.char '\''
        uname ← A.takeTill (≡ '\'')
        _ ← A.char '\''
        return uname

uidParse ∷ Parser Int
uidParse = do
        _ ← A.string (pack "(id:")
        uid ← A.decimal
        _ ← A.string (pack ")")
        return  uid

unidParse ∷ Parser (Text, Int)
unidParse = do
        nm ← nameParse
        id ← uidParse
        return (nm, id)

rsnParse ∷ Parser Text
rsnParse = do
        _ ← A.char '\''
        _ ← A.string (pack "reasonmsg=")
        r ← A.takeTill (≡ '\'')
        _ ← A.char '\''
        _ ← A.char ';'
        return r

dcnParser ∷ Parser Connection
dcnParser = do
        _ ← A.string (pack "client disconnected ")
        (nm, id) ← unidParse
        _ ← A.string (pack " reason ")
        rsn ← rsnParse
        return (Connection DCN nm id Nothing (Just rsn))

conParser ∷ Parser Connection
conParser = do
        _ ← A.string (pack "client connected ")
        (nm, id) ← unidParse
        _ ← A.string (pack " from ")
        ip ← ipParse
        return (Connection CON nm id (Just ip) Nothing)
        
cParser ∷ Parser Connection
cParser = A.choice [conParser, dcnParser]

connParse ∷ Text -> Maybe Connection
connParse = A.maybeResult . A.parse cParser
