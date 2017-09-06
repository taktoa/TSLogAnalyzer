{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}

-- | Main functions for log analysis
module Utility.TSLogAnalyzer (module Utility.TSLogAnalyzer) where

import           ClassyPrelude

import qualified Data.Text.Encoding               as Text

import qualified Data.Foldable                    as Fold
import qualified Data.Traversable                 as Trav

import qualified Data.Graph                       as G
import qualified Data.IntervalMap.FingerTree      as IMap
import qualified Data.List                        as L
import qualified Data.ListLike                    as LL
import qualified Data.Map.Strict                  as Map
import qualified Data.Set                         as Set
import qualified Data.Tree                        as Tree
import qualified Utility.TSLogAnalyzer.BiMultiMap as BMM

import           Control.Monad.State.Lazy         (State)
import           Data.IntMap.Strict               (IntMap)
import           Data.ListLike                    (ListLike)
import           Data.Map.Strict                  (Map)
import           Data.Set                         (Set)
import           Utility.TSLogAnalyzer.BiMultiMap (BiMultiMap)

import           Control.Arrow                    ((&&&))
import           Control.Monad.State.Lazy         (get, modify, put, state)
import           Control.Monad.State.Lazy
                 (evalState, execState, runState)
import           Data.Maybe
                 (fromMaybe, mapMaybe, maybeToList)
import           Data.Ord                         (comparing)
import           Data.Tuple.Extra                 (dupe)
import           Extra                            (snd3)

import           Utility.TSLogAnalyzer.Log
import           Utility.TSLogAnalyzer.MsgParse
import           Utility.TSLogAnalyzer.Parse
import           Utility.TSLogAnalyzer.TimeParse

import           Utility.TSLogAnalyzer.Util

(!@) :: (Ord κ) => Map κ α -> κ -> Maybe α
a !@ b = Map.lookup b a

ø :: (Monoid μ) => μ
ø = mempty

sing :: (MonoPointed μ) => Element μ -> μ
sing = opoint

range :: (Ord α, Foldable τ, Traversable τ) => τ α -> (α, α)
range xs = Fold.foldr1 cmp $ dupe <$> xs
  where
    cmp (a, b) (c, d) = (min a c, max b d)

data ServerState
  = SState
    { usersOnline :: Set UserID
    , userTime    :: Map UserID (Time, DiffTime)
    }
  deriving (Eq, Show, Read)

data ServerEvent
  = SCon UserID UserName
  | SDcn UserID UserName
  deriving ()

genConnEvents :: [(Time, Connection)] -> [(Time, ServerEvent)]
genConnEvents = map (second conToSE)
  where
    conToSE (Connection CON name uid _ _) = SCon uid name
    conToSE (Connection DCN name uid _ _) = SDcn uid name

stateEvents :: [(Time, ServerEvent)] -> Time -> ServerState
stateEvents evts time = flip execState (SState ø ø)
                      $ mapM_ stateEvent
                      $ filter ((> time) . fst)
                      $ sortBy (comparing fst) evts

stateEvent :: (Time, ServerEvent) -> State ServerState ()
stateEvent (time, (SCon uid name)) = conSC time uid
stateEvent (time, (SDcn uid name)) = dcnSC time uid

conSC :: Time -> UserID -> State ServerState ()
conSC time uid = modify modder
  where
    modder ss = ss { usersOnline = Set.insert uid $ usersOnline ss
                   , userTime    = Map.adjust addLogin uid $ userTime ss
                   }
    addLogin (last, acc) = (time, acc)

dcnSC :: Time -> UserID -> State ServerState ()
dcnSC time uid = modify modder
  where
    modder ss = ss { usersOnline = Set.delete uid (usersOnline ss)
                   , userTime    = Map.adjust addDelta uid (userTime ss)
                   }
    addDelta (last, acc) = (last, (time .+^ acc) .-. last)

type AliasMap = Map UserID (Map UserName (Set Octets))
type Identifier = (UserName, UserID, Octets)

getConns :: FilePath -> IO [(Time, Connection)]
getConns fp = parseConns <$> logParse fp

generateAliases :: [(Time, Connection)] -> [Identifier]
generateAliases = hashNub . mapMaybe (process . snd)
  where
    process (Connection _ _ _   Nothing  _) = Nothing
    process (Connection _ n uid (Just a) _) = Just (n, uid, toOctets (tlAddr a))

printAliases :: [Identifier] -> IO ()
printAliases = mapM_ printAlias
  where
    printAlias :: Identifier -> IO ()
    printAlias (UserName name, UserID uid, ip) = print (name, uid, showIP ip)

    showIP (o1, o2, o3, o4)
      = mconcat [tshow o1, ".", tshow o2, ".", tshow o3, ".", tshow o4]

printAliasMap :: AliasMap -> IO ()
printAliasMap = mapM_ (uncurry helper1) . Map.toList
  where
    helper1 :: UserID -> Map UserName (Set Octets) -> IO ()
    helper1 (UserID uid) m = do
      putStrLn $ "UserID: " <> tshow uid
      mapM_ (uncurry helper2) (Map.toList m)

    helper2 :: UserName -> Set Octets -> IO ()
    helper2 (UserName name) s = do
      putStrLn ("  " <> "Name: " <> name)
      mapM_ (putStrLn . ("    " <>) . showIP) s

    showIP (o1, o2, o3, o4)
      = mconcat [tshow o1, ".", tshow o2, ".", tshow o3, ".", tshow o4]

processAliases :: [Identifier] -> AliasMap
processAliases xs = execState (mapM_ process xs) ø
  where
    process (n, u, i) = modify ((Just . process' n i) `Map.alter` u)
    process' name ip = Map.insertWith Set.union name (sing ip) . fromMaybe ø

mergeUsers :: [Identifier] -> [Identifier]
mergeUsers = map snd . Map.toList . BMM.getDist . identGroups

identGroups :: [Identifier] -> BiMultiMap Int Identifier
identGroups xs = BMM.fromList
               $ zip [0..]
               $ map (mapMaybe (`Map.lookup` idxMap) . Tree.flatten)
               $ G.components
               $ identGraph xs
  where
    (_, _, idxMap) = genIndex xs

identGraph :: [Identifier] -> G.Graph
identGraph xs = G.buildG (iMin, iMax)
              $ flip evalState idxMap
              $ concatMapM process [iMin .. iMax]
  where
    process i = do
      imap <- get
      put (Map.delete i imap)
      return $ concat $ procMap imap i <$> Map.lookup i imap
    procMap imap i = zip (repeat i)
                   . Map.keys
                   . flip Map.filter imap
                   . equalElem
    equalElem (a, p, x) (b, q, y) = (a == b && p == q) ||
                                    (a == b && x == y) ||
                                    (p == q && x == y)
    (iMin, iMax, idxMap) = genIndex xs

genIndex :: [Identifier] -> (Int, Int, Map Int Identifier)
genIndex xs = (0, Map.size idxMap, idxMap)
  where
    idxMap = Map.fromList $ zip [0..] xs

bigLog :: IO Text
bigLog = Text.decodeUtf8 <$> readFile "./data/BIGLOG_2015.log"

testTime :: Time
testTime = mkTime 1440200402000000000

main :: IO ()
main = do
  fp <- fromMaybe (error "No file specified") . headMay <$> getArgs
  aliases <- generateAliases <$> getConns (unpack fp)
  printAliases aliases
  -- printAliases $ processAliases $ mergeUsers aliases
