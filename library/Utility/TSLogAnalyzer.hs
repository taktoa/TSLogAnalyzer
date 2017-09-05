{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}

-- | Main functions for log analysis
module Utility.TSLogAnalyzer (module Utility.TSLogAnalyzer) where

import           ClassyPrelude
import           Prelude.Unicode

import qualified Data.Foldable                    as Fold
import qualified Data.Traversable                 as Trav

import qualified Data.Graph                       as G
import qualified Data.IntMap.Strict               as IM
import qualified Data.List                        as L
import qualified Data.ListLike                    as LL
import qualified Data.Map.Strict                  as M
import qualified Data.Set                         as S
import qualified Data.Tree                        as T
import qualified Utility.TSLogAnalyzer.BiMultiMap as BMM

import           Control.Monad.State.Lazy         (State)
import           Data.IntMap.Strict               (IntMap)
import           Data.ListLike                    (ListLike)
import           Data.Map.Strict                  (Map)
import           Data.Set                         (Set)
import           Utility.TSLogAnalyzer.BiMultiMap (BiMultiMap)

import           Control.Arrow                    ((&&&))
import           Control.Monad.State.Lazy         (get, modify, put, state)
import           Control.Monad.State.Lazy         (evalState, execState,
                                                   runState)
import           Data.Maybe                       (fromMaybe, mapMaybe,
                                                   maybeToList)
import           Data.Ord                         (comparing)
import           Data.Set.Unicode                 ((∩), (∪))
import           Data.Tuple.Extra                 (dupe)
import           Extra                            (snd3)

import           Utility.TSLogAnalyzer.Log
import           Utility.TSLogAnalyzer.MsgParse
import           Utility.TSLogAnalyzer.Parse
import           Utility.TSLogAnalyzer.TimeParse

import           Utility.TSLogAnalyzer.Util

(!@) :: Ord κ => Map κ α → κ → Maybe α
a !@ b = M.lookup b a

ø :: Monoid μ => μ
ø = mempty

sing :: MonoPointed μ => Element μ → μ
sing = opoint

range :: (Ord α, Foldable τ, Traversable τ) => τ α → (α, α)
range xs = Fold.foldr1 cmp $ dupe <$> xs
  where cmp (a, b) (c, d) = (min a c, max b d)

data ServerState = SState { usersOnline :: Set UserID
                          , userTime    :: Map UserID (Time, DiffTime)
                          } deriving (Eq, Show, Read)

data ServerEvent = SCon UserID UserName
                 | SDcn UserID UserName

genConnEvents :: [(Time, Connection)] → [(Time, ServerEvent)]
genConnEvents = map (second $ conToSE)
  where
    conToSE (Connection CON name uid _ _) = SCon uid name
    conToSE (Connection DCN name uid _ _) = SDcn uid name

stateEvents :: [(Time, ServerEvent)] → Time → ServerState
stateEvents evts time = flip execState (SState ø ø)
                      $ mapM_ stateEvent
                      $ filter ((> time) . fst)
                      $ sortBy (comparing fst) evts

stateEvent :: (Time, ServerEvent) → State ServerState ()
stateEvent (time, (SCon uid name)) = conSC time uid
stateEvent (time, (SDcn uid name)) = dcnSC time uid

conSC :: Time → UserID → State ServerState ()
conSC time uid = modify modder
  where
    modder ss = ss { usersOnline = S.insert uid $ usersOnline ss
                   , userTime    = M.adjust addLogin uid $ userTime ss
                   }
    addLogin (last, acc) = (time, acc)

dcnSC :: Time → UserID → State ServerState ()
dcnSC time uid = modify modder
  where
    modder ss = ss { usersOnline = S.delete uid $ usersOnline ss
                   , userTime    = M.adjust addDelta uid $ userTime ss
                   }
    addDelta (last, acc) = (last, (time .+^ acc) .-. last)

type IPAddr = (Int, Int, Int, Int)
type AliasMap = Map UserID (Map UserName (Set IPAddr))
type Identifier = (UserName, UserID, IPAddr)

getConns :: FilePath → IO [(Time, Connection)]
getConns = parseConns <∘> logParse

generateAliases :: [(Time, Connection)] → [Identifier]
generateAliases = hashNub ∘ mapMaybe (process ∘ snd)
  where
    process (Connection _ _    _   Nothing   _) = Nothing
    process (Connection _ name uid (Just ip) _) = Just (name, uid, getOctets ip)

printAliases :: AliasMap → IO ()
printAliases = mapM_ (uncurry helper1) ∘ M.toList
  where
    helper1 :: UserID → Map UserName (Set IPAddr) → IO ()
    helper1 (UserID uid) m = do
      putStrLn $ "UserID: " <> tshow uid
      mapM_ (uncurry helper2) $ M.toList m
    helper2 :: UserName → Set IPAddr → IO ()
    helper2 (UserName name) s = do
      putStrLn $ "  " <> "Name: " <> name
      mapM_ (putStrLn ∘ ("    " <>) ∘ showIP) s
    showIP (o1, o2, o3, o4) = tshow o1 <> "."
                           <> tshow o2 <> "."
                           <> tshow o3 <> "."
                           <> tshow o4

processAliases :: [Identifier] → AliasMap
processAliases xs = execState (mapM_ process xs) ø
  where
    process (n, u, i) = modify ((Just ∘ process' n i) `M.alter` u)
    process' name ip = M.insertWith (∪) name (sing ip) ∘ fromMaybe ø

mergeUsers :: [Identifier] → [Identifier]
mergeUsers = map snd ∘ M.toList ∘ BMM.getDist ∘ identGroups

identGroups :: [Identifier] → BiMultiMap Int Identifier
identGroups xs = BMM.fromList
               $ zip [0..]
               $ map (mapMaybe (`IM.lookup` idxMap) ∘ T.flatten)
               $ G.components
               $ identGraph xs
  where
    (_, _, idxMap) = genIndex xs

identGraph :: [Identifier] → G.Graph
identGraph xs = G.buildG (iMin, iMax)
              $ flip evalState idxMap
              $ concatMapM process [iMin .. iMax]
  where
    process i = do
      imap <- get
      put (IM.delete i imap)
      return $ concat $ procMap imap i <$> IM.lookup i imap
    procMap imap i = zip (repeat i)
                   ∘ IM.keys
                   ∘ flip IM.filter imap
                   ∘ equalElem
    equalElem (a, p, x) (b, q, y) = (a == b && p == q) ||
                                    (a == b && x == y) ||
                                    (p == q && x == y)
    (iMin, iMax, idxMap) = genIndex xs

genIndex :: [Identifier] → (Int, Int, IntMap Identifier)
genIndex xs = (0, IM.size idxMap, idxMap)
  where
    idxMap = IM.fromList $ zip [0..] xs

bigLog :: IO Text
bigLog = readFile "./data/BIGLOG_2015.log"

testTime :: Time
testTime = mkTime 1440200402000000000

main :: IO ()
main = do
  fp <- fromMaybe (error "No file specified") ∘ headMay <$> getArgs
  aliases <- generateAliases <$> getConns (unpack fp)
  printAliases $ processAliases $ mergeUsers aliases
