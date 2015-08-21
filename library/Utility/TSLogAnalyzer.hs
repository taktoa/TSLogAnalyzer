{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

-- | Main functions for log analysis
module Utility.TSLogAnalyzer (module Utility.TSLogAnalyzer) where

import           ClassyPrelude
import           Prelude.Unicode

import qualified Data.Foldable                   as Fold
import qualified Data.Traversable                as Trav

import qualified Data.Graph                      as G
import qualified Data.IntMap.Strict              as IM
import qualified Data.List                       as L
import qualified Data.ListLike                   as LL
import qualified Data.Map.Strict                 as M
import qualified Data.Set                        as S
import qualified Data.Tree                       as T

import           Control.Monad.State.Lazy        (State)
import           Data.IntMap.Strict              (IntMap)
import           Data.ListLike                   (ListLike)
import           Data.Map.Strict                 (Map)
import           Data.Set                        (Set)

import           Control.Arrow                   ((&&&))
import           Control.Monad.State.Lazy        (get, modify, put, state)
import           Control.Monad.State.Lazy        (evalState, execState,
                                                  runState)
import           Data.Maybe                      (fromMaybe, mapMaybe,
                                                  maybeToList)
import           Data.Ord                        (comparing)
import           Data.Set.Unicode                ((∩), (∪))
import           Data.Tuple.Extra                (dupe)
import           Extra                           (snd3)

import           Utility.TSLogAnalyzer.Log
import           Utility.TSLogAnalyzer.MsgParse
import           Utility.TSLogAnalyzer.Parse
import           Utility.TSLogAnalyzer.TimeParse

import           Utility.TSLogAnalyzer.Util

(!@) ∷ Ord κ ⇒ Map κ α → κ → Maybe α
a !@ b = M.lookup b a

ø ∷ Monoid μ ⇒ μ
ø = mempty

sing ∷ MonoPointed μ ⇒ Element μ → μ
sing = opoint

range ∷ (Ord α, Foldable τ, Traversable τ) ⇒ τ α → (α, α)
range xs = Fold.foldr1 cmp $ dupe <$> xs
  where cmp (a, b) (c, d) = (min a c, max b d)

type IPAddr = (Int, Int, Int, Int)
type AliasMap = Map UserID (Map UserName (Set IPAddr))
type Identifier = (UserName, UserID, IPAddr)

generateAliases ∷ FilePath → IO [Identifier]
generateAliases fp = do
  !logs <- parseConns <$> logParse fp
  return $ hashNub $ mapMaybe (process . snd) logs
  where
    process (Connection _ _    _   Nothing   _) = Nothing
    process (Connection _ name uid (Just ip) _) = Just (name, uid, getOctets ip)

printAliases ∷ AliasMap → IO ()
printAliases = mapM_ (uncurry helper1) . M.toList
  where
    helper1 ∷ UserID → Map UserName (Set IPAddr) → IO ()
    helper1 (UserID uid) m = do
      putStrLn $ "UserID: " <> tshow uid
      mapM_ (uncurry helper2) $ M.toList m
    helper2 ∷ UserName → Set IPAddr → IO ()
    helper2 (UserName name) s = do
      putStrLn $ "  " <> "Name: " <> name
      mapM_ (putStrLn . ("    " <>) . showIP) s
    showIP (o1, o2, o3, o4) = tshow o1 <> "."
                           <> tshow o2 <> "."
                           <> tshow o3 <> "."
                           <> tshow o4

processAliases ∷ [Identifier] → AliasMap
processAliases xs = execState (mapM_ process xs) ø
  where
    process (n, u, i) = modify ((Just . process' n i) `M.alter` u)
    process' name ip = M.insertWith (∪) name (sing ip) . fromMaybe ø

data BiMultiMap α β = BMM (Map α (β, Set β)) (Map β α) deriving Eq

instance (Ord α, Show α, Ord β, Show β) ⇒ Show (BiMultiMap α β) where
  show = unpack ∘ showBMM

-- | Create a BiMultiMap from the given list of key / value set pairs
bmmFromList ∷ (Ord α, Ord β) ⇒ [(α, [β])] → BiMultiMap α β
bmmFromList = uncurry BMM ∘ ((apply ∘ fmap forward) &&& (apply ∘ fmap backward))
  where
    apply = M.fromList ∘ L.concat
    forward  (a, b) = case b of
      (_:_) -> [(a, (L.minimum b, S.fromList b))]
      _     -> []
    backward (a, b) = fmap (,a) b

-- | Look up the set of values corresponding to a given key.
bmmLookupF ∷ (Ord α, Ord β) ⇒ α → BiMultiMap α β → Set β
bmmLookupF k (BMM f _) = concat $ snd <$> M.lookup k f

-- | Look up the distinguished element in the set for the given key.
bmmLookupD ∷ (Ord α, Ord β) ⇒ α → BiMultiMap α β → Maybe β
bmmLookupD k (BMM f _) = fst <$> M.lookup k f

-- | Look up the key corresponding to the given value.
bmmLookupB ∷ (Ord α, Ord β) ⇒ β → BiMultiMap α β → Maybe α
bmmLookupB v (BMM _ b) = M.lookup v b

-- | Find the distinguished element corresponding to the set in which the given
--   value is contained, if it exists.
bmmRoundtrip ∷ (Ord α, Ord β) ⇒ BiMultiMap α β → α → Maybe α
bmmRoundtrip m k = bmmLookupD k m >>= flip bmmLookupB m

-- | Show a BiMultiMap as a 'Text'
showBMM ∷ (Ord α, Show α, Ord β, Show β) ⇒ BiMultiMap α β → Text
showBMM (BMM m _) = concatMap (\(b, a) -> tshow a <> " -> " <> tshow b <> "\n")
                  $ M.toList
                  $ M.map fst m

identGroups ∷ [Identifier] → BiMultiMap Int Identifier
identGroups xs = bmmFromList
               $ zip [0..]
               $ map (mapMaybe (`IM.lookup` idxMap) ∘ T.flatten)
               $ G.components
               $ identGraph xs
  where
    idxMap = IM.fromList $ zip [0..] xs

identGraph ∷ [Identifier] → G.Graph
identGraph xs = G.buildG iBounds
              $ flip evalState idxMap
              $ concatMapM process iRange
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
    iRange  = [iMin .. iMax]
    iBounds = (iMin, iMax)
    (iMin, iMax) = (0, IM.size idxMap)
    idxMap = IM.fromList $ zip [0..] xs

bigLog ∷ IO Text
bigLog = readFile "./data/BIGLOG_2015.log"

main ∷ IO ()
main = do
  fp <- fromMaybe (error "No file specified") . headMay <$> getArgs
  aliases <- generateAliases $ unpack fp
  -- printAliases $ processAliases $ S.toList $ mergeMatchingIPs aliases

