{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE ViewPatterns              #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

-- | TODO
module Utility.TSLogAnalyzer (module Utility.TSLogAnalyzer) where

import           Control.Arrow                   ((&&&))

import qualified Data.Foldable                   as F
import qualified Data.Traversable                as T

import qualified Data.Graph                      as G
import qualified Data.List                       as L
import qualified Data.Map.Strict                 as M
import qualified Data.Set                        as S

import           Data.ListLike                   (ListLike)
import qualified Data.ListLike                   as LL


import           Data.Map.Strict                 (Map)
import           Data.Maybe                      (fromMaybe, mapMaybe,
                                                  maybeToList)
import           Data.Ord                        (comparing)
import           Data.Set                        (Set)
import           Data.Set.Unicode                ((∩), (∪))
import           Data.Text                       (pack)
import           Data.Tuple.Extra                (dupe)

import           Control.Monad.State.Lazy        (State, runState, execState, modify, put, get, 
                                                  state)

import           ClassyPrelude
import           Prelude.Unicode

import Extra (snd3)

import           Utility.TSLogAnalyzer.Log
import           Utility.TSLogAnalyzer.MsgParse
import           Utility.TSLogAnalyzer.Parse
import           Utility.TSLogAnalyzer.TimeParse

(!@) ∷ Ord κ ⇒ Map κ α → κ → Maybe α
a !@ b = M.lookup b a

ø ∷ Monoid μ ⇒ μ
ø = mempty

sing ∷ MonoPointed μ ⇒ Element μ → μ
sing = opoint

range ∷ (Ord α, Foldable τ, Traversable τ) ⇒ τ α → (α, α)
range xs = F.foldr1 cmp $ dupe <$> xs
  where cmp (a, b) (c, d) = (min a c, max b d)

type IPAddr = (Int, Int, Int, Int)
type AliasMap = Map UserID (Map UserName (Set IPAddr))

generateAliases ∷ FilePath → IO [(UserName, UserID, IPAddr)]
generateAliases fp = do
  logs <- parseConns <$> logParse fp
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

processAliases ∷ [(UserName, UserID, IPAddr)] → AliasMap
processAliases xs = execState (mapM_ process xs) ø
  where
    process ∷ (UserName, UserID, IPAddr) → State AliasMap ()
    process (n, u, i) = modify ((Just . process' n i) `M.alter` u)
    process' name ip = M.insertWith (∪) name (sing ip) . fromMaybe ø

type El a = Element a

omergeBy ∷ IsSequence ς ⇒ (El ς → Bool) → (El ς → El ς → El ς) → ς → ς
omergeBy filt comb xs = if   null trueVals
                        then ø
                        else ofoldr1Ex comb trueVals `cons` falseVals
  where
    (trueVals, falseVals) = partition filt xs


mergeBy ∷ ListLike τ ɛ ⇒ (ɛ → Bool) → (ɛ → ɛ → ɛ) → τ → τ
mergeBy filt comb xs = if   LL.null trueVals
                       then ø
                       else LL.foldr1 comb trueVals `LL.cons` falseVals
  where
    (trueVals, falseVals) = partitionFT filt xs

partitionFT ∷ ListLike τ ɛ ⇒ (ɛ → Bool) → τ → (τ, τ)
partitionFT p xs = execState (LL.mapM_ genState xs) (ø, ø)
  where
    genState e = modify (\(x, y) -> if p e
                                    then (e `LL.cons` x, y)
                                    else (x, e `LL.cons` y))

genIPMap :: [(UserName, UserID, IPAddr)] -> Map IPAddr (Set UserID)
genIPMap xs = execState (mapM_ mapper xs) ø
  where
    mapper (_, uid, ip) = modify $ M.insertWith (∪) ip $ sing uid

mergeMatchingIPs ∷ [(UserName, UserID, IPAddr)] → Set (UserName, UserID, IPAddr)
mergeMatchingIPs xs = S.fromList
                    $ nubTriples
                    $ fst
                    $ flip runState ()
                    $ mapM mapS xs
  where
    nubTriples = ordNubBy snd3 (\(a, x, _) (b, y, _) -> x == y && a == b)
    mapS trip = return $ mapper trip
    mapper trip@(name, _, ip) =
      fromMaybe trip $ process name ip =<< lookup ip (genIPMap xs)
    process name ip us = (,,) <$> Just name <*> minimumMay us <*> Just ip


main ∷ IO ()
main = do
  fp <- fromMaybe (error "No file specified") . headMay <$> getArgs
  aliases <- generateAliases $ unpack fp
  printAliases $ processAliases $ S.toList $ mergeMatchingIPs aliases

