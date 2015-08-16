{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# NoMonomorphismRestriction #-}

-- | TODO
module Utility.TSLogAnalyzer (module UtilityTSLogAnalyzer) where

import           Control.Arrow                   ((&&&))

import qualified Data.Graph                      as G
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Tree                       as T

import           Data.List                       (nub, sort, sortBy)
import           Data.Map                        (Map)
import           Data.Maybe                      (mapMaybe, maybeToList)
import           Data.Ord                        (comparing)
import           Data.Set                        (Set)
import           Data.Set.Unicode                ((∩), (∪))
import           Data.Text                       (pack)

import           Prelude.Unicode

import           System.Environment              (getArgs)

import           Utility.TSLogAnalyzer
import           Utility.TSLogAnalyzer.Log
import           Utility.TSLogAnalyzer.MsgParse
import           Utility.TSLogAnalyzer.Parse
import           Utility.TSLogAnalyzer.TimeParse

extractIDIP :: Connection -> Maybe (UserID, Set Int)
extractIDIP (Connection _ _ uid (Just (IP ip _)) _) = Just (uid, wr ip)
extractIDIP _ = Nothing

extractIPID :: Connection -> Maybe (Int, Set UserID)
extractIPID (Connection _ _ uid (Just (IP ip _)) _) = Just (ip, wr uid)
extractIPID _ = Nothing

extractIDIPs :: [Connection] -> Map UserID (Set Int)
extractIDIPs = M.fromListWith (∪) . mapMaybe extractIDIP

extractIPIDs :: [Connection] -> Map Int (Set UserID)
extractIPIDs = M.fromListWith (∪) . mapMaybe extractIPID

addIndices :: [Set UserID] -> [(Int, Set UserID)]
addIndices = zip [1..]

sortTuple :: Ord α => (α, α) -> (α, α)
sortTuple (i1, i2) = if i1 > i2 then (i2, i1) else (i1, i2)

trans [] = Nothing
trans xs = Just (snd $ head xs, map fst xs)

a !@ b = M.lookup b a
ø = S.empty
wr = S.singleton
mfl = M.fromList
mflw = M.fromListWith
mtl = M.toList
sfl = S.fromList
stl = S.toList

proc1 :: Ord β => (α, Set β) -> (α, Set β) -> [(α, α)]
proc1 (i1, s1) (i2, s2)
    | (s1 ∩ s2) ≢ ø             = [(i1, i2)]
    | otherwise                 = []

proc2 xs = concatMap ((`concatMap` xs) . proc1) xs

proc3 = nub . map sortTuple . proc2 . addIndices

proc4 :: Ord α => [(α, α)] -> (α, α)
proc4 xs = (minimum r, maximum r) where r = uncurry (++) $ unzip xs

proc5 xs x
    | null z                = x
    | otherwise             = head z
    where
    z = concatMap (\(y, ys) -> [y | x `elem` ys]) xs

proc6 pl = do
        let cons = map snd pl
        let map1 = M.map S.toList $ extractIDIPs cons
        let map2 = M.map S.toList $ extractIPIDs cons
        let f m x = concat $ mapMaybe (m !@) x
        let g k x = S.fromList (k : f map2 x)
        let r1 = (\xs -> [(a, b) | (a, b) <- xs, a ≡ b]) $ proc3 $ nub $ M.elems (M.mapWithKey g map1)
        let r2 = G.components $ G.buildG (proc4 r1) r1
        let r3 = filter ((> 1) . length . T.flatten) r2
        let isolate (x:xs) = (x, sort xs)
        let r4 = map (isolate . concat . T.levels) r3
        let h (a, Connection c n id ip r) = (a, Connection c n (proc5 r4 id) ip r)
        return $ map h pl

processor fp = do
        le <- logParse fp
        let pl = parseLogs le
        r4 <- proc6 pl
        let (min, max) = (minimum z, maximum z) where z = nub $ map (userID . snd) r4
        let idf y = filter ((≡ y) . snd) $ map ((userName &&& userID) . snd) r4
        mapM print $ sortBy (comparing fst) $ mapMaybe trans $ tail $ nub $ map (nub . idf) [min..max]
        --let r5 = r4
        --let r6 = M.fromList $ (flip zip) [1..] $ sort $ nub $ map (userID . snd) r5
        --let k (a, Connection c n id ip r) = do { i <- M.lookup id r6; return (a, Connection c n i ip r) }
        --let r7 = concatMap (maybeToList . k) r5
        --return r5
        --print r7
        --print (nub $ map (userID . snd) r7)
        --let f x m = concat . Set.toList . Set.map (maybeToList . Map.lookup m) x
        --let map3 = Map.mapWithKey (\k x -> k : (f x map2)) map1
        --sequence (map print (sortBy (comparing fst) $ (filter (\(_, x) -> userName x ≡ pack "taktoa") $ r7)))

main :: IO ()
main = do
        args <- getArgs
        let fp = if null args then error "No file specified" else head args
        processor fp
        putStrLn "hi"
        --sequence (map print (sortBy (comparing fst) $ (filter (\(_, x) -> userID x ≡ 128) $ r)))
        --print (S.fromList (map S.fromList (map (\y -> map (userName . snd) (filter (\(_, x) -> userID x ≡ y) r)) [1..205])))
        --sequence (map print (S.toList $ S.fromList $ map (\x -> (snd (head x), S.fromList (map fst x))) (map (\y -> map ((\x -> (userName x, userID x)) . snd) (filter (\(_, x) -> userID x ≡ y) r)) [1..205])))
        --sequence (map print (tail $ nub $ map S.fromList $ flip map [1..205] (\y -> map ((\x -> (userName x, userID x)) . snd) $ filter ((≡ y) . userID . snd) r)))
        --let trans xs = (snd $ head xs, map fst xs)
        --sequence (map print $ sortBy (comparing fst) $ map trans $ (tail $ nub $ map nub $ flip map [1..205] (\y -> map ((\x -> (userName x, userID x)) . snd) $ filter ((≡ y) . userID . snd) r)))

