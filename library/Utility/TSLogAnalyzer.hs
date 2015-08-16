{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | TODO
module Utility.TSLogAnalyzer (module Utility.TSLogAnalyzer) where

import           Control.Arrow                   ((&&&))

import qualified Data.Graph                      as G
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Tree                       as T

import           Data.Foldable

import           Data.List                       (nub, sort, sortBy)
import           Data.Map                        (Map)
import           Data.Maybe                      (fromMaybe, mapMaybe,
                                                  maybeToList)
import           Data.Ord                        (comparing)
import           Data.Set                        (Set)
import           Data.Set.Unicode                ((∩), (∪))
import           Data.Text                       (pack)
import           Data.Tuple.Extra                (dupe)

import           Control.Monad.State.Lazy

import           Prelude.Unicode

import           System.Environment              (getArgs)

import           Utility.TSLogAnalyzer.Log
import           Utility.TSLogAnalyzer.MsgParse
import           Utility.TSLogAnalyzer.Parse
import           Utility.TSLogAnalyzer.TimeParse

extractIDIP ∷ Connection → Maybe (UserID, Set Int)
extractIDIP (Connection _ _ uid (Just (IP ip _)) _) = Just (uid, wr ip)
extractIDIP _ = Nothing

extractIPID ∷ Connection → Maybe (Int, Set UserID)
extractIPID (Connection _ _ uid (Just (IP ip _)) _) = Just (ip, wr uid)
extractIPID _ = Nothing

extractIDIPs ∷ [Connection] → Map UserID (Set Int)
extractIDIPs = M.fromListWith (∪) . mapMaybe extractIDIP

extractIPIDs ∷ [Connection] → Map Int (Set UserID)
extractIPIDs = M.fromListWith (∪) . mapMaybe extractIPID

addIndices ∷ [Set UserID] → [(Int, Set UserID)]
addIndices = zip [1..]

sortTuple ∷ Ord α ⇒ (α, α) → (α, α)
sortTuple (i1, i2) = if i1 > i2 then (i2, i1) else (i1, i2)

trans ∷ [(α, β)] → Maybe (β, [α])
trans [] = Nothing
trans xs = Just (snd $ head xs, map fst xs)

(!@) ∷ Ord κ ⇒ Map κ α → κ → Maybe α
a !@ b = M.lookup b a

ø ∷ Set α
ø = S.empty

wr ∷ α → Set α
wr = S.singleton

proc1 ∷ Ord β ⇒ (α, Set β) → (α, Set β) → [(α, α)]
proc1 (i1, s1) (i2, s2)
    | (s1 ∩ s2) ≢ ø             = [(i1, i2)]
    | otherwise                 = []

proc2 ∷ (Ord β, Foldable τ) ⇒ τ (α, Set β) → [(α, α)]
proc2 xs = concatMap ((`concatMap` xs) . proc1) xs

proc3 ∷ [Set UserID] → [(Int, Int)]
proc3 = nub . map sortTuple . proc2 . addIndices

proc4 ∷ Ord α ⇒ [(α, α)] → (α, α)
proc4 xs = (minimum r, maximum r) where r = uncurry (++) $ unzip xs

proc5 ∷ (Eq a, Foldable t, Foldable t') ⇒ t (a, t' a) → a → a
proc5 xs x
    | null z                = x
    | otherwise             = head z
    where
    z = concatMap (\(y, ys) → [y | x `elem` ys]) xs

proc6 ∷ [(α, Connection)] → [(α, Connection)]
proc6 pl = map h pl
  where
    cons = map snd pl
    map1 = M.map S.toList $ extractIDIPs cons
    map2 = M.map S.toList $ extractIPIDs cons
    f m x = concat $ mapMaybe (m !@) x
    g k x = S.fromList (k : f map2 x)
    r1 = (\xs → [(a, b) | (a, b) <- xs, a ≡ b]) $ proc3 $ nub $ M.elems (M.mapWithKey g map1)
    r2 = G.components $ G.buildG (proc4 r1) r1
    r3 = filter ((> 1) . length . T.flatten) r2
    isolate = head &&& sort . tail
    r4 = map (isolate . concat . T.levels) r3
    h (a, c@(Connection { connUID = uid }))
      = (a, c { connUID = UserID $ proc5 r4 $ getUID uid })

range ∷ (Ord a, Foldable t, Traversable t) ⇒ t a → (a, a)
range xs = foldr1 cmp $ dupe <$> xs
  where cmp (a, b) (c, d) = (min a c, max b d)

processor ∷ FilePath → IO ()
processor fp = do
  r4 <- proc6 . parseLogs <$> logParse fp
  let userIDs = (\(a, b) -> [a .. b]) $ range $ map (connUID . snd) r4
  let idf y = filter ((≡ y) . snd) $ map ((connName &&& connUID) . snd) r4
  mapM_ print $ sortBy (comparing fst) $ mapMaybe trans $ tail $ nub $ map (nub . idf) userIDs

-- let r5 = r4
-- let r6 = M.fromList $ (flip zip) [1..] $ sort $ nub $ map (userID . snd) r5
-- let k (a, Connection c n id ip r) = do { i <- M.lookup id r6; return (a, Connection c n i ip r) }
-- let r7 = concatMap (maybeToList . k) r5
-- return r5
-- print r7
-- print (nub $ map (userID . snd) r7)
-- let f x m = concat . Set.toList . Set.map (maybeToList . Map.lookup m) x
-- let map3 = Map.mapWithKey (\k x → k : (f x map2)) map1
-- sequence (map print (sortBy (comparing fst) $ (filter (\(_, x) → userName x ≡ pack "taktoa") $ r7)))

main ∷ IO ()
main = do
        args <- getArgs
        let fp = if null args then error "No file specified" else head args
        processor fp
        putStrLn "hi"

--  sequence (map print (sortBy (comparing fst) $ (filter (\(_, x) → userID x ≡ 128) $ r)))
--  print (S.fromList (map S.fromList (map (\y → map (userName . snd) (filter (\(_, x) → userID x ≡ y) r)) [1..205])))
--  sequence (map print (S.toList $ S.fromList $ map (\x → (snd (head x), S.fromList (map fst x))) (map (\y → map ((\x → (userName x, userID x)) . snd) (filter (\(_, x) → userID x ≡ y) r)) [1..205])))
--  sequence (map print (tail $ nub $ map S.fromList $ flip map [1..205] (\y → map ((\x → (userName x, userID x)) . snd) $ filter ((≡ y) . userID . snd) r)))
--  let trans xs = (snd $ head xs, map fst xs)
--  sequence (map print $ sortBy (comparing fst) $ map trans $ (tail $ nub $ map nub $ flip map [1..205] (\y → map ((\x → (userName x, userID x)) . snd) $ filter ((≡ y) . userID . snd) r)))

