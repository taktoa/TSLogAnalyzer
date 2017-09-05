{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

module Utility.TSLogAnalyzer.BiMultiMap ( BiMultiMap
                                        , fromList
                                        , getForward, getBackward, getDist
                                        , lookupF,    lookupD,     lookupB
                                        , roundTrip
                                        ) where

import           ClassyPrelude   hiding (fromList)
import           Prelude.Unicode

import qualified Data.List       as L
import qualified Data.Map.Strict as M
import qualified Data.Set        as S

import           Data.Map.Strict (Map)
import           Data.Set        (Set)


data BiMultiMap α β = BMM (Map α (β, Set β)) (Map β α) deriving Eq

instance (Ord α, Show α, Ord β, Show β) => Show (BiMultiMap α β) where
  show (BMM m _) = unpack
                 $ concatMap (\(b, a) -> tshow a <> " -> " <> tshow b <> "\n")
                 $ M.toList
                 $ M.map fst m


-- | Get the map from keys to value sets
getForward :: BiMultiMap α β -> Map α (Set β)
getForward (BMM m _) = M.map snd m

-- | Get the map from values to keys
getBackward :: BiMultiMap α β -> Map β α
getBackward (BMM _ m) = m

-- | Get the map from keys to distinguished values
getDist :: BiMultiMap α β -> Map α β
getDist (BMM m _) = M.map fst m

-- | Create a BiMultiMap from the given list of key / value set pairs
fromList :: (Ord α, Ord β) => [(α, [β])] -> BiMultiMap α β
fromList = uncurry BMM ∘ (apply forward &&& apply backward)
  where
    apply f = M.fromList ∘ L.concat ∘ fmap f
    forward  (a, b) = case b of
      (_:_) -> [(a, (L.minimum b, S.fromList b))]
      _     -> []
    backward (a, b) = fmap (,a) b

-- | Look up the set of values corresponding to a given key.
lookupF :: (Ord α, Ord β) => α -> BiMultiMap α β -> Set β
lookupF k (BMM f _) = concat $ snd <$> M.lookup k f

-- | Look up the distinguished element in the set for the given key.
lookupD :: (Ord α, Ord β) => α -> BiMultiMap α β -> Maybe β
lookupD k (BMM f _) = fst <$> M.lookup k f

-- | Look up the key corresponding to the given value.
lookupB :: (Ord α, Ord β) => β -> BiMultiMap α β -> Maybe α
lookupB v (BMM _ b) = M.lookup v b

-- | Find the distinguished element corresponding to the set in which the given
--   value is contained, if it exists.
roundTrip :: (Ord α, Ord β) => BiMultiMap α β -> α -> Maybe α
roundTrip m k = lookupD k m >>= flip lookupB m
