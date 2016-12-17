module Voting.Tally where

import qualified Data.Map as M
import qualified Data.List as L
import Control.Arrow (first)

type Votes = Int
type Tally a = [(a, Votes)]
type Seats = Int

tally :: (Ord a) => [a] -> Tally a
tally = M.toList . M.unionsWith (+) . fmap toTally
  where toTally x = M.fromList [(x, 1)]

votesCast :: Tally a -> Votes
votesCast = sum . fmap snd



votesFor :: Ord a => a -> Tally a -> Votes
votesFor c = head . map snd . filterFst (==c)

mapFst :: (a -> c) -> (a,b) -> (c,b)
mapFst f (a,b) = (f a, b)

rank :: Tally a -> Tally a
rank = L.sortOn (negate . snd)
                 
mapSnd :: (b -> c) -> (a,b) -> (a,c)
mapSnd f (a,b) = (a, f b)

filterFst :: (a -> Bool) -> [(a,b)] -> [(a,b)]
filterFst f = filter (f . fst)

filterSnd :: (b -> Bool) -> [(a,b)] -> [(a,b)]
filterSnd f = filter (f . snd)

mapVotes :: (Votes -> Votes) -> Tally a -> Tally a
mapVotes f = fmap (mapSnd f)

mapCandidates :: (a -> a) -> Tally a -> Tally a
mapCandidates f = fmap (mapFst f)

headChoice :: Ord a => Tally [a] -> Tally a
headChoice a = M.toList $ M.fromListWith (+) (fmap (first head) a)

