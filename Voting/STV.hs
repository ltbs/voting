module Voting.STV where

import Voting.Tally

data RoundResult a = RoundResult { rndWin :: [a]
                                 , rndLose :: [a]
                                 , rndRem :: Tally [a]
                                 } deriving Show

data Result a = Success [a] [RoundResult a]
              | Tie [RoundResult a]
              | Incomplete [RoundResult a] deriving Show

type Quota = Integer

stv :: Ord a => Seats -> Tally [a] -> Result a
stv s t = inner [] t
  where
    inner acc it | sts acc > s  = Tie racc
                 | sts acc == s = Success (concat $ rndWin <$> racc) racc
                 | null it      = Incomplete racc
                 | otherwise    = inner (r:acc) (rndRem r)
      where r      = stvRound it
            sts xs = sum $ (length . rndWin) <$> xs
            racc   = reverse acc
    quota = votesCast t `div` (s+1) + 1
    stvRound r = RoundResult elected eliminated nextRound
      where
        elected = fmap fst $ filterSnd (>= quota) $ headChoice r
        eliminated | (not . null) elected = []
                   | otherwise = fst <$> takeWhile ((==worstCount) . snd) worst
          where
            worst = (reverse . rank . headChoice) r
            worstCount = (snd . head) worst
        nextRound = exclude eliminated $ reduceVotes elected r
          where 
            exclude loser = filterFst (not . null) . mapCandidates (filter (`notElem` loser))
    -- could be more efficient by instead getting the intersection at the start?
            reduceVotes candidates it = (exclude candidates . fmap red) it
              where red (a,c) | head a `elem` candidates = (a, (c * quota) `div` votesForCandidate (head a))
                              | otherwise = (a,c)
                    votesForCandidate c = votesFor c (headChoice it)

irv :: Ord a => Tally [a] -> Result a
irv = stv 1

data TestCandidate = Andrea | Brad | Carter | Delilah deriving (Show, Enum, Eq, Ord)

testTally :: Tally [TestCandidate]
testTally = [ ([Andrea, Brad, Carter, Delilah], 16)
            , ([Andrea, Carter, Brad, Delilah], 24)
            , ([Delilah, Andrea, Brad, Carter], 17) ]

