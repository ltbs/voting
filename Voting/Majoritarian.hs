module Voting.Majoritarian where

import Voting.Tally

simpleMajoritarian' :: Seats -> Tally a -> [(a,Votes)]
simpleMajoritarian' s = take s . rank

data Result a = Success [(a,Votes)]
              | Tie [(a,Votes)] deriving Show

simpleMajoritarian :: Seats -> Tally a -> Result a
simpleMajoritarian s vs | null tieees = Success win
                        | otherwise = Tie $ win ++ tieees
  where ordered = rank vs
        (win,lose) = splitAt s ordered
        tieees = takeWhile ((==(snd.last) win).snd) lose                 
