module RandomNumbers where

import Prelude (undefined)
import MCPrelude

fiveRands :: [Integer]
fiveRands = [first, second, third, fourth, fifth]
  where
    initialSeed = mkSeed 1
    (first, fstSeed) = rand initialSeed
    (second, sndSeed) = rand fstSeed
    (third, thirdSeed) = rand sndSeed
    (fourth, fourthSeed) = rand thirdSeed
    (fifth, _) = rand fourthSeed

randLetter :: Seed -> (Char, Seed)
randLetter seed = (toLetter number, newSeed)
  where
    (number, newSeed) = rand seed

randString3 :: String
randString3 = [first, second, third]
  where
    (first, fstSeed) = randLetter $ mkSeed 1
    (second, sndSeed) = randLetter fstSeed
    (third, _) = randLetter sndSeed
