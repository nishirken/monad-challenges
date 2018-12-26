module RandomNumbers where

import Prelude (undefined)
import MCPrelude

type Gen a = Seed -> (a, Seed)

fiveRands :: [Integer]
fiveRands = [first, second, third, fourth, fifth]
  where
    initialSeed = mkSeed 1
    (first, fstSeed) = rand initialSeed
    (second, sndSeed) = rand fstSeed
    (third, thirdSeed) = rand sndSeed
    (fourth, fourthSeed) = rand thirdSeed
    (fifth, _) = rand fourthSeed

randLetter :: Gen Char
randLetter seed = (toLetter number, newSeed)
  where
    (number, newSeed) = rand seed

randString3 :: String
randString3 = [first, second, third]
  where
    (first, fstSeed) = randLetter $ mkSeed 1
    (second, sndSeed) = randLetter fstSeed
    (third, _) = randLetter sndSeed

randEven :: Gen Integer
randEven seed = (number * 2, newSeed)
  where
    (number, newSeed) = rand seed

randOdd :: Gen Integer
randOdd seed = (number * 2 + 1, newSeed)
  where
    (number, newSeed) = rand seed

randTen :: Gen Integer
randTen seed = (number * 10, newSeed)
  where
    (number, newSeed) = rand seed
