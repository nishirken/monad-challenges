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
