module Maybe where

import Prelude (undefined)
import MCPrelude

data Maybe a = Just a | Nothing

instance Show a => Show (Maybe a) where
  show (Just a) = "Just " ++ show a
  show Nothing = "Nothing"

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:xs) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (x:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay x (y:ys) = if x == fst y then Just $ snd y else lookupMay x ys

divMay :: (Ord a, Fractional a) => a -> a -> Maybe a
divMay x 0 = Nothing
divMay x y
  | x < y = Just 0
  | x == y = Just 1
  | otherwise = Just $ iter 0 (x / y)
    where
      iter acc res = if res < 1 then acc else iter (acc + 1) (res - 1)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay [x] = Just x
maximumMay (x:y:xs) = if x > y then maximumMay (x:xs) else maximumMay (y:xs)

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay [x] = Just x
minimumMay (x:y:xs) = if x < y then minimumMay (x:xs) else minimumMay (y:xs)
