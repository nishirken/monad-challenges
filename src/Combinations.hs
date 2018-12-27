module Combinations where

import Prelude (undefined)
import MCPrelude

allPairs :: [a] -> [b] -> [(a,b)]
allPairs [] _ = []
allPairs _ [] = []
allPairs (x:xs) ys = map ((,) x) ys ++ allPairs xs ys

data Card = Card Int String

instance Show Card where
  show (Card x y) = show x ++ y

allCards :: [Int] -> [String] -> [Card]
allCards [] _ = []
allCards _ [] = []
allCards (x:xs) ys = map (Card x) ys ++ allCards xs ys

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs _ [] _ = []
allCombs _ _ [] = []
allCombs f (x:xs) ys = map (f x) ys ++ allCombs f xs ys

allPairs' = allCombs (,)
allCards' = allCombs Card

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 _ [] _ _ = []
allCombs3 _ _ [] _ = []
allCombs3 _ _ _ [] = []
allCombs3 f (x:xs) ys zs = concatMap (\z -> map (\y -> f x y z) ys) zs ++ allCombs3 f xs ys zs

combStep :: [a -> b] -> [a] -> [b]
combStep [] _ = []
combStep _ [] = []
combStep (f:fs) xs = map f xs ++ combStep fs xs

allCombs' :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs' f xs = combStep $ map f xs

allCombs3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3' f xs ys = combStep (combStep (map f xs) ys)
