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

queryGreek :: GreekData -> String -> Maybe Double
queryGreek greekData key = case lookupMay key greekData of
  (Just xs) -> case headMay xs of
    (Just x) -> case tailMay xs of
      (Just ys) -> case maximumMay ys of
        (Just m) -> case fromIntegral m `divMay` fromIntegral x of
          Nothing -> Nothing
          x -> x
        Nothing -> Nothing
      Nothing -> Nothing
    Nothing -> Nothing
  Nothing -> Nothing

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link (Just x) f = f x
link Nothing _ = Nothing

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 greekData key =
  lookupMay key greekData `link` \xs ->
    headMay xs `link` \x ->
      tailMay xs `link` \ys ->
        maximumMay ys `link` \m -> fromIntegral m `divMay` fromIntegral x

salaries :: [(String, Integer)]
salaries =
  [
    ("alice", 105000)
    , ("bob", 90000)
    , ("carol", 85000)
  ]

mkMay :: a -> Maybe a
mkMay = Just

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries xs key1 key2 =
  lookupMay key1 xs `link` \sal1 -> lookupMay key2 xs `link` \sal2 -> mkMay $ sal1 + sal2

tailProd :: Num a => [a] -> Maybe a
tailProd xs = tailMay xs `link` \ys -> mkMay $ product ys

tailSum :: Num a => [a] -> Maybe a
tailSum xs = tailMay xs `link` \ys -> mkMay $ sum ys

transMaybe :: ([a] -> b) -> [a] -> Maybe b
transMaybe f xs = tailMay xs `link` \ys -> mkMay $ f ys

tailProd' = transMaybe product
tailSum' = transMaybe sum

tailMax :: Ord a => [a] -> Maybe a
tailMax xs = combine $ transMaybe maximumMay xs

tailMin :: Ord a => [a] -> Maybe a
tailMin xs = combine $ transMaybe minimumMay xs

combine :: Maybe (Maybe a) -> Maybe a
combine Nothing = Nothing
combine (Just Nothing) = Nothing
combine (Just x) = x
