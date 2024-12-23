module Util where

rotate :: Int -> [a] -> [a]
rotate n xs = take (length xs) $ drop n (cycle xs)

shift :: forall a. (Enum a, Bounded a, Eq a) => Int -> a -> a
shift n x
  | n == 0    = x
  | n >  0    = if x == maxBound
                  then shift (n - 1) minBound
                  else shift (n - 1) (succ x)
  | otherwise = if x == minBound
                  then shift (n + 1) maxBound
                  else shift (n + 1) (pred x)

-- | Elements at even indices in the list
--
-- > evens [1 .. 10] == [2,4,6,8,10]
evens :: [a] -> [a]
evens []     = []
evens (_:xs) = odds xs

-- | Elements at odd indices in the list
--
-- > odds [1 .. 10] == [1,3,5,7,9]
odds :: [a] -> [a]
odds []     = []
odds (x:xs) = x : evens xs
