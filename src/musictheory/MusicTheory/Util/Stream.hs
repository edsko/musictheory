-- | Streams (infinite lists)
--
-- Intended for qualified import.
--
-- > import MusicTheory.Util.Stream (Stream)
-- > import MusicTheory.Util.Stream qualified as Stream
module MusicTheory.Util.Stream (
    Stream -- abstract
    -- * Introduction
  , cycle
  , tails
    -- * Elimination
  , toList
  , take
  , (!!)
  ) where

import Prelude hiding (cycle, take, (!!))

import Data.List.NonEmpty (NonEmpty(..))

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Stream a = Stream a (Stream a)
  deriving stock (Functor)

{-------------------------------------------------------------------------------
  Introduction
-------------------------------------------------------------------------------}

cycle :: forall a. NonEmpty a -> Stream a
cycle (x :| xs) = Stream x (go xs)
  where
    go :: [a] -> Stream a
    go []       = cycle (x :| xs)
    go (y : ys) = Stream y (go ys)

-- | Tails
--
-- >    take 7 (fmap (take 5) (tails (cycle (NE.fromList "Abc"))))
-- > == ["AbcAb","bcAbc","cAbcA","AbcAb","bcAbc","cAbcA","AbcAb"]
tails :: Stream a -> Stream (Stream a)
tails s@(Stream _ xs) = Stream s (tails xs)

{-------------------------------------------------------------------------------
  Elimination
-------------------------------------------------------------------------------}

toList :: Stream a -> [a]
toList (Stream x xs) = x : toList xs

take :: Word -> Stream a -> [a]
take 0 _             = []
take n (Stream x xs) = x : take (n - 1) xs

(!!) :: Stream a -> Word -> a
Stream x _  !! 0 = x
Stream _ xs !! n = xs !! (n - 1)
