-- | Utilities for working with lists
--
-- Intended for qualified import.
--
-- > import MusicTheory.Util.List qualified as List
module MusicTheory.Util.List (
    -- * Construction
    alternate
  , odds
  , evens
  , rotate
  , Elem(..)
  , markElems
  ) where

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Alternate elements from two lists
--
-- > alternate [1, 3 .. 9] [2, 4 .. 10] == [1 .. 10]
-- > alternate [1, 3 .. 9] [2, 4 .. 8]  == [1 .. 9]
-- > alternate [1, 3 .. 7] [2, 4 .. 10] == [1 .. 8] ++ [10]
alternate :: [a] -> [a] -> [a]
alternate []     ys = ys
alternate (x:xs) ys = x : alternate ys xs

-- | Elements at odd indices
--
-- > odds @Int [1..10] == [1,3,5,7,9]
odds :: [a] -> [a]
odds []     = []
odds (x:xs) = x : evens xs

-- | Elements at even indices
--
-- > evens @Int [1..10] == [2,4,6,8,10]
evens :: [a] -> [a]
evens []     = []
evens (_:xs) = odds xs

-- | Rotate elements
--
-- > rotate   0  "(abc)" == "(abc)"
-- > rotate   1  "(abc)" == "abc)("
-- > rotate   2  "(abc)" == "bc)(a"
-- > rotate   3  "(abc)" == "c)(ab"
-- > rotate   4  "(abc)" == ")(abc"
-- > rotate   5  "(abc)" == "(abc)"
-- > rotate   6  "(abc)" == "abc)("
-- > rotate   7  "(abc)" == "bc)(a"
--
-- > rotate   0  "(abc)" == "(abc)"
-- > rotate (-1) "(abc)" == ")(abc"
-- > rotate (-2) "(abc)" == "c)(ab"
-- > rotate (-3) "(abc)" == "bc)(a"
-- > rotate (-4) "(abc)" == "abc)("
-- > rotate (-5) "(abc)" == "(abc)"
-- > rotate (-6) "(abc)" == ")(abc"
-- > rotate (-7) "(abc)" == "c)(ab"
rotate :: Int -> [a] -> [a]
rotate i xs = take (length xs) $ drop (i `mod` length xs) (cycle xs)

data Elem a = Elem{
      value   :: a
    , isFirst :: Bool
    , isLast  :: Bool
    }
  deriving stock (Show)

-- | Mark elements
--
-- >    markElems "abc"
-- > == [ Elem {value = 'a', isFirst = True,  isLast = False}
-- >    , Elem {value = 'b', isFirst = False, isLast = False}
-- >    , Elem {value = 'c', isFirst = False, isLast = True }
-- >    ]
markElems :: [a] -> [Elem a]
markElems = \case
    []      -> []
    [x]     -> [Elem x True True]
    x:x':xs -> Elem x True False : go x' xs
  where
    go :: a -> [a] -> [Elem a]
    go x []      = [Elem x False True]
    go x (x':xs) = Elem x False False : go x' xs
