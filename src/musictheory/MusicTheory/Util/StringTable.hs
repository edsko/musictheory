-- | 'Show' and 'IsString' using a lookup table
--
-- Intended for unqualified import.
module MusicTheory.Util.StringTable (
    -- * String tables
    StringTable(..)
  , stringTableEntry
  , HasStringTable(..)
  , UseShow(..)
    -- ** Construction
  , stringTableEnum
  , stringTableMaybe
  , stringTablePair
  , stringTableNum
    -- * 'Show' vs 'IsString' instances
  , UseStringTable(..)
  , IsString(..)
  ) where

import Data.String
import Data.Tuple (swap)

{-------------------------------------------------------------------------------
  String tables
-------------------------------------------------------------------------------}

newtype StringTable a = StringTable{
      entries :: [(a, String)]
    }
  deriving stock (Functor)

stringTableLookup :: Eq a => StringTable a -> a -> String
stringTableLookup table x =
    case lookup x table.entries of
      Just str -> str
      Nothing  -> "Missing string table entry"

stringTableReverse :: StringTable a -> String -> a
stringTableReverse table str =
    case lookup str (map swap table.entries) of
      Just x  -> x
      Nothing -> error $ "Missing string table entry for " ++ show str

class Eq a => HasStringTable a where
  stringTable :: StringTable a

stringTableEntry :: HasStringTable a => a -> String
stringTableEntry = stringTableLookup stringTable

-- | Derive 'StringTable' using stock-derived 'Show'
--
-- Usage example:
--
-- > data NoteName = C | D | E | F | G | A | B
-- >   deriving stock (Show, Eq, Ord, Enum, Bounded)
-- >   deriving StringTable via UseShow NoteName
-- >   deriving IsString via UseStringTable NoteName
newtype UseShow a = UseShow a
  deriving newtype (Eq)

instance (Show a, Eq a, Enum a, Bounded a) => HasStringTable (UseShow a) where
  stringTable = UseShow <$> stringTableEnum show

{-------------------------------------------------------------------------------
  String table construction
-------------------------------------------------------------------------------}

stringTableEnum :: (Enum a, Bounded a) => (a -> String) -> StringTable a
stringTableEnum f = StringTable [(x, f x) | x <- [minBound .. maxBound]]

-- | String table for 'Maybe'
--
-- All 'Just' values are directly mapped to the string for the value; 'Nothing'
-- is mapped to the specified string.
stringTableMaybe :: String -> StringTable a -> StringTable (Maybe a)
stringTableMaybe nothing table = StringTable $
      (Nothing, nothing)
    : (Just <$> table).entries

-- | String table for pairs
--
-- We simply concatenate the strings for the values in the pair.
stringTablePair :: StringTable a -> StringTable b -> StringTable (a, b)
stringTablePair tableA tableB = StringTable [
      ((a, b), strA ++ strB)
    | (a, strA) <- tableA.entries
    , (b, strB) <- tableB.entries
    ]

-- | String table for numerical types
--
-- This should only be used for small ranges.
stringTableNum :: Num a => [Integer] -> StringTable a
stringTableNum range = StringTable [
      (fromInteger n, show n)
    | n <- range
    ]

{-------------------------------------------------------------------------------
  'Show' vs 'IsString' instances
-------------------------------------------------------------------------------}

-- | Derive 'Show' and 'IsString' using the string table
--
-- This ensures that they are consistent with each other.
--
-- Intended usage example:
--
-- > data Note = Note NoteName (Maybe Accidental)
-- >   deriving stock (Eq)
-- >   deriving (Show, IsString) via UseStringTable Note
newtype UseStringTable a = UseStringTable a

instance HasStringTable a => Show (UseStringTable a) where
  show (UseStringTable x) = "\"" ++ stringTableLookup stringTable x ++ "\""

instance HasStringTable a => IsString (UseStringTable a) where
  fromString = UseStringTable . stringTableReverse stringTable
