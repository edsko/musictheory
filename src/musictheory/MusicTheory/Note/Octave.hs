-- | Octaves
--
-- Intended for qualified import.
--
-- > import MusicTheory.Note.Octave (Octave(..))
-- > import MusicTheory.Note.Octave qualified as Octave
module MusicTheory.Note.Octave (
    Octave(..)
    -- * Simple functions
  , middle
  , aboveMiddle
  ) where

import MusicTheory.Util.StringTable

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Octave
newtype Octave = Octave Word
  deriving stock (Eq)
  deriving newtype (Enum)
  deriving (Show, IsString) via UseStringTable Octave

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance Bounded Octave where
  minBound = Octave 0
  maxBound = Octave 9

instance HasStringTable Octave where
  stringTable = stringTableEnum $ \(Octave o) ->
      case o of
        0 -> "0"
        1 -> "1"
        2 -> "2"
        3 -> "3"
        4 -> "4"
        5 -> "5"
        6 -> "6"
        7 -> "7"
        8 -> "8"
        9 -> "9"
        _ -> error "Invalid Octave"

{-------------------------------------------------------------------------------
  Simple functions
-------------------------------------------------------------------------------}

-- | Octave containing middle C
middle :: Octave
middle = Octave 4

-- | How many octaves is the specified octave about the middle octave?
aboveMiddle :: Octave -> Int
aboveMiddle (Octave o) = fromIntegral o - 4
