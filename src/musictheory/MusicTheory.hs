-- | General purpose definitions
--
-- Intended for unqualified import.
module MusicTheory (
    -- * Classes
    -- ** Transposition
    OctaveShift(..)
  , noOctaveShift
  , TransposeOctave(..)
    -- ** Inversions
  , Inversion(..)
  , rootPosition
  , Invert(..)
    -- ** Distance
  , Distance(..)
  ) where

{-------------------------------------------------------------------------------
  Transposition
-------------------------------------------------------------------------------}

newtype OctaveShift = OctaveShift Int

noOctaveShift :: OctaveShift
noOctaveShift = OctaveShift 0

class TransposeOctave a where
  -- | Move to higher or lower octave
  transposeOctave :: OctaveShift -> a -> a

{-------------------------------------------------------------------------------
  Inversions
-------------------------------------------------------------------------------}

newtype Inversion = Inversion Word

rootPosition :: Inversion
rootPosition = Inversion 0

class Invert a where
  invert :: Inversion -> a -> a

{-------------------------------------------------------------------------------
  Distance
-------------------------------------------------------------------------------}

class Distance a where
  -- | Absolute distance in semitones
  distance :: a -> a -> Word