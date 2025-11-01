-- | Supporting definitions for working with chords
--
-- Intended for qualified import.
--
-- > import MusicTheory.Chord qualified as Chord
module MusicTheory.Chord (
    -- * Chord names
    Type(..)
  , Name(..)
  , nameWrtScale
  ) where

import MusicTheory
import MusicTheory.Note.Octave (Octave)
import MusicTheory.Reference
import MusicTheory.Scale (Scale)

{-------------------------------------------------------------------------------
  Chord names
-------------------------------------------------------------------------------}

data Type =
    MajorTriad
  | MinorTriad
  | DiminishedTriad
  | Major7
  | Minor7
  | Dominant7
  | HalfDiminished
  | Altered
  | SevenFlat9
  | Diminished7
  | Sus
  deriving stock (Show, Eq)

-- | Chord name
data Name r = Name{
      root :: Reference r
    , typ  :: Type
    }

deriving instance IsReferenceKind r => Show (Name r)
deriving instance IsReferenceKind r => Eq   (Name r)

instance TransposeOctave (Name Abs) where
  transposeOctave d (Name root typ) = Name (transposeOctave d root) typ

nameWrtScale :: Scale -> Octave -> Name Rel -> Name Abs
nameWrtScale scale octave Name{root, typ} = Name{
      root = referenceWrtScale scale octave root
    , typ
    }
