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

import MusicTheory.Reference
import MusicTheory.Scale (Scale)
import MusicTheory.Note.Octave (Octave)

{-------------------------------------------------------------------------------
  Chord names
-------------------------------------------------------------------------------}

data Type =
    MajorTriad
  | MinorTriad
  | Major7
  | Minor7
  | Dominant7
  | HalfDiminished
  | Altered
  | AlteredFlat9
  | Sus
  deriving stock (Show, Eq)

-- | Chord name
data Name r = Name{
      root :: Reference r
    , typ  :: Type
    }

deriving instance IsReferenceKind r => Show (Name r)
deriving instance IsReferenceKind r => Eq   (Name r)

nameWrtScale :: Scale -> Octave -> Name Rel -> Name Abs
nameWrtScale scale octave Name{root, typ} = Name{
      root = referenceWrtScale scale octave root
    , typ
    }
