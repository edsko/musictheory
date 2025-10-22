-- | Supporting definitions for working with chords
--
-- Intended for qualified import.
--
-- > import MusicTheory.Chord qualified as Chord
module MusicTheory.Chord (
    -- * Chord names
    Type(..)
  , Name(..)
  ) where

import MusicTheory.Reference

{-------------------------------------------------------------------------------
  Chord names
-------------------------------------------------------------------------------}

data Type =
    -- Basic chords
    Basic_MajorTriad
  | Basic_MinorTriad
  | Basic_MajorSeventh
  | Basic_MinorSeventh
  | Basic_DominantSeventh

    -- Standard Jazz voicings
  | StdJazz_Major
  | StdJazz_Minor
  | StdJazz_Dominant
  | StdJazz_HalfDiminished
  | StdJazz_Altered
  deriving stock (Show)

data Name r = Name{
      root :: Reference r
    , typ  :: Type
    }

deriving instance IsReferenceKind r => Show (Name r)

instance MakeAbsolute Name where
  wrtScale octave scale Name{root, typ} = Name{
        root = absoluteReference octave scale root
      , typ
      }
