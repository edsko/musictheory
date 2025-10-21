-- | Chord names
--
-- Intended for qualified import.
--
-- > import MusicTheory.Chord.Name qualified as Chord (Name(..))
-- > import MusicTheory.Chord.Name qualified as Chord.Name
module MusicTheory.Chord.Name (
    Name(..)
    -- * Construction
  , chordI
  , chordNth
  ) where

import MusicTheory.Chord.Type qualified as Chord (Type)
import MusicTheory.Note (Note)
import MusicTheory.Note qualified as Note
import MusicTheory.Scale (Scale)
import MusicTheory.Scale qualified as Scale

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Name = Name{
      note :: Note
    , typ  :: Chord.Type
    }
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | I-chord in a given scale
chordI :: Scale.Name -> Chord.Type -> Name
chordI scale = Name (Note.fromSimple $ Scale.rootNote scale)

-- | N-th chord in a given scale
chordNth :: Scale -> Scale.Degree -> Chord.Type -> Name
chordNth scale = Name . Scale.fromDegree scale
