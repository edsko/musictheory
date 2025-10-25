-- | Named chords
--
-- Intended for qualified import.
--
-- > import MusicTheory.Chord.Named qualified as Named (Chord)
-- > import MusicTheory.Chord.Named qualified as Chord.Named
module MusicTheory.Chord.Named (
    Chord(..)
    -- * Construction
  , chordI
    -- * Query
  , getName
  , getType
  , getNotes
  ) where

import Data.Function

import MusicTheory
import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Unnamed qualified as Unnamed (Chord)
import MusicTheory.Reference qualified as Reference
import MusicTheory.Scale qualified as Scale

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Named chord
data Chord r where
  -- | Relative chord
  --
  -- Until we pick a scale, we cannot know which notes are in the chord,
  Rel ::
       Chord.Name Reference.Rel
    -> Chord Reference.Rel

  -- | Absolute
  Abs ::
       Chord.Name Reference.Abs
    -> Unnamed.Chord Reference.Abs
    -> Chord Reference.Abs

deriving instance Reference.IsReferenceKind r => Show (Chord r)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Chord of the specified type at the root of the scale
chordI :: Chord.Type -> Chord Reference.Rel
chordI typ = Rel $ Chord.Name Scale.firstDegree typ

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

getName :: Chord r -> Chord.Name r
getName (Rel name)   = name
getName (Abs name _) = name

getType :: Chord r -> Chord.Type
getType = (.typ) . getName

getNotes :: Chord Reference.Abs -> Unnamed.Chord Reference.Abs
getNotes (Abs _ chord) = chord

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance Invert (Chord Reference.Abs) where
  invert = mapNotes . invert

instance TransposeOctave (Chord Reference.Abs) where
  transposeOctave = mapNotes . transposeOctave

instance Distance (Chord Reference.Abs) where
  distance = distance `on` getNotes

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Apply function to the notes of the chord, leaving the name unchanged
mapNotes ::
     (Unnamed.Chord Reference.Abs -> Unnamed.Chord Reference.Abs)
  -> Chord Reference.Abs -> Chord Reference.Abs
mapNotes f (Abs name chord) = Abs name (f chord)
