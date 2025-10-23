-- | Named chords
--
-- Intended for qualified import.
--
-- > import MusicTheory.Chord.Named qualified as Named (Chord)
-- > import MusicTheory.Chord.Named qualified as Chord.Named
module MusicTheory.Chord.Named (
    Chord -- opaque
    -- * Construction
  , relative
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
import MusicTheory.Chord.Unnamed qualified as Unnamed.Chord
import MusicTheory.Reference
import MusicTheory.Scale qualified as Scale

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Named chord
data Chord r where
  -- | Relative chord
  --
  -- Until we pick a scale, we cannot know which notes are in the chord,
  Rel :: Chord.Name Relative -> Chord Relative

  -- | Absolute
  Abs :: Chord.Name Absolute -> Unnamed.Chord Absolute -> Chord Absolute

deriving instance IsReferenceKind r => Show (Chord r)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

relative :: Chord.Name Relative -> Chord Relative
relative = Rel

-- | Chord of the specified type at the root of the scale
chordI :: Chord.Type -> Chord Relative
chordI typ = relative $ Chord.Name Scale.firstDegree typ

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

getName :: Chord r -> Chord.Name r
getName (Rel name)   = name
getName (Abs name _) = name

getType :: Chord r -> Chord.Type
getType = (.typ) . getName

getNotes :: Chord Absolute -> Unnamed.Chord Absolute
getNotes (Abs _ chord) = chord

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance Invert (Chord Absolute) where
  invert = mapNotes . invert

instance TransposeOctave (Chord Absolute) where
  transposeOctave = mapNotes . transposeOctave

instance Distance (Chord Absolute) where
  distance = distance `on` getNotes

instance MakeAbsolute Chord where
  wrtScale octave scale (Rel name) =
      Abs name' $ wrtScale octave scale notes
    where
      name' :: Chord.Name Absolute
      name' = wrtScale octave scale name

      notes :: Unnamed.Chord Relative
      notes = Unnamed.Chord.fromScaleDegrees $
          Chord.scaleDegrees scale.name.typ name.typ name.root

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Apply function to the notes of the chord, leaving the name unchanged
mapNotes ::
     (Unnamed.Chord Absolute -> Unnamed.Chord Absolute)
  -> Chord Absolute -> Chord Absolute
mapNotes f (Abs name chord) = Abs name (f chord)
