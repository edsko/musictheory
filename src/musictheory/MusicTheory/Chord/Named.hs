-- | Named chords
--
-- Intended for qualified import.
--
-- > import MusicTheory.Chord.Named qualified as Named (Chord(..))
-- > import MusicTheory.Chord.Named qualified as Chord.Named
module MusicTheory.Chord.Named (
     Chord(..)
     -- * Construction
   , wrtMajorScale
  ) where

import MusicTheory
import MusicTheory.Chord.Type qualified as Chord (Type)
import MusicTheory.Chord.Type qualified as Chord.Type
import MusicTheory.Chord.Unnamed qualified as Chord.Unnamed
import MusicTheory.Chord.Unnamed qualified as Unnamed (Chord)
import MusicTheory.Note qualified as Note
import MusicTheory.Scale qualified as Scale
import MusicTheory.Chord.Name qualified as Chord (Name)
import MusicTheory.Chord.Name qualified as Chord.Name

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Chord = Chord{
      name  :: Chord.Name
    , notes :: Unnamed.Chord
    }
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

-- | Invert named chord
--
-- This does not affect the chord name.
instance Invert Chord where
  invert i Chord{name, notes} = Chord{
        name
      , notes = invert i notes
      }

instance TransposeOctave Chord where
  transposeOctave d Chord{name, notes} = Chord{
        name
      , notes = transposeOctave d notes
      }

instance Distance Chord where
  distance a b = distance a.notes b.notes

{-------------------------------------------------------------------------------
  Construct notes
-------------------------------------------------------------------------------}

-- | Construct chord, using note names wrt the corresponding /major/ scale
--
-- > wrtMajorScale TriadMajor "C" == Chord ["C4","E4","G4"]
-- > wrtMajorScale TriadMinor "C" == Chord ["C4","E♭4","G4"]
-- > wrtMajorScale TriadMajor "A" == Chord ["A4","C♯5","E5"]
-- > wrtMajorScale TriadMinor "A" == Chord ["A4","C♮5","E5"]
wrtMajorScale :: Note.Octave -> Scale.Name -> Chord.Type -> Chord
wrtMajorScale octave scale typ = Chord{
      name  = Chord.Name.chordI scale typ
    , notes = Chord.Unnamed.fromScaleDegrees octave scale $
                Chord.Type.wrtMajorScale typ
    }
