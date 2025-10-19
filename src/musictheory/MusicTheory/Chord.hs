-- | Chords
--
-- Intended for qualified import.
--
-- > import MusicTheory.Chord (Chord(Chord))
-- > import MusicTheory.Chord qualified as Chord
module MusicTheory.Chord (
    -- * Basic definitions
    Chord(..)
  , fromOctave
    -- * Chord types
  , Type(..)
  , wrtMajorScale
  , majorScaleDegrees
  ) where

import MusicTheory.Note (Note)
import MusicTheory.Note qualified as Note
import MusicTheory.Scale qualified as Scale

{-------------------------------------------------------------------------------
  Basic definition
-------------------------------------------------------------------------------}

data Chord = Chord [Note.InOctave]
  deriving stock (Show)

-- | Helper function for constructing a chord, given octave for first note
fromOctave :: Note.Octave -> [Note] -> Chord
fromOctave = \o ns -> Chord $ go o ns
  where
    go :: Note.Octave -> [Note] -> [Note.InOctave]
    go _ []        = []
    go o [n]       = [Note.InOctave n o]
    go o (n:n':ns) = Note.InOctave n o
                   : if Note.normalize n <= Note.normalize n'
                       then go       o  (n':ns)
                       else go (succ o) (n':ns)

{-------------------------------------------------------------------------------
  Chord types
-------------------------------------------------------------------------------}

data Type =
    TriadMajor
  | TriadMinor

-- | Scale degrees for chord, wrt to the corresponding /major/ scale
--
-- > majorScaleDegrees TriadMajor == ["I","III" ,"V"]
-- > majorScaleDegrees TriadMinor == ["I","III♭","V"]
majorScaleDegrees :: Type -> [Scale.Degree]
majorScaleDegrees = \case
    TriadMajor -> ["I", "III" , "V"]
    TriadMinor -> ["I", "III♭", "V"]

-- | Construct chord, using note names wrt the corresponding /major/ scale
--
-- > wrtMajorScale TriadMajor "C" == Chord ["C4","E4","G4"]
-- > wrtMajorScale TriadMinor "C" == Chord ["C4","E♭4","G4"]
-- > wrtMajorScale TriadMajor "A" == Chord ["A4","C♯5","E5"]
-- > wrtMajorScale TriadMinor "A" == Chord ["A4","C♮5","E5"]
wrtMajorScale :: Type -> Scale.Name -> Chord
wrtMajorScale typ scale = fromOctave Note.middleOctave $
    map fromDegree $ majorScaleDegrees typ
  where
    fromDegree :: Scale.Degree -> Note
    fromDegree = Scale.wrtScale (Scale.majorScale scale) . Scale.fromDegree
