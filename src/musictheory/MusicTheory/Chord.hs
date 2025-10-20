-- | Chords
--
-- Intended for qualified import.
--
-- > import MusicTheory.Chord (Chord(Chord))
-- > import MusicTheory.Chord qualified as Chord
module MusicTheory.Chord (
    -- * Basic definitions
    Chord(..)
  , size
    -- * Construction
  , fromOctave
  , fromScaleDegrees
    -- * Chord types
  , Type(..)
  , wrtMajorScale
    -- * Inversions
  , Inversion(..)
  , rootPosition
  , invert
    -- * Chord names
  , Name(..)
    -- * Distance
  , distance
  ) where

import MusicTheory
import MusicTheory.Note (Note)
import MusicTheory.Note qualified as Note
import MusicTheory.Scale qualified as Scale

{-------------------------------------------------------------------------------
  Basic definition
-------------------------------------------------------------------------------}

data Chord = Chord [Note.InOctave]
  deriving stock (Show)

instance TransposeOctave Chord where
  transposeOctave d (Chord ns) = Chord $ map (transposeOctave d) ns

-- | Number of notes in the chord
size :: Chord -> Word
size (Chord ns) = fromIntegral $ length ns

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Helper function for constructing a chord, given octave for first note
fromOctave :: Note.Octave -> [Note] -> Chord
fromOctave = \o ns -> Chord $ go o ns
  where
    go :: Note.Octave -> [Note] -> [Note.InOctave]
    go _ []        = []
    go o [n]       = [Note.InOctave n o]
    go o (n:n':ns) = Note.InOctave n o
                   : if nextOctave n n' then go (succ o) (n':ns)
                                        else go       o  (n':ns)

-- | Should we jump to the next octave?
--
-- This is a bit subtle. Consider
--
-- > G B D
--
-- In this case, we want to jump to the next octave when we see the D.
-- Intuitively, this is because D is a \"lower\" note than B, and so it should
-- be placed in the octave up. However, we should not just call 'normalize' on
-- the notes to see which note is \"lower\"; consider:
--
-- > D♭ F A♭ C♭
--
-- Here we want to jump to the next octave on the C♭, even though C♭ is not a
-- \"lower\" note than A♭. Indeed, if this was
--
-- > D♭ F A♭ B
--
-- we would /not/ want to jump to the next octave, even though C♭ and B are the
-- same note: the spelling matters, and we should ignore accidentals.
nextOctave :: Note -> Note -> Bool
nextOctave n n' =
      Note.normalize (Note.noteName n )
    > Note.normalize (Note.noteName n')

fromScaleDegrees :: Note.Octave -> Scale.Name -> [Scale.Degree] -> Chord
fromScaleDegrees octave scale =
    fromOctave octave . map fromDegree
  where
    fromDegree :: Scale.Degree -> Note
    fromDegree = Scale.wrtScale (Scale.majorScale scale) . Scale.fromDegree

{-------------------------------------------------------------------------------
  Chord types
-------------------------------------------------------------------------------}

data Type =
    MajorTriad
  | MinorTriad
  | MajorSeventh
  | MinorSeventh
  | DominantSeventh
  deriving stock (Show)

-- | Scale degrees for chord, wrt to the corresponding /major/ scale
--
-- > majorScaleDegrees TriadMajor == ["I","III" ,"V"]
-- > majorScaleDegrees TriadMinor == ["I","III♭","V"]
majorScaleDegrees :: Type -> [Scale.Degree]
majorScaleDegrees = \case
    MajorTriad      -> ["I", "III" , "V"]
    MinorTriad      -> ["I", "III♭", "V"]
    MajorSeventh    -> ["I", "III" , "V", "VII"]
    MinorSeventh    -> ["I", "III♭", "V", "VII♭"]
    DominantSeventh -> ["I", "III" , "V", "VII♭"]

-- | Construct chord, using note names wrt the corresponding /major/ scale
--
-- > wrtMajorScale TriadMajor "C" == Chord ["C4","E4","G4"]
-- > wrtMajorScale TriadMinor "C" == Chord ["C4","E♭4","G4"]
-- > wrtMajorScale TriadMajor "A" == Chord ["A4","C♯5","E5"]
-- > wrtMajorScale TriadMinor "A" == Chord ["A4","C♮5","E5"]
wrtMajorScale :: Note.Octave -> Scale.Name -> Type -> Chord
wrtMajorScale octave scale = fromScaleDegrees octave scale . majorScaleDegrees

{-------------------------------------------------------------------------------
  Inversions
-------------------------------------------------------------------------------}

newtype Inversion = Inversion Word

rootPosition :: Inversion
rootPosition = Inversion 0

invert :: Inversion -> Chord -> Chord
invert = \(Inversion i) (Chord ns) -> Chord $ go [] i ns
  where
    go :: [Note.InOctave] -> Word -> [Note.InOctave] -> [Note.InOctave]
    go acc _ []     = reverse acc
    go acc 0 ns     = ns ++ reverse acc
    go acc i (n:ns) = go (transposeOctave 1 n : acc) (i - 1) ns

{-------------------------------------------------------------------------------
  Chord names
-------------------------------------------------------------------------------}

data Name = Name Note.Simple Type

{-------------------------------------------------------------------------------
  Distance
-------------------------------------------------------------------------------}

-- | Distance between two chords
--
-- For now this is a pretty simplistic definition: we merely compute the
-- distance in semitones between corresponding pairs of notes.
distance :: Chord -> Chord -> Word
distance (Chord as) (Chord bs) = sum $ zipWith Note.distance as bs