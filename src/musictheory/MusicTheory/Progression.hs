-- | Chord progressions
--
-- Intended for qualified import.
--
-- > import MusicTheory.Progression (Progression(Progression))
-- > import MusicTheory.Progression qualified as Progression
module MusicTheory.Progression (
    Progression(..)
    -- * Standard progressions
  , Type(..)
  , wrtMajorScale
    -- * Voice leading
  , voiceLeading
  ) where

import Data.Bifunctor

import MusicTheory
import MusicTheory.Chord (Chord)
import MusicTheory.Chord qualified as Chord
import MusicTheory.Note qualified as Note
import MusicTheory.Scale qualified as Scale
import MusicTheory.Util

{-------------------------------------------------------------------------------
  Basic definitions
-------------------------------------------------------------------------------}

newtype Progression = Progression [(Chord.Type, Chord)]
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Standard progressions

  TODO: There is duplicated logic between here and "MusicProgression.Chord",
  but it's not trivial to avoid it, for two reasons:

  - A progression such as 2-5-1 is all with respect to a /single/ scale;
    we should not think of these as chords in different scales, with potentially
    different note spellings.
  - We cannot easily shift scale degrees, without knowing what the underlying
    scale looks like. For example, shifting degree "III" by one semitone in a
    major scale results in degree "IV", but shifting degree "II" should either
    result in "II♯" or "III♭" -- and it's unclear which.

  This requires more thought, but for now my conclusion is that how we think
  of these chords as relating to notes in the context key is non-obvious.
-------------------------------------------------------------------------------}

-- | Standard progression with choice of voicing
data Type =
    Major251_Seventh

majorScaleDegrees :: Type -> [(Chord.Type, [Scale.Degree])]
majorScaleDegrees = \case
    Major251_Seventh -> [
        (Chord.MinorSeventh,    ["II", "IV", "VI", "I"])
      , (Chord.DominantSeventh, ["V", "VII", "II", "IV"])
      , (Chord.MajorSeventh,    ["I", "III", "V", "VII"])
      ]

wrtMajorScale :: Note.Octave -> Scale.Name -> Type -> Progression
wrtMajorScale octave scale =
      Progression
    . map (second $ Chord.fromScaleDegrees octave scale)
    . majorScaleDegrees

{-------------------------------------------------------------------------------
  Voice leading
-------------------------------------------------------------------------------}

-- | Choose inversions to minimize distance between successive chords
--
-- Fails if there is no unique solution.
voiceLeading :: Progression -> Progression
voiceLeading = \(Progression chords) -> Progression $
    case chords of
      []         -> []
      (typ, c):cs -> (typ, c) : go c cs
  where
    go :: Chord -> [(Chord.Type, Chord)] -> [(Chord.Type, Chord)]
    go _    []               = []
    go prev ((typ, next):cs) =
        let next' = minimize (Chord.distance prev) allOptions
         in (typ, next') : go next' cs
      where
        numNotes :: Word
        numNotes = Chord.size next

        possibleInversions :: [Chord]
        possibleInversions = [
              Chord.invert (Chord.Inversion n) next
            | n <- [0 .. numNotes - 1]
            ]

        -- We consider all inversions in their \"natural\" octave, as well as
        -- one octave longer (because inversion tends to move everything up).
        allOptions :: [Chord]
        allOptions = concatMap addOctaveDown possibleInversions
          where
            addOctaveDown :: Chord -> [Chord]
            addOctaveDown c = [c, transposeOctave (-1) c]
