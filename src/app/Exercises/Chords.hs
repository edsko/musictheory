module Exercises.Chords (
    majorTriads
  , majorSeventh
  , dominantSeventh
  ) where

import MusicTheory.Chord qualified as Chord
import MusicTheory.Note  qualified as Note
import MusicTheory.Scale qualified as Scale

import Lilypond qualified as Ly

{-------------------------------------------------------------------------------
  Individual exercises
-------------------------------------------------------------------------------}

majorTriads :: Ly.Score
majorTriads = Ly.Score{
      header = Ly.ScoreHeader{
          piece = "Major triads, root position"
        }
    , elems =
        allChordsOfType
          Note.middleOctave
          Chord.MajorTriad
          Chord.rootPosition
    }

majorSeventh :: Ly.Score
majorSeventh = Ly.Score{
      header = Ly.ScoreHeader{
          piece = "Major seventh chords, root position"
        }
    , elems =
        allChordsOfType
          Note.middleOctave
          Chord.MajorSeventh
          Chord.rootPosition
    }

dominantSeventh :: Ly.Score
dominantSeventh = Ly.Score{
      header = Ly.ScoreHeader{
          piece = "Dominant seventh chords, seventh in the bass"
        }
    , elems =
        allChordsOfType
          (pred Note.middleOctave) -- Start lower, to make room for inversion
          Chord.DominantSeventh
          (Chord.Inversion 3)
    }

{-------------------------------------------------------------------------------
  Internall auxiliary
-------------------------------------------------------------------------------}

allChordsOfType :: Note.Octave -> Chord.Type -> Chord.Inversion -> Ly.ScoreElem
allChordsOfType octave typ inversion = Ly.Staff $ Ly.Absolute [
      Ly.AbsoluteElem{
          chordName = Just $ Ly.ChordName (Scale.rootNote scale) typ
        , chord     = Chord.invert inversion $
                        Chord.wrtMajorScale octave typ scale
        , duration  = Ly.Whole
        }
    | scale <- Scale.scaleNames
    ]
