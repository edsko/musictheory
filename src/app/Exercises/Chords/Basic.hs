module Exercises.Chords.Basic (exercises) where

import Data.Default

import MusicTheory.Chord qualified as Chord
import MusicTheory.Note  qualified as Note
import MusicTheory.Scale qualified as Scale

import Lilypond qualified as Ly

{-------------------------------------------------------------------------------
  List of exercises
-------------------------------------------------------------------------------}

exercises :: Ly.Section
exercises = Ly.Section{
      title  = "Basic chord exercises"
    , scores = [
          majorTriads
        , majorSeventh
        , dominantSeventh
        , minorSeventh
        ]
    }

{-------------------------------------------------------------------------------
  Individual exercises
-------------------------------------------------------------------------------}

majorTriads :: Ly.Score
majorTriads = Ly.Score{
      title = "Major triads, root position"
    , elems =
        allChordsOfType
          Note.middleOctave
          Chord.MajorTriad
          Chord.rootPosition
    }

majorSeventh :: Ly.Score
majorSeventh = Ly.Score{
      title = "Major seventh chords, root position"
    , elems =
        allChordsOfType
          Note.middleOctave
          Chord.MajorSeventh
          Chord.rootPosition
    }

dominantSeventh :: Ly.Score
dominantSeventh = Ly.Score{
      title = "Dominant seventh chords, seventh in the bass"
    , elems =
        allChordsOfType
          (pred Note.middleOctave) -- Start lower, to make room for inversion
          Chord.DominantSeventh
          (Chord.Inversion 3)
    }

minorSeventh :: Ly.Score
minorSeventh = Ly.Score{
      title = "Minor seventh chords, seventh in the bass"
    , elems =
        allChordsOfType
          (pred Note.middleOctave) -- Start lower, to make room for inversion
          Chord.MinorSeventh
          (Chord.Inversion 3)
    }

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

allChordsOfType :: Note.Octave -> Chord.Type -> Chord.Inversion -> Ly.ScoreElem
allChordsOfType octave typ inversion = Ly.ScoreStaff props $ mconcat [
      chordsOfTypeIn octave typ inversion firstHalf
    , [Ly.StaffLinebreak]
    , chordsOfTypeIn octave typ inversion secondHalf
    ]
  where
    props :: Ly.StaffProps
    props = def{
          Ly.hideTimeSignature  = True
        , Ly.omitMeasureNumbers = True
        }

chordsOfTypeIn ::
     Note.Octave      -- ^ Octave to start the chord in
  -> Chord.Type       -- ^ Chord type
  -> Chord.Inversion  -- ^ Chord inversion
  -> [Scale.Name]     -- ^ Scales
  -> [Ly.StaffElem]
chordsOfTypeIn octave typ inversion scales = [
      Ly.StaffChord $ Ly.Chord{
          name     = Just $ Chord.Name (Scale.rootNote scale) typ
        , notes    = Chord.invert inversion $
                       Chord.wrtMajorScale octave scale typ
        , duration = Ly.Whole
        }
    | scale <- scales
    ]

-- | Split the scales into two halves
--
-- We repeat "C" at the end for nice symmetry.
firstHalf, secondHalf :: [Scale.Name]
firstHalf  = [ "C"  , "G"  , "D"  , "A"  , "E"  , "B" , "F♯" ]
secondHalf = [ "G♭" , "D♭" , "A♭" , "E♭" , "B♭" , "F" , "C"  ]
