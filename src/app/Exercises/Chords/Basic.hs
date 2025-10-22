module Exercises.Chords.Basic (exercises) where

import MusicTheory
import MusicTheory.Chord qualified as Chord

import Lilypond qualified as Ly

import Exercises.Chords

{-------------------------------------------------------------------------------
  List of exercises
-------------------------------------------------------------------------------}

exercises :: Ly.Section
exercises = Ly.Section{
      title  = "Basic chord exercises"
    , intro  = Nothing
    , scores = [
          majorTriads
        , majorSeventh
        , dominantSeventh
        , minorSeventh
        ]
    }

{-------------------------------------------------------------------------------
  Individual exercises

  When we use an inversion to have the seventh in the bass, we shift everything
  down an octave.
-------------------------------------------------------------------------------}

majorTriads :: Ly.Score
majorTriads = Ly.Score{
      title = "Major triads, root position"
    , elems =
        chordExercise
          Chord.Basic_MajorTriad
          [(rootPosition, noOctaveShift)]
    }

majorSeventh :: Ly.Score
majorSeventh = Ly.Score{
      title = "Major seventh chords, root position"
    , elems =
        chordExercise
          Chord.Basic_MajorSeventh
          [(rootPosition, noOctaveShift)]
    }

dominantSeventh :: Ly.Score
dominantSeventh = Ly.Score{
      title = "Dominant seventh chords, seventh in the bass"
    , elems =
        chordExercise
          Chord.Basic_DominantSeventh
          [(Inversion 3, OctaveShift (-1))]
    }

minorSeventh :: Ly.Score
minorSeventh = Ly.Score{
      title = "Minor seventh chords, seventh in the bass"
    , elems =
        chordExercise
          Chord.Basic_MinorSeventh
          [(Inversion 3, OctaveShift (-1))]
    }
