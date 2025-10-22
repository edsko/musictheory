module Exercises.Chords.StdJazz (exercises) where

import MusicTheory
import MusicTheory.Chord qualified as Chord

import Lilypond qualified as Ly

import Exercises.Chords

{-------------------------------------------------------------------------------
  List of exercises
-------------------------------------------------------------------------------}

exercises :: Ly.Section
exercises = Ly.Section{
      title  = "Standard Jazz voicings"
    , intro  = Just "All chords shown with the seventh and the third in the bass."
    , scores = [
          major
        , minor
        , dominant
        , halfDiminished
        , altered
        ]
    }

{-------------------------------------------------------------------------------
  Individual exercises
-------------------------------------------------------------------------------}

major :: Ly.Score
major = Ly.Score{
      title = "Major seventh"
    , elems =
        chordExercise
          Chord.StdJazz_Major
          [(Inversion 3, OctaveShift (-1)), (Inversion 1, noOctaveShift)]
    }

minor :: Ly.Score
minor = Ly.Score{
      title = "Minor seventh"
    , elems =
        chordExercise
          Chord.StdJazz_Minor
          [(Inversion 3, OctaveShift (-1)), (Inversion 1, noOctaveShift)]
    }

dominant :: Ly.Score
dominant = Ly.Score{
      title = "Dominant seventh"
    , elems =
        chordExercise
          Chord.StdJazz_Dominant
          [(Inversion 3, OctaveShift (-1)), (Inversion 1, noOctaveShift)]
    }

halfDiminished :: Ly.Score
halfDiminished = Ly.Score{
      title = "Half-diminished"
    , elems =
        chordExercise
          Chord.StdJazz_HalfDiminished
          [(Inversion 3, OctaveShift (-1)), (Inversion 1, noOctaveShift)]
    }

altered :: Ly.Score
altered = Ly.Score{
      title = "Altered"
    , elems =
        chordExercise
          Chord.StdJazz_Altered
          [(Inversion 3, OctaveShift (-1)), (Inversion 1, noOctaveShift)]
    }
