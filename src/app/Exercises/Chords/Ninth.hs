module Exercises.Chords.Ninth (exercises) where

import MusicTheory.Chord qualified as Chord

import Lilypond qualified as Ly

import Exercises.Chords

{-------------------------------------------------------------------------------
  List of exercises
-------------------------------------------------------------------------------}

exercises :: Ly.Section
exercises = Ly.Section{
      title  = "Standard rootless ninth voicings"
    , intro  = Just "Seventh or third in the bass."
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
          Chord.StdNinth_Major
          [(Chord.Inversion 3, (-1)), (Chord.Inversion 1, 0)]
    }

minor :: Ly.Score
minor = Ly.Score{
      title = "Minor seventh"
    , elems =
        chordExercise
          Chord.StdNinth_Minor
          [(Chord.Inversion 3, (-1)), (Chord.Inversion 1, 0)]
    }

dominant :: Ly.Score
dominant = Ly.Score{
      title = "Dominant seventh"
    , elems =
        chordExercise
          Chord.StdNinth_Dominant
          [(Chord.Inversion 3, (-1)), (Chord.Inversion 1, 0)]
    }

halfDiminished :: Ly.Score
halfDiminished = Ly.Score{
      title = "Half-diminished"
    , elems =
        chordExercise
          Chord.StdNinth_HalfDiminished
          [(Chord.Inversion 3, (-1)), (Chord.Inversion 1, 0)]
    }

altered :: Ly.Score
altered = Ly.Score{
      title = "Altered"
    , elems =
        chordExercise
          Chord.StdNinth_Altered
          [(Chord.Inversion 3, (-1)), (Chord.Inversion 1, 0)]
    }
