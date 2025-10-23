module Exercises.Chords.StdJazz (exercises) where

import MusicTheory
import MusicTheory.Chord qualified as Chord

import Lilypond qualified as Ly
import Lilypond.Markup qualified as Markup

import Exercises.Chords
import Exercises.Lilypond
import Exercises.Lilypond.Style qualified as Style

{-------------------------------------------------------------------------------
  List of exercises
-------------------------------------------------------------------------------}

exercises :: Ly.Section Style.Class
exercises = Ly.Section{
      title  = sectionTitle "Standard Jazz voicings"
    , intro  = Just $ Markup.Wordwrap $ mconcat [
          "Every chord shown twice: "
        , "first with the seventh in the bass, "
        , " then with the third in the bass. "
        ]
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

major :: Ly.Score Style.Class
major = Ly.Score{
      title = exerciseTitle "Major seventh"
    , intro = Just $ voicing ["3", "5", "7", "9"]
    , staff =
        chordExercise
          Chord.StdJazz_Major
          [(Inversion 3, OctaveShift (-1)), (Inversion 1, noOctaveShift)]
    }

minor :: Ly.Score Style.Class
minor = Ly.Score{
      title = exerciseTitle "Minor seventh"
    , intro = Just $ voicing ["♭3", "5", "♭7", "9"]
    , staff =
        chordExercise
          Chord.StdJazz_Minor
          [(Inversion 3, OctaveShift (-1)), (Inversion 1, noOctaveShift)]
    }

dominant :: Ly.Score Style.Class
dominant = Ly.Score{
      title = exerciseTitle "Dominant seventh"
    , intro = Just $ voicing ["3", "13", "♭7", "9"]
    , staff =
        chordExercise
          Chord.StdJazz_Dominant
          [(Inversion 3, OctaveShift (-1)), (Inversion 1, noOctaveShift)]
    }

halfDiminished :: Ly.Score Style.Class
halfDiminished = Ly.Score{
      title = exerciseTitle "Half-diminished"
    , intro = Just $ voicing ["1", "♭3", "♭5", "♭7"]
    , staff =
        chordExercise
          Chord.StdJazz_HalfDiminished
          [(Inversion 3, OctaveShift (-1)), (Inversion 1, noOctaveShift)]
    }

altered :: Ly.Score Style.Class
altered = Ly.Score{
      title = exerciseTitle "Altered"
    , intro = Just $ voicing ["3", "♯5", "♭7", "♯9"]
    , staff =
        chordExercise
          Chord.StdJazz_Altered
          [(Inversion 3, OctaveShift (-1)), (Inversion 1, noOctaveShift)]
    }
