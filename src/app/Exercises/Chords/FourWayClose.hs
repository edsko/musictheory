module Exercises.Chords.FourWayClose (exercises) where

import MusicTheory
import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Voicing qualified as Voicing

import Lilypond qualified as Ly
import Lilypond.Markup qualified as Ly (Markup)
import Lilypond.Markup qualified as Ly.Markup

import Exercises.Chords

{-------------------------------------------------------------------------------
  List of exercises
-------------------------------------------------------------------------------}

exercises :: Ly.Section
exercises = Ly.Section{
      title = "Four Note Closed Hand Voicings"
    , intro = Just $ Ly.Markup.Wordwrap $ mconcat [
          "Every chord shown twice: "
        , "first with the third in the bass, "
        , " then with the seventh in the bass. "
        ]
    , elems = [
          Ly.SectionScore major
        , Ly.SectionScore minor
        , Ly.SectionScore dominant
        , Ly.SectionPageBreak

        , Ly.SectionScore halfDiminished
        , Ly.SectionScore altered
        , Ly.SectionScore sus
        ]
    }

{-------------------------------------------------------------------------------
  Individual exercises

  NOTE: The rootless chords will start on the third without any inversions,
  and on the seventh after two inversions.
-------------------------------------------------------------------------------}

major :: Ly.Score
major = Ly.Score{
      title = "Major seventh"
    , intro = Just $ Ly.Markup.Wordwrap $ mconcat [
          "Voiced using"
        , voicing Chord.Major7
        , "."
        ]
    , staff =
        chordExercise
          Chord.Major7
          Voicing.FourWayClose
          [(Inversion 0, noOctaveShift), (Inversion 2, OctaveShift (-1))]
    }

minor :: Ly.Score
minor = Ly.Score{
      title = "Minor seventh"
    , intro = Just $ Ly.Markup.Wordwrap $ mconcat [
          "Voiced using"
        , voicing Chord.Minor7
        , "."
        ]
    , staff =
        chordExercise
          Chord.Minor7
          Voicing.FourWayClose
          [(Inversion 0, noOctaveShift), (Inversion 2, OctaveShift (-1))]
    }

dominant :: Ly.Score
dominant = Ly.Score{
      title = "Dominant seventh"
    , intro = Just $ Ly.Markup.Wordwrap $ mconcat [
          "Voiced using"
        , voicing Chord.Dominant7
        , "."
        ]
    , staff =
        chordExercise
          Chord.Dominant7
          Voicing.FourWayClose
          [(Inversion 0, noOctaveShift), (Inversion 2, OctaveShift (-1))]
    }

halfDiminished :: Ly.Score
halfDiminished = Ly.Score{
      title = "Half-diminished"
    , intro = Just $ Ly.Markup.Wordwrap $ mconcat [
          "Voiced using"
        , voicing Chord.HalfDiminished
        , "."
        ]
    , staff =
        chordExercise
          Chord.HalfDiminished
          Voicing.FourWayClose
          [(Inversion 1, noOctaveShift), (Inversion 3, OctaveShift (-1))]
    }

altered :: Ly.Score
altered = Ly.Score{
      title = "Altered"
    , intro = Just $ Ly.Markup.Wordwrap $ mconcat [
          "Voiced using"
        , voicing Chord.Altered
        , "."
        ]
    , staff =
        chordExercise
          Chord.Altered
          Voicing.FourWayClose
          [(Inversion 0, noOctaveShift), (Inversion 2, OctaveShift (-1))]
    }

sus :: Ly.Score
sus = Ly.Score{
      title = "Suspended"
    , intro = Just $ Ly.Markup.Wordwrap $ mconcat [
          "Voiced using"
        , voicing Chord.Sus
        ,  "(or equivalently using a maj7 chord voiced using"
        , voicing Chord.Major7
        , " a whole step down)"
        , "."
        ]
    , staff =
        chordExercise
          Chord.Sus
          Voicing.FourWayClose
          [(Inversion 1, noOctaveShift), (Inversion 3, OctaveShift (-1))]
    }

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

voicing :: Chord.Type -> Ly.Markup
voicing chordType =
    foldMap (Ly.Markup.Music . Ly.Markup.Interval) $
      Voicing.intervals Voicing.FourWayClose chordType
