module Exercises.Chords.StdJazz (exercises) where

import MusicTheory
import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Voicing qualified as Voicing

import Lilypond qualified as Ly
import Lilypond.Markup qualified as Markup

import Exercises.Chords

{-------------------------------------------------------------------------------
  List of exercises
-------------------------------------------------------------------------------}

exercises :: Ly.Section
exercises = Ly.Section{
      title = "Standard Jazz voicings"
    , intro = Just $ Markup.Wordwrap $ mconcat [
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
    , intro = Just $ Markup.Wordwrap $ mconcat [
          "Voiced using"
        , voicing Chord.Major7
        , "."
        ]
    , staff =
        chordExercise
          Chord.Major7
          Voicing.StdJazz
          [(Inversion 0, noOctaveShift), (Inversion 2, OctaveShift (-1))]
    }

minor :: Ly.Score
minor = Ly.Score{
      title = "Minor seventh"
    , intro = Just $ Markup.Wordwrap $ mconcat [
          "Voiced using"
        , voicing Chord.Minor7
        , "."
        ]
    , staff =
        chordExercise
          Chord.Minor7
          Voicing.StdJazz
          [(Inversion 0, noOctaveShift), (Inversion 2, OctaveShift (-1))]
    }

dominant :: Ly.Score
dominant = Ly.Score{
      title = "Dominant seventh"
    , intro = Just $ Markup.Wordwrap $ mconcat [
          "Voiced using"
        , voicing Chord.Dominant7
        , "."
        ]
    , staff =
        chordExercise
          Chord.Dominant7
          Voicing.StdJazz
          [(Inversion 0, noOctaveShift), (Inversion 2, OctaveShift (-1))]
    }

halfDiminished :: Ly.Score
halfDiminished = Ly.Score{
      title = "Half-diminished"
    , intro = Just $ Markup.Wordwrap $ mconcat [
          "Voiced using"
        , voicing Chord.HalfDiminished
        , "."
        ]
    , staff =
        chordExercise
          Chord.HalfDiminished
          Voicing.StdJazz
          [(Inversion 1, noOctaveShift), (Inversion 3, OctaveShift (-1))]
    }

altered :: Ly.Score
altered = Ly.Score{
      title = "Altered"
    , intro = Just $ Markup.Wordwrap $ mconcat [
          "Voiced using"
        , voicing Chord.Altered
        , "."
        ]
    , staff =
        chordExercise
          Chord.Altered
          Voicing.StdJazz
          [(Inversion 0, noOctaveShift), (Inversion 2, OctaveShift (-1))]
    }

sus :: Ly.Score
sus = Ly.Score{
      title = "Suspended"
    , intro = Just $ Markup.Wordwrap $ mconcat [
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
          Voicing.StdJazz
          [(Inversion 1, noOctaveShift), (Inversion 3, OctaveShift (-1))]
    }

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

voicing :: Chord.Type -> Markup.Markup
voicing chordType =
    foldMap (Markup.Music . Markup.Interval) $
      Voicing.intervals Voicing.StdJazz chordType
