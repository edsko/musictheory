module Exercises.Chords.StdJazz (exercises) where

import Data.List.NonEmpty (NonEmpty)

import MusicTheory
import MusicTheory.Chord qualified as Chord
import MusicTheory.Scale qualified as Scale

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
      title = sectionTitle "Standard Jazz voicings"
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

major :: Ly.Score Style.Class
major = Ly.Score{
      title = exerciseTitle "Major seventh"
    , intro = Just $ Markup.Wordwrap $ mconcat [
          "Voiced using"
        , voicing Chord.StdJazz_Major
        , "."
        ]
    , staff =
        chordExercise
          Chord.StdJazz_Major
          [(Inversion 0, noOctaveShift), (Inversion 2, OctaveShift (-1))]
    }

minor :: Ly.Score Style.Class
minor = Ly.Score{
      title = exerciseTitle "Minor seventh"
    , intro = Just $ Markup.Wordwrap $ mconcat [
          "Voiced using"
        , voicing Chord.StdJazz_Minor
        , "."
        ]
    , staff =
        chordExercise
          Chord.StdJazz_Minor
          [(Inversion 0, noOctaveShift), (Inversion 2, OctaveShift (-1))]
    }

dominant :: Ly.Score Style.Class
dominant = Ly.Score{
      title = exerciseTitle "Dominant seventh"
    , intro = Just $ Markup.Wordwrap $ mconcat [
          "Voiced using"
        , voicing Chord.StdJazz_Dominant
        , "."
        ]
    , staff =
        chordExercise
          Chord.StdJazz_Dominant
          [(Inversion 0, noOctaveShift), (Inversion 2, OctaveShift (-1))]
    }

halfDiminished :: Ly.Score Style.Class
halfDiminished = Ly.Score{
      title = exerciseTitle "Half-diminished"
    , intro = Just $ Markup.Wordwrap $ mconcat [
          "Voiced using"
        , voicing Chord.StdJazz_HalfDiminished
        , "."
        ]
    , staff =
        chordExercise
          Chord.StdJazz_HalfDiminished
          [(Inversion 1, noOctaveShift), (Inversion 3, OctaveShift (-1))]
    }

altered :: Ly.Score Style.Class
altered = Ly.Score{
      title = exerciseTitle "Altered"
    , intro = Just $ Markup.Wordwrap $ mconcat [
          "Voiced using"
        , voicing Chord.StdJazz_Altered
        , "."
        ]
    , staff =
        chordExercise
          Chord.StdJazz_Altered
          [(Inversion 0, noOctaveShift), (Inversion 2, OctaveShift (-1))]
    }

sus :: Ly.Score Style.Class
sus = Ly.Score{
      title = exerciseTitle "Suspended"
    , intro = Just $ Markup.Wordwrap $ mconcat [
          "Voiced using"
        , voicing Chord.StdJazz_Sus
        ,  "(or equivalently using a maj7 chord voiced using"
        , voicing Chord.Basic_MajorSeventh
        , " a whole step down)"
        , "."
        ]
    , staff =
        chordExercise
          Chord.StdJazz_Sus
          [(Inversion 1, noOctaveShift), (Inversion 3, OctaveShift (-1))]
    }

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

voicing :: Chord.Type -> Markup.Markup cls
voicing chordType =
    foldMap (Markup.Music . Markup.ScaleDegree) scaleDegrees
  where
    scaleDegrees :: NonEmpty Scale.Degree
    scaleDegrees = Chord.scaleDegrees Scale.Major chordType Scale.firstDegree