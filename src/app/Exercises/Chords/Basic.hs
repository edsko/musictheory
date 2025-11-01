module Exercises.Chords.Basic (exercises) where

import MusicTheory
import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Voicing qualified as Voicing
import MusicTheory.Note.Octave qualified as Octave
import MusicTheory.Scale qualified as Scale

import Lilypond qualified as Ly

import Exercises.Chords qualified as Chords
import Exercises.Util.ChordInversion (ChordInversion(..))

{-------------------------------------------------------------------------------
  List of exercises
-------------------------------------------------------------------------------}

exercises :: [Ly.Section]
exercises = [
      Ly.Section{
          title = "Diatonic triads"
        , intro = mempty
        , elems = [
              Ly.SectionSub $ Ly.Section{
                  title = "Root position"
                , intro = mempty
                , elems = triads (inversions [0])
                }
            , Ly.SectionSub $ Ly.Section{
                  title = "Inversions"
                , intro = mempty
                , elems = triads (inversions [0..2])
                }
            ]
        }
    , Ly.Section{
          title = "Diatonic seventh chords"
        , intro = mempty
        , elems = [
              Ly.SectionSub $ Ly.Section{
                  title = "Root position"
                , intro = mempty
                , elems = sevenths (inversions [0])
                }
            , Ly.SectionSub $ Ly.Section{
                  title = "Inversions"
                , intro = mempty
                , elems = sevenths (inversions [0..3])
                }
            ]
        }
    ]

triads :: [ChordInversion] -> [Ly.SectionElem]
triads invs = concat [
      Chords.exercise
        Scale.Major
        (mkSetup    "Major"          invs)
        (mkExercise Chord.MajorTriad invs)
    , Chords.exercise
        Scale.Minor
        (mkSetup    "Minor"          invs)
        (mkExercise Chord.MinorTriad invs)
    , Chords.exercise
        Scale.Minor
        (mkSetup    "Diminished / m(♭5)"  invs)
        (mkExercise Chord.DiminishedTriad invs)
    ]

sevenths :: [ChordInversion] -> [Ly.SectionElem]
sevenths invs = concat [
      Chords.exercise
        Scale.Major
        (mkSetup    "Major seventh" invs)
        (mkExercise Chord.Major7    invs)
    , Chords.exercise
        Scale.Major
        (mkSetup    "Dominant seventh" invs)
        (mkExercise Chord.Dominant7    invs)
    , [Ly.SectionPageBreak]
    , Chords.exercise
        Scale.Minor
        (mkSetup    "Minor seventh" invs)
        (mkExercise Chord.Minor7    invs)
    , Chords.exercise
        Scale.Minor
        (mkSetup    "Half-diminished / m7(♭5)" invs)
        (mkExercise Chord.HalfDiminished       invs)
    ]

inversions :: [Word] -> [ChordInversion]
inversions is = [
      ChordInversion (Inversion i) noOctaveShift Ly.NoAnnotation
    | i <- is
    ]

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

mkSetup :: String -> [ChordInversion] -> Chords.Setup
mkSetup title invs = Chords.Setup{
      title
    , intro          = mempty
    , clef           = Ly.ClefTreble
    , numInversions  = length invs
    }

mkExercise :: Chord.Type -> [ChordInversion] -> Chords.Exercise
mkExercise chordType invs = Chords.Exercise{
      voicing        = Voicing.Default
    , startingOctave = Octave.middle
    , simplifyNotes  = False
    , adjustOctave   = \_ -> Just noOctaveShift
    , inversionsFor  = \_ -> invs
    , chordType
    }