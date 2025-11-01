module Exercises.Chords.Basic (exercises) where

import MusicTheory
import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Voicing qualified as Voicing
import MusicTheory.Note.Octave qualified as Octave
import MusicTheory.Scale qualified as Scale

import Lilypond qualified as Ly

import Exercises.Chords
import Exercises.Util.ChordInversion (ChordInversion(..))

{-------------------------------------------------------------------------------
  List of exercises
-------------------------------------------------------------------------------}

exercises :: [Ly.Section]
exercises = [
      Ly.Section{
          title = "Diatonic triads"
        , intro = Nothing
        , elems = [
              Ly.SectionSub $ Ly.Section{
                  title = "Root position"
                , intro = Nothing
                , elems = triads (inversions [0])
                }
            , Ly.SectionSub $ Ly.Section{
                  title = "Inversions"
                , intro = Nothing
                , elems = triads (inversions [0..2])
                }
            ]
        }
    , Ly.Section{
          title = "Diatonic seventh chords"
        , intro = Nothing
        , elems = [
              Ly.SectionSub $ Ly.Section{
                  title = "Root position"
                , intro = Nothing
                , elems = sevenths (inversions [0])
                }
            , Ly.SectionSub $ Ly.Section{
                  title = "Inversions"
                , intro = Nothing
                , elems = sevenths (inversions [0..3])
                }
            ]
        }
    ]

triads :: [ChordInversion] -> [Ly.SectionElem]
triads invs = concat [
      chordExercise Scale.Major $
        mkExercise "Major" Chord.MajorTriad invs
    , chordExercise Scale.Minor $
        mkExercise "Minor" Chord.MinorTriad invs
    , chordExercise Scale.Minor $
        mkExercise "Diminished / m(♭5)" Chord.DiminishedTriad invs
    ]

sevenths :: [ChordInversion] -> [Ly.SectionElem]
sevenths invs = concat [
      chordExercise Scale.Major $
        mkExercise "Major seventh" Chord.Major7 invs
    , chordExercise Scale.Major $
        mkExercise "Dominant seventh" Chord.Dominant7 invs
    , [Ly.SectionPageBreak]
    , chordExercise Scale.Minor $
        mkExercise "Minor seventh" Chord.Minor7 invs
    , chordExercise Scale.Minor $
        mkExercise "Half-diminished / m7(♭5)" Chord.HalfDiminished invs
    ]

inversions :: [Word] -> [ChordInversion]
inversions is = [
      ChordInversion (Inversion i) noOctaveShift Ly.NoAnnotation
    | i <- is
    ]

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

mkExercise :: String -> Chord.Type -> [ChordInversion] -> ChordExercise
mkExercise title chordType invs = ChordExercise{
      title
    , intro          = Nothing
    , clef           = Ly.ClefTreble
    , voicing        = Voicing.Default
    , startingOctave = Octave.middle
    , adjustOctave   = \_ -> Just noOctaveShift
    , numInversions  = length invs
    , inversionsFor  = \_ -> invs
    , chordType
    }