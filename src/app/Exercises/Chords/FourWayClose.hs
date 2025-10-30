module Exercises.Chords.FourWayClose (exercises) where

import Data.Foldable

import MusicTheory
import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Voicing qualified as Voicing
import MusicTheory.Scale qualified as Scale

import Lilypond qualified as Ly
import Lilypond.Markup qualified as Ly (Markup)
import Lilypond.Markup qualified as Ly.Markup

import Exercises.Chords
import Exercises.Util.TypeAB (TypeAB(..))
import Exercises.Util.TypeAB qualified as TypeAB

{-------------------------------------------------------------------------------
  List of exercises
-------------------------------------------------------------------------------}

exercises :: Ly.Section
exercises = Ly.Section{
      title = "Four Note Closed Hand Voicings"
    , intro = Just $ Ly.Markup.Wordwrap $ mconcat [
          "Every is chord shown twice: "
        , "first with the third at the bottom (type A), "
        , "then with the seventh at the bottom (type B)."
        ]
    , elems = [
          Ly.SectionScore major
        , Ly.SectionScore minor
        , Ly.SectionScore dominant
        , Ly.SectionPageBreak

        , Ly.SectionScore halfDiminished
        , Ly.SectionScore altered
        , Ly.SectionScore sus
        , Ly.SectionPageBreak

        , Ly.SectionScore sevenFlat9
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
    , staff = chordExercise Chord.Major7 Voicing.FourWayClose 2 inversions
    }
  where
    inversions :: Scale.Root -> [ChordInversion]
    inversions scaleRoot = toList $
        TypeAB.markInversion (\_ -> scaleRoot == Scale.C) TypeAB{
            typeA = (Inversion 0, noOctaveShift)
          , typeB = (Inversion 2, OctaveShift (-1))
          }

minor :: Ly.Score
minor = Ly.Score{
      title = "Minor seventh"
    , intro = Just $ Ly.Markup.Wordwrap $ mconcat [
          "Voiced using"
        , voicing Chord.Minor7
        , "."
        ]
    , staff = chordExercise Chord.Minor7 Voicing.FourWayClose 2 inversions
    }
  where
    inversions :: Scale.Root -> [ChordInversion]
    inversions scaleRoot = toList $
        TypeAB.markInversion (\_ -> scaleRoot == Scale.C) TypeAB{
            typeA = (Inversion 0, noOctaveShift)
          , typeB = (Inversion 2, OctaveShift (-1))
          }

dominant :: Ly.Score
dominant = Ly.Score{
      title = "Dominant seventh"
    , intro = Just $ Ly.Markup.Wordwrap $ mconcat [
          "Voiced using"
        , voicing Chord.Dominant7
        , "."
        ]
    , staff = chordExercise Chord.Dominant7 Voicing.FourWayClose 2 inversions
    }
  where
    inversions :: Scale.Root -> [ChordInversion]
    inversions scaleRoot = toList $
        TypeAB.markInversion (\_ -> scaleRoot == Scale.C) TypeAB{
            typeA = (Inversion 0, noOctaveShift)
          , typeB = (Inversion 2, OctaveShift (-1))
          }

halfDiminished :: Ly.Score
halfDiminished = Ly.Score{
      title = "Half-diminished / m⁷(♭5)"
    , intro = Just $ Ly.Markup.Wordwrap $ mconcat [
          "Voiced using"
        , voicing Chord.HalfDiminished
        , "."
        ]
    , staff = chordExercise Chord.HalfDiminished Voicing.FourWayClose 2 inversions
    }
  where
    -- These are not rootless!
    inversions :: Scale.Root -> [ChordInversion]
    inversions scaleRoot = toList $
        TypeAB.markInversion (\_ -> scaleRoot == Scale.C) TypeAB{
          typeA = (Inversion 1, noOctaveShift)
        , typeB = (Inversion 3, OctaveShift (-1))
        }

altered :: Ly.Score
altered = Ly.Score{
      title = "Altered"
    , intro = Just $ Ly.Markup.Wordwrap $ mconcat [
          "Voiced using"
        , voicing Chord.Altered
        , "."
        ]
    , staff = chordExercise Chord.Altered Voicing.FourWayClose 2 inversions
    }
  where
    inversions :: Scale.Root -> [ChordInversion]
    inversions scaleRoot = toList $
        TypeAB.markInversion (\_ -> scaleRoot == Scale.C) TypeAB{
            typeA = (Inversion 0, noOctaveShift)
          , typeB = (Inversion 2, OctaveShift (-1))
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
    , staff = chordExercise Chord.Sus Voicing.FourWayClose 2 inversions
    }
  where
    -- TODO: It's not clear if we want the 2nd of the 4th at the bottom.
    -- The seventh is the /fourth/ note in this voicing, so we need the 3rd inv.
    inversions :: Scale.Root -> [ChordInversion]
    inversions _ = [
        ChordInversion (Inversion 1) noOctaveShift      Ly.NoAnnotation
      , ChordInversion (Inversion 3) (OctaveShift (-1)) Ly.NoAnnotation
      ]

sevenFlat9 :: Ly.Score
sevenFlat9 = Ly.Score{
      title = "7(♭9)"
    , intro = Just $ Ly.Markup.Wordwrap $ mconcat [
          "Voiced using"
        , voicing Chord.SevenFlat9
        , " (or equivalenty as a diminished chord starting at the 3)"
        , "."
        ]
    , staff = chordExercise Chord.SevenFlat9 Voicing.FourWayClose 2 inversions
    }
  where
    inversions :: Scale.Root -> [ChordInversion]
    inversions scaleRoot = toList $
        TypeAB.markInversion (\_ -> scaleRoot == Scale.C) TypeAB{
            typeA = (Inversion 0, noOctaveShift)
          , typeB = (Inversion 2, OctaveShift (-1))
          }

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

voicing :: Chord.Type -> Ly.Markup
voicing chordType =
    foldMap (Ly.Markup.Music . Ly.Markup.Interval) $
      Voicing.intervals Voicing.FourWayClose chordType
