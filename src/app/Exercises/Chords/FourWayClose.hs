module Exercises.Chords.FourWayClose (exercises) where

import Data.Foldable

import MusicTheory
import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Named qualified as Chord.Named
import MusicTheory.Chord.Voicing qualified as Voicing
import MusicTheory.Note.Octave qualified as Octave
import MusicTheory.Scale qualified as Scale

import Lilypond qualified as Ly
import Lilypond.Markup qualified as Ly (Markup)
import Lilypond.Markup qualified as Ly.Markup

import Exercises.Chords
import Exercises.Util.ChordInversion (ChordInversion(..))
import Exercises.Util.TypeAB (TypeAB(..))
import Exercises.Util.TypeAB qualified as TypeAB

{-------------------------------------------------------------------------------
  List of exercises
-------------------------------------------------------------------------------}

exercises :: [Ly.Section]
exercises = [
      Ly.Section{
          title = "Four Note Closed Hand Voicings"
        , intro = Just $ Ly.Markup.wordwrap $ mconcat [
              "Every is chord shown twice: "
            , "first with the third at the bottom (type A), "
            , "then with the seventh at the bottom (type B)."
            ]
        , elems = exercisesForHand RightHand
        }
    , Ly.Section{
          title = "Four Note Closed Hand Voicings (Left Hand)"
        , intro = Just $ Ly.Markup.wordwrap $ mconcat [
              "Only voicings that fit between D3 and G4 are typically used. "
            , "Inversions outside this range are shown as rests."
            ]
        , elems = exercisesForHand LeftHand
        }
    ]

exercisesForHand :: Hand -> [Ly.SectionElem]
exercisesForHand hand = concat [
      major          hand
    , minor          hand
    , [Ly.SectionPageBreak]

    , dominant       hand
    , halfDiminished hand
    , [Ly.SectionPageBreak]

    , altered        hand
    , sus            hand
    , [Ly.SectionPageBreak]

    , sevenFlat9     hand
    ]

{-------------------------------------------------------------------------------
  Individual exercises

  NOTE: The rootless chords will start on the third without any inversions,
  and on the seventh after two inversions.
-------------------------------------------------------------------------------}

major :: Hand -> [Ly.SectionElem]
major hand =
    chordExercise Scale.Major $
      mkExercise hand "Major seventh" intro Chord.Major7 inversions
  where
    intro :: Ly.Markup
    intro = Ly.Markup.wordwrap $ mconcat [
          "Voiced using"
        , showVoicing Chord.Major7
        , "."
        ]

    inversions :: Scale.Root -> [ChordInversion]
    inversions scaleRoot = toList $
        TypeAB.markInversion (\_ -> scaleRoot == Scale.C) TypeAB{
            typeA = (Inversion 0, noOctaveShift)
          , typeB = (Inversion 2, OctaveShift (-1))
          }

minor :: Hand -> [Ly.SectionElem]
minor hand =
    chordExercise Scale.Minor $
      mkExercise hand "Minor seventh" intro Chord.Minor7 inversions
  where
    intro :: Ly.Markup
    intro = Ly.Markup.wordwrap $ mconcat [
          "Voiced using"
        , showVoicing Chord.Minor7
        , "."
        ]

    inversions :: Scale.Root -> [ChordInversion]
    inversions scaleRoot = toList $
        TypeAB.markInversion (\_ -> scaleRoot == Scale.C) TypeAB{
            typeA = (Inversion 0, noOctaveShift)
          , typeB = (Inversion 2, OctaveShift (-1))
          }

dominant :: Hand -> [Ly.SectionElem]
dominant hand =
    chordExercise Scale.Major $
      mkExercise hand "Dominant seventh" intro Chord.Dominant7 inversions
  where
    intro :: Ly.Markup
    intro = Ly.Markup.wordwrap $ mconcat [
          "Voiced using"
        , showVoicing Chord.Dominant7
        , "."
        ]

    inversions :: Scale.Root -> [ChordInversion]
    inversions scaleRoot = toList $
        TypeAB.markInversion (\_ -> scaleRoot == Scale.C) TypeAB{
            typeA = (Inversion 0, noOctaveShift)
          , typeB = (Inversion 2, OctaveShift (-1))
          }

halfDiminished :: Hand -> [Ly.SectionElem]
halfDiminished hand =
    chordExercise Scale.Minor $
      mkExercise hand "Half-diminished / m⁷(♭5)" intro Chord.HalfDiminished inversions
  where
    intro :: Ly.Markup
    intro = Ly.Markup.wordwrap $ mconcat [
          "Voiced using"
        , showVoicing Chord.HalfDiminished
        , "."
        ]

    -- These are not rootless!
    inversions :: Scale.Root -> [ChordInversion]
    inversions scaleRoot = toList $
        TypeAB.markInversion (\_ -> scaleRoot == Scale.C) TypeAB{
          typeA = (Inversion 1, noOctaveShift)
        , typeB = (Inversion 3, OctaveShift (-1))
        }

altered :: Hand -> [Ly.SectionElem]
altered hand =
    chordExercise Scale.Major $
      mkExercise hand  "Altered" intro Chord.Altered inversions
  where
    intro :: Ly.Markup
    intro = Ly.Markup.wordwrap $ mconcat [
          "Voiced using"
        , showVoicing Chord.Altered
        , "."
        ]

    inversions :: Scale.Root -> [ChordInversion]
    inversions scaleRoot = toList $
        TypeAB.markInversion (\_ -> scaleRoot == Scale.C) TypeAB{
            typeA = (Inversion 0, noOctaveShift)
          , typeB = (Inversion 2, OctaveShift (-1))
          }

sus :: Hand -> [Ly.SectionElem]
sus hand =
    chordExercise Scale.Major $
      mkExercise hand "Suspended" intro Chord.Sus inversions
  where
    intro :: Ly.Markup
    intro = Ly.Markup.wordwrap $ mconcat [
          "Voiced using"
        , showVoicing Chord.Sus
        ,  "(or equivalently using a maj7 chord voiced using"
        , showVoicing Chord.Major7
        , " a whole step down)"
        , "."
        ]

    -- TODO: It's not clear if we want the 2nd of the 4th at the bottom.
    -- The seventh is the /fourth/ note in this voicing, so we need the 3rd inv.
    inversions :: Scale.Root -> [ChordInversion]
    inversions _ = [
        ChordInversion (Inversion 1) noOctaveShift      Ly.NoAnnotation
      , ChordInversion (Inversion 3) (OctaveShift (-1)) Ly.NoAnnotation
      ]

sevenFlat9 :: Hand -> [Ly.SectionElem]
sevenFlat9 hand =
    chordExercise Scale.Major $
      mkExercise hand "7(♭9)" intro Chord.SevenFlat9 inversions
  where
    intro :: Ly.Markup
    intro = Ly.Markup.wordwrap $ mconcat [
          "Voiced using"
        , showVoicing Chord.SevenFlat9
        , " (or equivalenty as a diminished chord starting at the 3)"
        , "."
        ]

    inversions :: Scale.Root -> [ChordInversion]
    inversions scaleRoot = toList $
        TypeAB.markInversion (\_ -> scaleRoot == Scale.C) TypeAB{
            typeA = (Inversion 0, noOctaveShift)
          , typeB = (Inversion 2, OctaveShift (-1))
          }

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

showVoicing :: Chord.Type -> Ly.Markup
showVoicing chordType =
    foldMap (Ly.Markup.Music . Ly.Markup.Interval) $
      Voicing.intervals Voicing.FourWayClose chordType

data Hand = RightHand | LeftHand

-- TODO: Instead of 'startingOctave', we should hvae a function that /chooses/
-- an octave shift for an already formed Abs chord, with the option of failure
-- (to rule out chords outside the "playable" range).
mkExercise ::
     Hand
  -> String
  -> Ly.Markup
  -> Chord.Type
  -> (Scale.Root -> [ChordInversion])
  -> ChordExercise
mkExercise RightHand title intro chordType inversionsFor = ChordExercise{
      title
    , intro          = Just intro
    , clef           = Ly.ClefTreble
    , voicing        = Voicing.FourWayClose
    , startingOctave = Octave.middle
    , adjustOctave   = \_ -> Just noOctaveShift
    , numInversions  = 2
    , chordType
    , inversionsFor
    }
mkExercise LeftHand title intro chordType inversionsFor = ChordExercise{
      title
    , intro          = Just intro
    , clef           = Ly.ClefBass
    , voicing        = Voicing.FourWayClose
      -- Starting octave is not critical here, as we adjust it anyway, but it's
      -- more useful for development /of/ 'adjustOctave' if notes are placed in
      -- a readable position on the bass cleff
    , startingOctave = Octave.middle - 2
    , adjustOctave   = Chord.Named.moveToRange ("D3", "G4")
    , numInversions  = 2
    , chordType
    , inversionsFor
    }
