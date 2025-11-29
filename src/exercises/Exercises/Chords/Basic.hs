module Exercises.Chords.Basic (exercises) where

import MusicTheory
import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Voicing qualified as Voicing
import MusicTheory.Note.Octave qualified as Octave
import MusicTheory.Scale qualified as Scale

import Lilypond qualified as Ly

import Construction.Chords qualified as Chords
import Construction.Util.ChordInversion (ChordInversion(..))

{-------------------------------------------------------------------------------
  List of exercises
-------------------------------------------------------------------------------}

exercises :: [Ly.Section]
exercises = [
      Ly.Section{
          title = "Triads"
        , intro = mempty
        , elems = [
              Ly.SectionSub $ Ly.Section{
                  title = "Root position"
                , intro = mempty
                , elems = triadsRoot
                }
            , Ly.SectionSub $ Ly.Section{
                  title = "Inversions"
                , intro = Ly.Paragraphs [
                      "We show all chords in root position followed by all possible inversions."
                    ]
                , elems = triadsInversions
                }
            ]
        }
    , Ly.Section{
          title = "Seventh chords"
        , intro = mempty
        , elems = [
              Ly.SectionSub $ Ly.Section{
                  title = "Root position"
                , intro = mempty
                , elems = seventhsRoot
                }
            , Ly.SectionSub $ Ly.Section{
                  title = "Inversions"
                , intro = Ly.Paragraphs [
                      "We show all chords in root position followed by all possible inversions."
                    ]
                , elems = seventhsInversions
                }
            ]
        }
    ]

triadsRoot :: [Ly.SectionElem]
triadsRoot = concat [
      Chords.exercise
        Scale.Major
        (mkSetup "Major" 1)
        (mkExercise Chord.MajorTriad invs)
    , Chords.exercise
        Scale.Minor
        (mkSetup "Minor" 1)
        (mkExercise Chord.MinorTriad invs)
    , Chords.exercise
        Scale.Minor
        (mkSetup "Diminished" 1)
        (mkExercise Chord.DiminishedTriad invs)
    ]
  where
    invs :: Scale.Root -> [ChordInversion]
    invs _ = [ChordInversion rootPosition noOctaveShift Ly.NoAnnotation]

triadsInversions :: [Ly.SectionElem]
triadsInversions = concat [
      Chords.exercise
        Scale.Major
        (mkSetup "Major" 3)
        (mkExercise Chord.MajorTriad invs)
    , Chords.exercise
        Scale.Minor
        (mkSetup "Minor" 3)
        (mkExercise Chord.MinorTriad invs)
    , Chords.exercise
        Scale.Minor
        (mkSetup "Diminished" 3)
        (mkExercise Chord.DiminishedTriad invs)
    ]
  where
    invs :: Scale.Root -> [ChordInversion]
    invs root = [
         ChordInversion (Inversion i) noOctaveShift (inversionAnn root i)
       | i <- [0..2]
       ]

seventhsRoot :: [Ly.SectionElem]
seventhsRoot = concat [
      Chords.exercise
        Scale.Major
        (mkSetup "Major seventh" 1)
        (mkExercise Chord.Major7 invs)
    , Chords.exercise
        Scale.Major
        (mkSetup "Dominant seventh" 1)
        (mkExercise Chord.Dominant7 invs)
    , [Ly.SectionPageBreak]
    , Chords.exercise
        Scale.Minor
        (mkSetup "Minor seventh" 1)
        (mkExercise Chord.Minor7 invs)
    , Chords.exercise
        Scale.Minor
        (mkSetup "Half-diminished / m7(♭5)" 1)
        (mkExercise Chord.HalfDiminished invs)
    , Chords.exercise
        Scale.Minor
        (mkSetup "Diminished" 1)
        (mkExercise Chord.Diminished7 invs)
    ]
  where
    invs :: Scale.Root -> [ChordInversion]
    invs _ = [ChordInversion rootPosition noOctaveShift Ly.NoAnnotation]

seventhsInversions :: [Ly.SectionElem]
seventhsInversions = concat [
      Chords.exercise
        Scale.Major
        (mkSetup "Major seventh" 4)
        (mkExercise Chord.Major7 invs)
    , [Ly.SectionPageBreak]
    , Chords.exercise
        Scale.Major
        (mkSetup "Dominant seventh" 4)
        (mkExercise Chord.Dominant7 invs)
    , [Ly.SectionPageBreak]
    , Chords.exercise
        Scale.Minor
        (mkSetup "Minor seventh" 4)
        (mkExercise Chord.Minor7 invs)
    , [Ly.SectionPageBreak]
    , Chords.exercise
        Scale.Minor
        (mkSetup "Half-diminished / m7(♭5)" 4)
        (mkExercise Chord.HalfDiminished invs)
    , [Ly.SectionPageBreak]
    , Chords.exercise
        Scale.Minor
        (mkSetup "Diminished" 4)
        (mkExercise Chord.Diminished7 invs)
    ]
  where
    invs :: Scale.Root -> [ChordInversion]
    invs root = [
         ChordInversion (Inversion i) noOctaveShift (inversionAnn root i)
       | i <- [0..3]
       ]

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

mkSetup :: String -> Int -> Chords.Setup
mkSetup title numInvs = Chords.Setup{
      title
    , intro          = mempty
    , clef           = Ly.ClefTreble
    , numInversions  = numInvs
    }

mkExercise :: Chord.Type -> (Scale.Root -> [ChordInversion]) -> Chords.Exercise
mkExercise chordType invs = Chords.Exercise{
      voicing        = Voicing.Default
    , startingOctave = Octave.middle
    , simplifyNotes  = False
    , adjustOctave   = \_ -> Just noOctaveShift
    , inversionsFor  = invs
    , chordType
    }

inversionAnn :: Scale.Root -> Word -> Ly.Annotation
inversionAnn root i =
    case (root, i) of
      (Scale.C, 0) -> "root"
      (Scale.C, 1) -> "1st"
      (Scale.C, 2) -> "2nd"
      (Scale.C, 3) -> "3rd"
      (Scale.C, _) -> error $ "unexpected inversion " ++ show i
      _otherwise   -> Ly.NoAnnotation
