module Exercises.Chords.FourWayClose (exercises) where

import Data.Foldable

import MusicTheory
import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Named qualified as Chord.Named
import MusicTheory.Chord.Named qualified as Named (Chord(..))
import MusicTheory.Chord.Voicing qualified as Voicing
import MusicTheory.Note.Octave qualified as Octave
import MusicTheory.Reference
import MusicTheory.Scale qualified as Scale

import Lilypond qualified as Ly
import Lilypond.Markup qualified as Ly (Markup)
import Lilypond.Markup qualified as Ly.Markup

import Exercises.Chords qualified as Chords
import Exercises.Util.ChordInversion (ChordInversion(..))
import Exercises.Util.TypeAB (TypeAB(..))
import Exercises.Util.TypeAB qualified as TypeAB

{-------------------------------------------------------------------------------
  List of exercises
-------------------------------------------------------------------------------}

exercises :: Ly.Section
exercises = Ly.Section{
      title = "Four Note Closed Hand Voicings"
    , intro = Ly.Paragraphs [
          Ly.Markup.wordwrap $ mconcat [
              "Every is chord shown twice: "
            , "first with the third at the bottom (type A), "
            , "then with the seventh at the bottom (type B)."
            ]
        ]
    , elems = [
          Ly.SectionSub $ Ly.Section{
              title = "Right hand"
            , intro = mempty
            , elems = exercisesForHand RightHand
            }
        , Ly.SectionSub $ Ly.Section{
              title = "Left hand"
            , intro = introLeftHand
            , elems = exercisesForHand LeftHand
            }
        ]
    }
  where
    introLeftHand :: Ly.Paragraphs
    introLeftHand = Ly.Paragraphs [
          Ly.Markup.wordwrap $ mconcat [
              "Only voicings that fit between D3 and G4 are typically used. "
            , "Inversions outside this range are shown as rests."
            ]
        ]

exercisesForHand :: Hand -> [Ly.SectionElem]
exercisesForHand hand = concat [
      major          hand
    , sus            hand
    , [Ly.SectionPageBreak]

    , dominant       hand
    , altered        hand
    , [Ly.SectionPageBreak]

    , minor          hand
    , halfDiminished hand
    , [Ly.SectionPageBreak]

    , sevenFlat9     hand
    , diminished     hand
    ]

{-------------------------------------------------------------------------------
  Individual exercises

  NOTE: The rootless chords will start on the third without any inversions,
  and on the seventh after two inversions.
-------------------------------------------------------------------------------}

major :: Hand -> [Ly.SectionElem]
major hand =
    Chords.exercise
      Scale.Major
      (mkSetup    hand "Major seventh" intro)
      (mkExercise hand Chord.Major7 inversions)
  where
    intro :: Ly.Paragraphs
    intro = Ly.Paragraphs [
          Ly.Markup.wordwrap $ mconcat [
              "Voiced using"
            , showVoicing Chord.Major7
            , "."
            ]
        ]

    inversions :: Scale.Root -> [ChordInversion]
    inversions scaleRoot = toList $
        TypeAB.markInversion (\_ -> scaleRoot == Scale.C) TypeAB{
            typeA = (Inversion 0, noOctaveShift)
          , typeB = (Inversion 2, OctaveShift (-1))
          }

minor :: Hand -> [Ly.SectionElem]
minor hand =
    Chords.exercise
      Scale.Minor
      (mkSetup    hand "Minor seventh" intro)
      (mkExercise hand Chord.Minor7 inversions)
  where
    intro :: Ly.Paragraphs
    intro = Ly.Paragraphs [
          Ly.Markup.wordwrap $ mconcat [
              "Voiced using"
            , showVoicing Chord.Minor7
            , "."
            ]
        ]

    inversions :: Scale.Root -> [ChordInversion]
    inversions scaleRoot = toList $
        TypeAB.markInversion (\_ -> scaleRoot == Scale.C) TypeAB{
            typeA = (Inversion 0, noOctaveShift)
          , typeB = (Inversion 2, OctaveShift (-1))
          }

dominant :: Hand -> [Ly.SectionElem]
dominant hand =
    Chords.exercise
      Scale.Major
      (mkSetup    hand "Dominant seventh" intro)
      (mkExercise hand Chord.Dominant7 inversions)
  where
    intro :: Ly.Paragraphs
    intro = Ly.Paragraphs [
          Ly.Markup.wordwrap $ mconcat [
              "Voiced using"
            , showVoicing Chord.Dominant7
            , "."
            ]
        ]

    inversions :: Scale.Root -> [ChordInversion]
    inversions scaleRoot = toList $
        TypeAB.markInversion (\_ -> scaleRoot == Scale.C) TypeAB{
            typeA = (Inversion 0, noOctaveShift)
          , typeB = (Inversion 2, OctaveShift (-1))
          }

halfDiminished :: Hand -> [Ly.SectionElem]
halfDiminished hand =
    Chords.exercise
      Scale.Minor
      (mkSetup    hand "Half-diminished / m7(♭5)" intro)
      (mkExercise hand Chord.HalfDiminished inversions)
  where
    intro :: Ly.Paragraphs
    intro = Ly.Paragraphs [
          Ly.Markup.wordwrap $ mconcat [
              "Voiced using"
            , showVoicing Chord.HalfDiminished
            , "."
            ]
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
    Chords.exercise
      Scale.Major
      (mkSetup    hand "Altered seventh" intro)
      (mkExercise hand Chord.Altered inversions)
  where
    intro :: Ly.Paragraphs
    intro = Ly.Paragraphs [
          Ly.Markup.wordwrap $ mconcat [
              "Voiced using"
            , showVoicing Chord.Altered
            , "."
            ]
        ]

    inversions :: Scale.Root -> [ChordInversion]
    inversions scaleRoot = toList $
        TypeAB.markInversion (\_ -> scaleRoot == Scale.C) TypeAB{
            typeA = (Inversion 0, noOctaveShift)
          , typeB = (Inversion 2, OctaveShift (-1))
          }

sus :: Hand -> [Ly.SectionElem]
sus hand =
    Chords.exercise
      Scale.Major
      (mkSetup    hand "Suspended" intro)
      (mkExercise hand Chord.Sus inversions)
  where
    intro :: Ly.Paragraphs
    intro = Ly.Paragraphs [
          Ly.Markup.wordwrap $ mconcat [
              "Voiced using"
            , showVoicing Chord.Sus
            ,  "(or equivalently using a maj7 chord voiced using"
            , showVoicing Chord.Major7
            , " a whole step down)"
            , "."
            ]
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
    Chords.exercise
      Scale.Major
      (mkSetup    hand "7(♭9)" intro)
      (mkExercise hand Chord.SevenFlat9 inversions)
  where
    intro :: Ly.Paragraphs
    intro = Ly.Paragraphs [
          Ly.Markup.wordwrap $ mconcat [
              "Voiced using"
            , showVoicing Chord.SevenFlat9
            , " (or equivalenty as a diminished chord starting at the 3)"
            , "."
            ]
        ]

    inversions :: Scale.Root -> [ChordInversion]
    inversions scaleRoot = toList $
        TypeAB.markInversion (\_ -> scaleRoot == Scale.C) TypeAB{
            typeA = (Inversion 0, noOctaveShift)
          , typeB = (Inversion 2, OctaveShift (-1))
          }

diminished :: Hand -> [Ly.SectionElem]
diminished RightHand = [
      Ly.SectionScore $ Ly.Score{
          title = Just "Diminished"
        , intro = intro
        , staff = Ly.Staff{
              props = staffProps
            , elems = concat [
                  Chords.exerciseIn Scale.Minor exercise ["F"]
                , [Ly.StaffLinebreak]
                , Chords.exerciseIn Scale.Minor exercise ["F♯"]
                , [Ly.StaffLinebreak]
                , Chords.exerciseIn Scale.Minor exercise ["G"]
                , [Ly.StaffLinebreak]
                ]
            }
        }
    ]
  where
    staffProps :: Ly.StaffProps
    staffProps = Chords.staffProps Ly.ClefTreble 1

    intro :: Ly.Paragraphs
    intro = Ly.Paragraphs [
          Ly.Markup.wordwrap $ mconcat [
              "Since a diminished chord is entirely symmetrical, there are really "
            , "only three different diminished chords, for which we can choose a "
            , "default inversion starting on F, F♯, or G. "
            ]
        , Ly.Markup.wordwrap $ mconcat [
              "Technically speaking a diminished chord is voiced using "
            , showVoicing Chord.Diminished7
            , ". However, since diminished chords are not diatonic to any key, "
            , "we choose to use the simplest possible note spellings instead."
            ]
        ]

    exercise :: Chords.Exercise
    exercise = Chords.Exercise{
          chordType      = Chord.Diminished7
        , voicing        = Voicing.FourWayClose
        , simplifyNotes  = True
        , startingOctave = Octave.middle
        , adjustOctave   = \_ -> Just noOctaveShift
        , inversionsFor  = \scaleRoot -> [
              ChordInversion{
                  inversion   = i
                , octaveShift = noOctaveShift
                , annotation  = ann
                }
            | (i, ann) <- inversions scaleRoot
            ]
        }

    inversions :: Scale.Root -> [(Inversion, Ly.Annotation)]
    inversions scaleRoot =
        case scaleRoot of
          Scale.F    -> [ (rootPosition , "(F)")
                        , (Inversion 1  , "(G♯/A♭)")
                        , (Inversion 2  , "(B)")
                        , (Inversion 2  , "(D)")
                        ]
          Scale.F#   -> [ (rootPosition , "(F♯/G♭)")
                        , (Inversion 1  , "(A)")
                        , (Inversion 2  , "(C)")
                        , (Inversion 2  , "(D♯/E♭)")
                        ]
          Scale.G    -> [ (rootPosition , "(G)")
                        , (Inversion 1  , "(A♯/B♭)")
                        , (Inversion 2  , "(C♯/D♭)")
                        , (Inversion 2  , "(E)")
                        ]
          _otherwise -> error $ "Unexpected scale root " ++ show scaleRoot

diminished LeftHand = [
      Ly.SectionScore $ Ly.Score{
          title = Just "Diminished"
        , intro = intro
        , staff = Ly.Staff{
              props = staffProps
            , elems = concat [
                  Chords.exerciseIn Scale.Minor exercise ["F"]
                , Chords.exerciseIn Scale.Minor exercise ["F♯"]
                , Chords.exerciseIn Scale.Minor exercise ["G"]
                , [Ly.StaffLinebreak]
                ]
            }
        }
    ]
  where
    staffProps :: Ly.StaffProps
    staffProps = (Chords.staffProps Ly.ClefBass 2){
          Ly.stretchLastLine = True
        }

    intro :: Ly.Paragraphs
    intro = Ly.Paragraphs [
          Ly.Markup.wordwrap $ mconcat [
              "As for the right hand, we can choose to always use an inversion "
            , "starting on F, F♯ or G. Unlike for the right, however, only one "
            , "other inversion fits within the range D3-G4."
            ]
        ]

    exercise :: Chords.Exercise
    exercise = Chords.Exercise{
          chordType      = Chord.Diminished7
        , voicing        = Voicing.FourWayClose
        , simplifyNotes  = True
        , startingOctave = Octave.middle
        , adjustOctave   = adjustLeftHand
        , inversionsFor  = \scaleRoot -> [
              ChordInversion{
                  inversion   = i
                , octaveShift = noOctaveShift
                , annotation  = ann
                }
            | (i, ann) <- inversions scaleRoot
            ]
        }

    inversions :: Scale.Root -> [(Inversion, Ly.Annotation)]
    inversions scaleRoot =
        case scaleRoot of
          Scale.F    -> [ (rootPosition , "(F, G♯/A♭, B, D)")
                        , (Inversion 1  , Ly.NoAnnotation)
                        ]
          Scale.F#   -> [ (rootPosition , "(F♯/G♭, A, C, D♯/E♭)")
                        , (Inversion 1  , Ly.NoAnnotation)
                        ]
          Scale.G    -> [ (rootPosition , "(G, A♯/B♭, C♯/D♭, E)")
                        , (Inversion 1  , Ly.NoAnnotation)
                        ]
          _otherwise -> error $ "Unexpected scale root " ++ show scaleRoot

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

showVoicing :: Chord.Type -> Ly.Markup
showVoicing chordType =
    foldMap (Ly.Markup.Music . Ly.Markup.Interval) $
      Voicing.intervals Voicing.FourWayClose chordType

data Hand = RightHand | LeftHand

mkSetup :: Hand -> String -> Ly.Paragraphs -> Chords.Setup
mkSetup hand title intro = Chords.Setup{
      title
    , intro
    , clef           = case hand of
                         RightHand -> Ly.ClefTreble
                         LeftHand  -> Ly.ClefBass
    , numInversions  = 2
    }

mkExercise ::
     Hand
  -> Chord.Type
  -> (Scale.Root -> [ChordInversion])
  -> Chords.Exercise
mkExercise hand chordType inversionsFor = Chords.Exercise{
      chordType
    , voicing        = Voicing.FourWayClose
    , simplifyNotes  = False
    , startingOctave = Octave.middle
    , inversionsFor
    , adjustOctave   = case hand of
                         RightHand -> \_ -> Just noOctaveShift
                         LeftHand  -> adjustLeftHand
    }

adjustLeftHand :: Named.Chord Abs -> Maybe OctaveShift
adjustLeftHand = Chord.Named.moveToRange ("D3", "G4")

