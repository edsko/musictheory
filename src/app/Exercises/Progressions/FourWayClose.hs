module Exercises.Progressions.FourWayClose (exercises) where

import Data.Foldable

import MusicTheory
import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Voicing qualified as Voicing
import MusicTheory.Progression qualified as Progression
import MusicTheory.Scale qualified as Scale

import Lilypond qualified as Ly
import Lilypond.Markup qualified as Ly.Markup

import Exercises.Progressions
import Exercises.Util.ChordInversion (ChordInversion(..))
import Exercises.Util.TypeAB (TypeAB(..))
import Exercises.Util.TypeAB qualified as TypeAB

{-------------------------------------------------------------------------------
  List of exercises
-------------------------------------------------------------------------------}

exercises :: Ly.Section
exercises = Ly.Section{
      title = "Four Note Closed Hand Voicings"
    , intro = Just $ Ly.Markup.wordwrap $ mconcat [
          "Every progression shown twice: "
        , "first starting with the type A voicing, "
        , "then with the type B voicing. "
        , "Basic voice leading is applied in both cases."
        ]
    , elems = concat [
          major251 "Major 2-5-1 using 7"     Progression.WithoutSevenFlat9
        , major251 "Major 2-5-1 using 7(♭9)" Progression.WithSevenFlat9
        , minor251 "Minor 2-5-1 using 7(♭9)" Progression.WithSevenFlat9
        , minor251 "Minor 2-5-1 using 7alt"  Progression.WithoutSevenFlat9
        ]
    }

{-------------------------------------------------------------------------------
  Individual exercises
-------------------------------------------------------------------------------}

major251 :: String -> Progression.UseSevenFlat9 -> [Ly.SectionElem]
major251 title useSevenFlat9 =
    progressionExercise Scale.Major $
      mkExercise
        title
        (Progression.Major251 useSevenFlat9)
        startingInversion
  where
    -- Starts on a rootless minor chord
    startingInversion :: Scale.Root -> [ChordInversion]
    startingInversion scaleRoot = toList $
        TypeAB.markInversion (\_ -> scaleRoot == Scale.C) TypeAB{
            typeA = (Inversion 0, noOctaveShift)
          , typeB = (Inversion 2, OctaveShift (-1))
          }

minor251 :: String -> Progression.UseSevenFlat9 -> [Ly.SectionElem]
minor251 title useSevenFlat9 =
    progressionExercise Scale.Minor $
      mkExercise
        title
        (Progression.Minor251 useSevenFlat9)
        startingInversion
  where
    -- This starts on a half-dimished chord, which we voice with a root.
    startingInversion :: Scale.Root -> [ChordInversion]
    startingInversion scaleRoot = toList $
        TypeAB.markInversion (\_ -> scaleRoot == Scale.C) TypeAB{
            typeA = (Inversion 1, noOctaveShift)
          , typeB = (Inversion 3, OctaveShift (-1))
          }

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

mkExercise ::
     String
  -> Progression.Name
  -> (Scale.Root -> [ChordInversion])
  -> ProgressionExercise
mkExercise title progressionName startingInversion = ProgressionExercise{
      title
    , intro                 = Nothing
    , progressionName
    , voicing               = Voicing.FourWayClose
    , startingInversion
    , permissibleInversions
    }
  where
  -- Possible inversions
  --
  -- See comment in "Exercises.Chords.FourWayClose" regarding inversions.
  --
  -- We don't need to implement the inversions for the half diminished chord, as
  -- it (currently) only appears as the /first/ chord.
  permissibleInversions :: Chord.Type -> [Inversion]
  permissibleInversions = \case
      Chord.Dominant7  -> [Inversion 0, Inversion 2]
      Chord.SevenFlat9 -> [Inversion 0, Inversion 2]
      Chord.Major7     -> [Inversion 0, Inversion 2]
      Chord.Minor7     -> [Inversion 0, Inversion 2]
      Chord.Altered    -> [Inversion 0, Inversion 2]

      typ -> error $ "Not implemented: " ++ show typ
