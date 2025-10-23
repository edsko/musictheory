module Exercises.Progressions.StdJazz (exercises) where

import MusicTheory
import MusicTheory.Chord qualified as Chord
import MusicTheory.Progression qualified as Progression
import MusicTheory.Scale qualified as Scale

import Lilypond qualified as Ly
import Lilypond.Markup qualified as Ly.Markup

import Exercises.Lilypond
import Exercises.Lilypond.Style qualified as Style
import Exercises.Progressions

{-------------------------------------------------------------------------------
  List of exercises
-------------------------------------------------------------------------------}

exercises :: Ly.Section Style.Class
exercises = Ly.Section{
      title = sectionTitle "Using standard Jazz voicings"
    , intro = Just $ Ly.Markup.Wordwrap $ mconcat [
          "Every progression shown twice: "
        , "first with the third in the bass of the first chord, "
        , " then with the seventh in the bass. "
        , "Basic voice leading is applied in both cases."
        ]
    , elems = [
          Ly.SectionScore major251
        , Ly.SectionScore minor251
        ]
    }

{-------------------------------------------------------------------------------
  Individual exercises
-------------------------------------------------------------------------------}

major251 :: Ly.Score Style.Class
major251 = Ly.Score{
      title = exerciseTitle "Major 2-5-1"
    , intro = Nothing
    , staff =
        progressionExercise
          (Progression.named Progression.StdJazz_Major251)
          -- Starts on a rootless minor chord
          [(Inversion 0, noOctaveShift), (Inversion 2, OctaveShift (-1))]
          permissibleInversions
          Scale.allMajorScales
    }

minor251 :: Ly.Score Style.Class
minor251 = Ly.Score{
      title = exerciseTitle "Minor 2-5-1"
    , intro = Nothing
    , staff =
        progressionExercise
          (Progression.named Progression.StdJazz_Minor251)
          -- This starts on a half-dimished chord, which we voice with a root.
          [(Inversion 1, noOctaveShift), (Inversion 3, OctaveShift (-1))]
          permissibleInversions
          Scale.allMinorScales
    }

-- | Possible inversions
--
-- See comment in "Exercises.Chords.StdJazz" regarding inversions.
--
-- We don't need to implement the inversions for the half diminished chord, as
-- it (currently) only appears as the /first/ chord.
permissibleInversions :: Chord.Type -> [Inversion]
permissibleInversions = \case
    Chord.StdJazz_Dominant     -> [Inversion 0, Inversion 2]
    Chord.StdJazz_AlteredFlat9 -> [Inversion 0, Inversion 2]
    Chord.StdJazz_Major        -> [Inversion 0, Inversion 2]
    Chord.StdJazz_Minor        -> [Inversion 0, Inversion 2]

    typ -> error $ "Not implemented: " ++ show typ
