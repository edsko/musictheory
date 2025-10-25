module Exercises.Progressions.StdJazz (exercises) where

import MusicTheory
import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Voicing qualified as Voicing
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
    , elems = concatMap addPageBreak [
          Ly.SectionScore major251
        , Ly.SectionScore minor251
        ]
    }
  where
    -- By adding a page break just before all 12 scales fit on a page
    addPageBreak :: Ly.SectionElem Style.Class -> [Ly.SectionElem Style.Class]
    addPageBreak e = [Ly.SectionPageBreak, e]

{-------------------------------------------------------------------------------
  Individual exercises
-------------------------------------------------------------------------------}

major251 :: Ly.Score Style.Class
major251 = Ly.Score{
      title = exerciseTitle "Major 2-5-1"
    , intro = Nothing
    , staff =
        progressionExercise
          (Progression.named Progression.Major251)
          Voicing.StdJazz
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
          (Progression.named Progression.Minor251)
          Voicing.StdJazz
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
    Chord.Dominant7    -> [Inversion 0, Inversion 2]
    Chord.AlteredFlat9 -> [Inversion 0, Inversion 2]
    Chord.Major7       -> [Inversion 0, Inversion 2]
    Chord.Minor7       -> [Inversion 0, Inversion 2]

    typ -> error $ "Not implemented: " ++ show typ
