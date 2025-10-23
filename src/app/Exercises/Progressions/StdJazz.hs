module Exercises.Progressions.StdJazz (exercises) where

import MusicTheory
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
      title  = sectionTitle "Using standard Jazz voicings"
    , intro  = Just $ Ly.Markup.Wordwrap $ mconcat [
          "Every progression shown twice: "
        , "first with the seventh in the bass of the first chord, "
        , " then with the third in the bass. "
        , "Basic voice leading is applied in both cases."
        ]
    , scores = [
          major251
        , minor251
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
          [(Inversion 3, OctaveShift (-1)), (Inversion 1, OctaveShift (0))]
          [Inversion 1, Inversion 3]
          (Scale.allOfType Scale.Major)
    }

minor251 :: Ly.Score Style.Class
minor251 = Ly.Score{
      title = exerciseTitle "Minor 2-5-1"
    , intro = Nothing
    , staff =
        progressionExercise
          (Progression.named Progression.StdJazz_Minor251)
          [(Inversion 3, OctaveShift (-1)), (Inversion 1, OctaveShift (0))]
          [Inversion 1, Inversion 3]
          (Scale.allOfType Scale.Minor)
    }
