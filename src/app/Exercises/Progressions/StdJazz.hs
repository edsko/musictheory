module Exercises.Progressions.StdJazz (exercises) where

import MusicTheory
import MusicTheory.Progression.Name qualified as Progression (Name(..))

import Lilypond qualified as Ly

import Exercises.Progressions

{-------------------------------------------------------------------------------
  List of exercises
-------------------------------------------------------------------------------}

exercises :: Ly.Section
exercises = Ly.Section{
      title  = "Using standard Jazz voicings"
    , intro  = Just $ concat [
          "Every progression shown twice: "
        , "first with the seventh in the bass of the first chord, "
        , " then with the third in the bass. "
        , "Basic voice leading is applied in both cases."
        ]
    , scores = [
          major251
        ]
    }

{-------------------------------------------------------------------------------
  Individual exercises
-------------------------------------------------------------------------------}

major251 :: Ly.Score
major251 = Ly.Score{
      title = "Major 2-5-1"
    , elems =
        progressionExercise
          Progression.StdJazz_Major251
          [(Inversion 3, OctaveShift (-1)), (Inversion 1, OctaveShift (0))]
          [Inversion 1, Inversion 3]
    }
