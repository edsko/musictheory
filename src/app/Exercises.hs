module Exercises (exercises) where

import Lilypond (Lilypond)
import Lilypond qualified as Ly

import Exercises.Chords.Basic qualified as Chords.Basic

exercises :: Lilypond
exercises = Ly.Lilypond{
      books = [
          Ly.Book{
              title  = "Music Theory Exercises"
            , author = "Edsko de Vries <edsko@edsko.net>"
            , parts  = [
                  chordExercises
                ]
            }
        ]
    }

chordExercises :: Ly.Bookpart
chordExercises = Ly.Bookpart{
      title    = "Chord Exercises"
    , sections = [
          Chords.Basic.exercises
        ]
    }
