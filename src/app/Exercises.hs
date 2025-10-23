module Exercises (exercises) where

import Lilypond (Lilypond)
import Lilypond qualified as Ly

import Exercises.Lilypond.Style qualified as Style

import Exercises.Chords.Basic         qualified as Chords.Basic
import Exercises.Chords.StdJazz       qualified as Chords.StdJazz
import Exercises.Progressions.StdJazz qualified as Progressions.StdJazz

exercises :: Lilypond Style.Class
exercises = Ly.Lilypond{
      books = [
          Ly.Book{
              title  = "Music Theory Exercises"
            , author = "Edsko de Vries <edsko@edsko.net>"
            , parts  = [
                  chords
                , progressions
                ]
            }
        ]
    }

chords :: Ly.Bookpart Style.Class
chords = Ly.Bookpart{
      title    = "Chords"
    , sections = [
          Chords.Basic.exercises
        , Chords.StdJazz.exercises
        ]
    }

progressions :: Ly.Bookpart Style.Class
progressions = Ly.Bookpart{
      title    = "Progressions"
    , sections = [
          Progressions.StdJazz.exercises
        ]
    }
