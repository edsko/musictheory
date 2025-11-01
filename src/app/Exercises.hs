module Exercises (exercises) where

import Lilypond (Lilypond)
import Lilypond qualified as Ly

import Exercises.Chords.Basic         qualified as Chords.Basic
import Exercises.Chords.FourWayClose       qualified as Chords.FourWayClose
import Exercises.Progressions.FourWayClose qualified as Progressions.FourWayClose

exercises :: Lilypond
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

chords :: Ly.Bookpart
chords = Ly.Bookpart{
      title    = "Chords"
    , sections = concat [
          Chords.Basic.exercises
        , [Chords.FourWayClose.exercises]
        ]
    }

progressions :: Ly.Bookpart
progressions = Ly.Bookpart{
      title    = "Progressions"
    , sections = [
          Progressions.FourWayClose.exercises
        ]
    }
