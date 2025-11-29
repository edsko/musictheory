module Exercises (exercises) where

import Lilypond (Lilypond)
import Lilypond qualified as Ly

import Exercises.Additional                qualified as Additional
import Exercises.Chords.Basic              qualified as Chords.Basic
import Exercises.Chords.FourWayClose       qualified as Chords.FourWayClose
import Exercises.Progressions.FourWayClose qualified as Progressions.FourWayClose

exercises :: Lilypond
exercises = Ly.Lilypond{
      books = [
          Ly.Book{
              title  = "Music Theory Exercises"
            , author = Just "Michael Keithson"
            , parts  = [
                  chords
                , progressions
                , additional
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

additional :: Ly.Bookpart
additional = Ly.Bookpart{
      title    = "Additional exercises"
    , sections = Additional.exercises
    }

