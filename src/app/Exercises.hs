module Exercises (exercises) where

import Lilypond (Lilypond)
import Lilypond qualified as Ly

import Exercises.Chords.Basic qualified as Chords.Basic

exercises :: Lilypond
exercises = Ly.Lilypond{
      header = Ly.Header{
          title    = "Music Theory Exercises"
        , composer = "edsko@edsko.net"
        }
    , scores = Chords.Basic.exercises
    }