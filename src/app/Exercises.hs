module Exercises (exercises) where

import Lilypond (Lilypond)
import Lilypond qualified as Ly

import Exercises.Chords qualified as Chords

exercises :: Lilypond
exercises = Ly.Lilypond{
      header = Ly.Header{
          title    = "Music Theory Exercises"
        , composer = "edsko@edsko.net"
        }
    , scores = [
          Chords.majorTriads
        , Chords.majorSeventh
        , Chords.dominantSeventh
        ]
    }