module Exercises (exercises) where

import Data.Default

import Lilypond (Lilypond)
import Lilypond qualified as Ly

import Exercises.Chords.Basic qualified as Chords.Basic

exercises :: Lilypond
exercises = Ly.Lilypond{
      header = def
    , books = [
          Ly.Book{
              header = def{
                  Ly.title    = Just "Music Theory Exercises"
                , Ly.arranger = Just "edsko@edsko.net"
                , Ly.tagline  = Just ""
                }
            , parts = [
                  chordExercises
                ]
            }
        ]
    }

chordExercises :: Ly.BookPart
chordExercises = Ly.BookPart{
      Ly.header = def{
          Ly.title    = Just "Chord Exercises"
        , Ly.arranger = Just ""
        }
    , Ly.elems = mconcat [
          Chords.Basic.exercises
        ]
    }
