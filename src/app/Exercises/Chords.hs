module Exercises.Chords (
    majorTriads
  ) where

import MusicTheory.Chord qualified as Chord
import MusicTheory.Scale qualified as Scale

import Lilypond qualified as Ly

{-------------------------------------------------------------------------------
  Individual exercises
-------------------------------------------------------------------------------}

majorTriads :: Ly.Score
majorTriads = Ly.Score{
      header = Ly.ScoreHeader{
          piece = "Major triads, root position"
        }
    , elems = allChordsOfType Chord.TriadMajor
    }

{-------------------------------------------------------------------------------
  Internall auxiliary
-------------------------------------------------------------------------------}

allChordsOfType :: Chord.Type -> Ly.ScoreElem
allChordsOfType typ = Ly.Staff $ Ly.Absolute [
      Ly.AbsoluteElem{
          chordName = Just $ Ly.ChordName (Scale.rootNote scale) typ
        , chord     = Chord.wrtMajorScale typ scale
        , duration  = Ly.Whole
        }
    | scale <- Scale.scaleNames
    ]
