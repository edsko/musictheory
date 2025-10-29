module Exercises.Chords.Basic (exercises) where

import MusicTheory
import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Voicing qualified as Voicing

import Lilypond qualified as Ly

import Exercises.Chords

{-------------------------------------------------------------------------------
  List of exercises
-------------------------------------------------------------------------------}

exercises :: Ly.Section
exercises = Ly.Section{
      title = "Basic chord exercises"
    , intro = Nothing
    , elems = [
          Ly.SectionSub $ Ly.Section{
              title = "Root position"
            , intro = Nothing
            , elems = [
                  Ly.SectionScore majorTriads
                , Ly.SectionScore majorSeventh
                , Ly.SectionScore dominantSeventh
                , Ly.SectionScore minorSeventh
                ]
            }
        ]
    }

{-------------------------------------------------------------------------------
  Root position
-------------------------------------------------------------------------------}

majorTriads :: Ly.Score
majorTriads = Ly.Score{
      title = "Major triad"
    , intro = Nothing
    , staff =
        chordExercise
          Chord.MajorTriad
          Voicing.Default
          [(rootPosition, noOctaveShift)]
    }

majorSeventh :: Ly.Score
majorSeventh = Ly.Score{
      title = "Major seventh"
    , intro = Nothing
    , staff =
        chordExercise
          Chord.Major7
          Voicing.Default
          [(rootPosition, noOctaveShift)]
    }

dominantSeventh :: Ly.Score
dominantSeventh = Ly.Score{
      title = "Dominant seventh"
    , intro = Nothing
    , staff =
        chordExercise
          Chord.Dominant7
          Voicing.Default
          [(rootPosition, noOctaveShift)]
    }

minorSeventh :: Ly.Score
minorSeventh = Ly.Score{
      title = "Minor seventh"
    , intro = Nothing
    , staff =
        chordExercise
          Chord.Minor7
          Voicing.Default
          [(rootPosition, noOctaveShift)]
    }
