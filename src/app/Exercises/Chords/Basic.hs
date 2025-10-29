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
  Individual exercises

  When we use an inversion to have the seventh at the bottom, we shift
  everything down an octave.
-------------------------------------------------------------------------------}

majorTriads :: Ly.Score
majorTriads = Ly.Score{
      title = "Major triads, root position"
    , intro = Nothing
    , staff =
        chordExercise
          Chord.MajorTriad
          Voicing.Default
          [(rootPosition, noOctaveShift)]
    }

majorSeventh :: Ly.Score
majorSeventh = Ly.Score{
      title = "Major seventh chords, root position"
    , intro = Nothing
    , staff =
        chordExercise
          Chord.Major7
          Voicing.Default
          [(rootPosition, noOctaveShift)]
    }

dominantSeventh :: Ly.Score
dominantSeventh = Ly.Score{
      title = "Dominant seventh chords, seventh in the bass"
    , intro = Nothing
    , staff =
        chordExercise
          Chord.Dominant7
          Voicing.Default
          [(Inversion 3, OctaveShift (-1))]
    }

minorSeventh :: Ly.Score
minorSeventh = Ly.Score{
      title = "Minor seventh chords, seventh in the bass"
    , intro = Nothing
    , staff =
        chordExercise
          Chord.Minor7
          Voicing.Default
          [(Inversion 3, OctaveShift (-1))]
    }
