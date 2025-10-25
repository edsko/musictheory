module Exercises.Chords.Basic (exercises) where

import MusicTheory
import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Voicing qualified as Voicing

import Lilypond qualified as Ly

import Exercises.Chords
import Exercises.Lilypond
import Exercises.Lilypond.Style qualified as Style

{-------------------------------------------------------------------------------
  List of exercises
-------------------------------------------------------------------------------}

exercises :: Ly.Section Style.Class
exercises = Ly.Section{
      title = sectionTitle "Basic chord exercises"
    , intro = Nothing
    , elems = [
          Ly.SectionScore majorTriads
        , Ly.SectionScore majorSeventh
        , Ly.SectionScore dominantSeventh
        , Ly.SectionScore minorSeventh
        ]
    }

{-------------------------------------------------------------------------------
  Individual exercises

  When we use an inversion to have the seventh in the bass, we shift everything
  down an octave.
-------------------------------------------------------------------------------}

majorTriads :: Ly.Score Style.Class
majorTriads = Ly.Score{
      title = exerciseTitle "Major triads, root position"
    , intro = Nothing
    , staff =
        chordExercise
          Chord.MajorTriad
          Voicing.Default
          [(rootPosition, noOctaveShift)]
    }

majorSeventh :: Ly.Score Style.Class
majorSeventh = Ly.Score{
      title = exerciseTitle "Major seventh chords, root position"
    , intro = Nothing
    , staff =
        chordExercise
          Chord.Major7
          Voicing.Default
          [(rootPosition, noOctaveShift)]
    }

dominantSeventh :: Ly.Score Style.Class
dominantSeventh = Ly.Score{
      title = exerciseTitle "Dominant seventh chords, seventh in the bass"
    , intro = Nothing
    , staff =
        chordExercise
          Chord.Dominant7
          Voicing.Default
          [(Inversion 3, OctaveShift (-1))]
    }

minorSeventh :: Ly.Score Style.Class
minorSeventh = Ly.Score{
      title = exerciseTitle "Minor seventh chords, seventh in the bass"
    , intro = Nothing
    , staff =
        chordExercise
          Chord.Minor7
          Voicing.Default
          [(Inversion 3, OctaveShift (-1))]
    }
