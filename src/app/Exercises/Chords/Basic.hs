module Exercises.Chords.Basic (exercises) where

import MusicTheory
import MusicTheory.Chord qualified as Chord

import Lilypond qualified as Ly

import Exercises.Chords
import Exercises.Lilypond
import Exercises.Lilypond.Style qualified as Style

{-------------------------------------------------------------------------------
  List of exercises
-------------------------------------------------------------------------------}

exercises :: Ly.Section Style.Class
exercises = Ly.Section{
      title  = sectionTitle "Basic chord exercises"
    , intro  = Nothing
    , scores = [
          majorTriads
        , majorSeventh
        , dominantSeventh
        , minorSeventh
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
          Chord.Basic_MajorTriad
          [(rootPosition, noOctaveShift)]
    }

majorSeventh :: Ly.Score Style.Class
majorSeventh = Ly.Score{
      title = exerciseTitle "Major seventh chords, root position"
    , intro = Nothing
    , staff =
        chordExercise
          Chord.Basic_MajorSeventh
          [(rootPosition, noOctaveShift)]
    }

dominantSeventh :: Ly.Score Style.Class
dominantSeventh = Ly.Score{
      title = exerciseTitle "Dominant seventh chords, seventh in the bass"
    , intro = Nothing
    , staff =
        chordExercise
          Chord.Basic_DominantSeventh
          [(Inversion 3, OctaveShift (-1))]
    }

minorSeventh :: Ly.Score Style.Class
minorSeventh = Ly.Score{
      title = exerciseTitle "Minor seventh chords, seventh in the bass"
    , intro = Nothing
    , staff =
        chordExercise
          Chord.Basic_MinorSeventh
          [(Inversion 3, OctaveShift (-1))]
    }
