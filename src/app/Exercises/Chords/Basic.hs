module Exercises.Chords.Basic (exercises) where

import MusicTheory
import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Voicing qualified as Voicing

import Lilypond qualified as Ly
import Lilypond.Markup qualified as Ly.Markup

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
                  Ly.SectionScore $ majorTriads     (inversions [0])
                , Ly.SectionScore $ majorSeventh    (inversions [0])
                , Ly.SectionScore $ dominantSeventh (inversions [0])
                , Ly.SectionScore $ minorSeventh    (inversions [0])
                ]
            }
        , Ly.SectionSub $ Ly.Section{
              title = "Inversions"
            , intro = Just $ Ly.Markup.Wordwrap $ mconcat [
                  "All chords are first shown in root position, "
                , "followed by all possible inversions."
                ]
            , elems = [
                  Ly.SectionScore $ majorTriads     (inversions [0..2])
                , Ly.SectionScore $ majorSeventh    (inversions [0..3])
                , Ly.SectionPageBreak
                , Ly.SectionScore $ dominantSeventh (inversions [0..3])
                , Ly.SectionScore $ minorSeventh    (inversions [0..3])
                ]
            }
        ]
    }
  where
    inversions :: [Word] -> [ChordInversion]
    inversions is = [
          ChordInversion (Inversion i) noOctaveShift Ly.NoAnnotation
        | i <- is
        ]

{-------------------------------------------------------------------------------
  Root position
-------------------------------------------------------------------------------}

majorTriads :: [ChordInversion] -> Ly.Score
majorTriads inversions = Ly.Score{
      title = "Major triad"
    , intro = Nothing
    , staff =
        chordExercise
          Chord.MajorTriad
          Voicing.Default
          (length inversions)
          (\_ -> inversions)
    }

majorSeventh :: [ChordInversion] -> Ly.Score
majorSeventh inversions = Ly.Score{
      title = "Major seventh"
    , intro = Nothing
    , staff =
        chordExercise
          Chord.Major7
          Voicing.Default
          (length inversions)
          (\_ -> inversions)
    }

dominantSeventh :: [ChordInversion] -> Ly.Score
dominantSeventh inversions = Ly.Score{
      title = "Dominant seventh"
    , intro = Nothing
    , staff =
        chordExercise
          Chord.Dominant7
          Voicing.Default
          (length inversions)
          (\_ -> inversions)
    }

minorSeventh :: [ChordInversion] -> Ly.Score
minorSeventh inversions = Ly.Score{
      title = "Minor seventh"
    , intro = Nothing
    , staff =
        chordExercise
          Chord.Minor7
          Voicing.Default
          (length inversions)
          (\_ -> inversions)
    }
