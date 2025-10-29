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
                  Ly.SectionScore $ majorTriads     onlyRootPosition
                , Ly.SectionScore $ majorSeventh    onlyRootPosition
                , Ly.SectionScore $ dominantSeventh onlyRootPosition
                , Ly.SectionScore $ minorSeventh    onlyRootPosition
                ]
            }
        , Ly.SectionSub $ Ly.Section{
              title = "Inversions"
            , intro = Just $ Ly.Markup.Wordwrap $ mconcat [
                  "All chords are first shown in root position, "
                , "followed by all possible inversions."
                ]
            , elems = [
                  Ly.SectionScore $ majorTriads     allInversionsTriad
                , Ly.SectionScore $ majorSeventh    allInversionsSeventh
                , Ly.SectionPageBreak
                , Ly.SectionScore $ dominantSeventh allInversionsSeventh
                , Ly.SectionScore $ minorSeventh    allInversionsSeventh
                ]
            }
        ]
    }
  where
    onlyRootPosition :: [(Inversion, OctaveShift)]
    onlyRootPosition = [(rootPosition, noOctaveShift)]

    allInversionsTriad :: [(Inversion, OctaveShift)]
    allInversionsTriad = [
          (rootPosition , noOctaveShift)
        , (Inversion 1  , noOctaveShift)
        , (Inversion 2  , noOctaveShift)
        ]

    allInversionsSeventh :: [(Inversion, OctaveShift)]
    allInversionsSeventh = [
          (rootPosition , noOctaveShift)
        , (Inversion 1  , noOctaveShift)
        , (Inversion 2  , noOctaveShift)
        , (Inversion 3  , noOctaveShift)
        ]

{-------------------------------------------------------------------------------
  Root position
-------------------------------------------------------------------------------}

majorTriads :: [(Inversion, OctaveShift)] -> Ly.Score
majorTriads inversions = Ly.Score{
      title = "Major triad"
    , intro = Nothing
    , staff = chordExercise Chord.MajorTriad Voicing.Default inversions
    }

majorSeventh :: [(Inversion, OctaveShift)] -> Ly.Score
majorSeventh inversions = Ly.Score{
      title = "Major seventh"
    , intro = Nothing
    , staff = chordExercise Chord.Major7 Voicing.Default inversions
    }

dominantSeventh :: [(Inversion, OctaveShift)] -> Ly.Score
dominantSeventh inversions = Ly.Score{
      title = "Dominant seventh"
    , intro = Nothing
    , staff = chordExercise Chord.Dominant7 Voicing.Default inversions
    }

minorSeventh :: [(Inversion, OctaveShift)] -> Ly.Score
minorSeventh inversions = Ly.Score{
      title = "Minor seventh"
    , intro = Nothing
    , staff = chordExercise Chord.Minor7 Voicing.Default inversions
    }
