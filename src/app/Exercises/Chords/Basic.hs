module Exercises.Chords.Basic (exercises) where

import MusicTheory
import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Voicing qualified as Voicing
import MusicTheory.Note.Octave qualified as Octave
import MusicTheory.Scale qualified as Scale

import Lilypond qualified as Ly
import Lilypond.Markup qualified as Ly.Markup

import Exercises.Chords
import Exercises.Util.ChordInversion (ChordInversion(..))

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
            , elems = concat [
                  majorTriads     (inversions [0])
                , majorSeventh    (inversions [0])
                , [Ly.SectionPageBreak]
                , dominantSeventh (inversions [0])
                , minorSeventh    (inversions [0])
                ]
            }
        , Ly.SectionSub $ Ly.Section{
              title = "Inversions"
            , intro = Just $ Ly.Markup.wordwrap $ mconcat [
                  "All chords are first shown in root position, "
                , "followed by all possible inversions."
                ]
            , elems = concat [
                  majorTriads     (inversions [0..2])
                , majorSeventh    (inversions [0..3])
                , [Ly.SectionPageBreak]
                , dominantSeventh (inversions [0..3])
                , [Ly.SectionPageBreak]
                , minorSeventh    (inversions [0..3])
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

majorTriads :: [ChordInversion] -> [Ly.SectionElem]
majorTriads inversions =
    chordExercise Scale.Major $
      mkExercise "Major triad" Chord.MajorTriad inversions

majorSeventh :: [ChordInversion] -> [Ly.SectionElem]
majorSeventh inversions =
    chordExercise Scale.Major $
      mkExercise "Major seventh" Chord.Major7 inversions

dominantSeventh :: [ChordInversion] -> [Ly.SectionElem]
dominantSeventh inversions =
    chordExercise Scale.Major $
      mkExercise "Dominant seventh" Chord.Dominant7 inversions

minorSeventh :: [ChordInversion] -> [Ly.SectionElem]
minorSeventh inversions =
    chordExercise Scale.Minor $
      mkExercise "Minor seventh" Chord.Minor7 inversions

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

mkExercise :: String -> Chord.Type -> [ChordInversion] -> ChordExercise
mkExercise title chordType inversions = ChordExercise{
      title
    , intro          = Nothing
    , clef           = Ly.ClefTreble
    , voicing        = Voicing.Default
    , startingOctave = Octave.middle
    , adjustOctave   = \_ -> Just noOctaveShift
    , numInversions  = length inversions
    , inversionsFor  = \_ -> inversions
    , chordType
    }