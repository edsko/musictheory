{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE ParallelListComp #-}

module Exercises.Additional.CircleOfFifths (exercises) where

import Data.Default
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)

import MusicTheory
import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Named qualified as Chord.Named
import MusicTheory.Chord.Named qualified as Named (Chord(..))
import MusicTheory.Chord.Voicing qualified as Voicing
import MusicTheory.Note.Octave qualified as Octave
import MusicTheory.Progression (ProgressionF(..))
import MusicTheory.Progression qualified as Progression
import MusicTheory.Reference
import MusicTheory.Scale (Scale(..))
import MusicTheory.Scale qualified as Scale
import MusicTheory.Util.Foldable qualified as Foldable
import MusicTheory.Util.List qualified as List
import MusicTheory.Util.Stream qualified as Stream

import Lilypond qualified as Ly

import Construction.Util.ChordInversion (ChordInversion(..))
import Construction.Util.ChordInversion qualified as ChordInversion

{-------------------------------------------------------------------------------
  List of exercises
-------------------------------------------------------------------------------}

exercises :: [Ly.Section]
exercises = [
      alongCircleOfFifths
    ]

{-------------------------------------------------------------------------------
  Major/minor along the circle of fifths
-------------------------------------------------------------------------------}

alongCircleOfFifths :: Ly.Section
alongCircleOfFifths = Ly.Section{
      title = "Minor/major anti-clockwise along the circle of fifths"
    , intro = mempty
    , elems = [
          triads
        , Ly.SectionPageBreak
        , sevenths
        , Ly.SectionPageBreak
        , fourWayClose
        ]
    }

triads :: Ly.SectionElem
triads = Ly.SectionScore Ly.Score{
      title = Just "Triads"
    , intro = mempty
    , staff = Ly.Staff{
          props = staffProps
        , elems =
            consecutiveProgressions
              initInversions
              progressions
              permissibleInversions
        }
    }
  where
    initInversions :: [ChordInversion]
    initInversions = [
          ChordInversion (Inversion 0) noOctaveShift def
        , ChordInversion (Inversion 1) noOctaveShift def
        , ChordInversion (Inversion 2) noOctaveShift def
        ]

    progressions :: [ProgressionF MultipleChordsPerMeasure Abs]
    progressions = [
          mkProgression Voicing.Default [Chord.MinorTriad, Chord.MajorTriad]
        , mkProgression Voicing.Default [Chord.MajorTriad, Chord.MinorTriad]
        ]

    permissibleInversions :: Chord.Type -> [Inversion]
    permissibleInversions _ = [Inversion i | i <- [0 .. 2]]

sevenths :: Ly.SectionElem
sevenths = Ly.SectionScore Ly.Score{
      title = Just "Minor7/dominant chords"
    , intro = mempty
    , staff = Ly.Staff{
          props = staffProps
        , elems =
            consecutiveProgressions
              initInversions
              progressions
              permissibleInversions
        }
    }
  where
    initInversions :: [ChordInversion]
    initInversions = [
          ChordInversion (Inversion 0) (OctaveShift 1) def
        , ChordInversion (Inversion 3) noOctaveShift   def
        , ChordInversion (Inversion 2) noOctaveShift   def
        , ChordInversion (Inversion 1) noOctaveShift   def
        ]

    progressions :: [ProgressionF MultipleChordsPerMeasure Abs]
    progressions = [
          mkProgression Voicing.Default [Chord.Minor7, Chord.Dominant7]
        , mkProgression Voicing.Default [Chord.Dominant7, Chord.Minor7]
        ]

    permissibleInversions :: Chord.Type -> [Inversion]
    permissibleInversions _ = [Inversion i | i <- [0 .. 3]]

fourWayClose :: Ly.SectionElem
fourWayClose = Ly.SectionScore Ly.Score{
      title = Just "Minor7/dominant chords, using four-way close voicing"
    , intro = mempty
    , staff = Ly.Staff{
          props = staffProps
        , elems =
            consecutiveProgressions
              initInversions
              progressions
              permissibleInversions
        }
    }
  where
    initInversions :: [ChordInversion]
    initInversions = [
          ChordInversion (Inversion 0) (OctaveShift 1) def
        , ChordInversion (Inversion 2) noOctaveShift   def
        ]

    progressions :: [ProgressionF MultipleChordsPerMeasure Abs]
    progressions = [
          mkProgression Voicing.FourWayClose [Chord.Minor7, Chord.Dominant7]
        , mkProgression Voicing.FourWayClose [Chord.Dominant7, Chord.Minor7]
        ]

    permissibleInversions :: Chord.Type -> [Inversion]
    permissibleInversions _ = [Inversion i | i <- [0, 2]]

{-------------------------------------------------------------------------------
  Construct exercise
-------------------------------------------------------------------------------}

consecutiveProgressions ::
     [ChordInversion]
  -> [ProgressionF MultipleChordsPerMeasure Abs]
  -> (Chord.Type -> [Inversion])
  -> [Ly.StaffElem]
consecutiveProgressions initInversions progressions permissibleInversions =
    List.intercalate [Ly.StaffLinebreak] [
        go initInversion progression
      | initInversion <- initInversions
      , progression   <- progressions
      ]
  where
    go :: ChordInversion
      -> ProgressionF MultipleChordsPerMeasure Abs
      -> [Ly.StaffElem]
    go initInversion progression =
        concatMap goMeasure withVoiceLeading
      where
        withVoiceLeading :: [[Named.Chord 'Abs]]
        Progression (MultipleChordsPerMeasure withVoiceLeading) =
            Progression.voiceLeading permissibleInversions $
              Progression.mapFirst
                (ChordInversion.apply initInversion)
                progression

    goMeasure :: [Named.Chord Abs] -> [Ly.StaffElem]
    goMeasure measure = map (goChord $ Foldable.length measure) measure

    goChord :: Word -> Named.Chord Abs -> Ly.StaffElem
    goChord measureLen chord = Ly.StaffChord Ly.Chord{
          notes      = Chord.Named.getNotes chord
        , duration   = Ly.OneOver measureLen
        , name       = Just $ Chord.Named.getName chord
        , annotation = Ly.NoAnnotation
        , simplify   = True
        }

mkProgression ::
     Voicing.Voicing
  -> NonEmpty Chord.Type
  -> ProgressionF MultipleChordsPerMeasure Abs
mkProgression voicing chordTypes = multipleChordsPerMeasure [
      [ mkChord scale.value chordType
      | chordType <- Stream.take numChords chordTypes'
      ]
    | scale <- List.markElems counterclockwise
    , let numChords = if scale.isLast then 2 else 1
    | chordTypes' <- Stream.toList $ Stream.tails $ Stream.cycle chordTypes
    ]
  where
    mkChord :: Scale -> Chord.Type -> Named.Chord Abs
    mkChord scale chordType =
        Voicing.wrtScale scale voicing Octave.middle $
          Chord.Named.chordI chordType

{-------------------------------------------------------------------------------
  Auxiliary: progressions with multiple chords per measure

  This could conceivably live somewhere more general.
-------------------------------------------------------------------------------}

newtype MultipleChordsPerMeasure a =
    MultipleChordsPerMeasure [[a]]
  deriving stock (Functor, Foldable, Traversable)

multipleChordsPerMeasure ::
     [[Named.Chord Abs]]
  -> ProgressionF MultipleChordsPerMeasure Abs
multipleChordsPerMeasure = Progression . MultipleChordsPerMeasure

{-------------------------------------------------------------------------------
  Miscellaneous internal auxiliary
-------------------------------------------------------------------------------}

clockwise :: [Scale]
clockwise = [
      Scale.named (Scale.Name root Scale.Major)
    | root <- Scale.defaultRoots Scale.Major
    ]

counterclockwise :: [Scale]
counterclockwise = List.rotate (-1) $ reverse clockwise

staffProps :: Ly.StaffProps
staffProps = def{
      Ly.hideTimeSignature  =  True
    , Ly.omitMeasureNumbers = True
    }
