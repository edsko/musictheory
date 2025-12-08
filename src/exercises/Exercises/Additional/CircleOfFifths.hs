{-# LANGUAGE ParallelListComp #-}

module Exercises.Additional.CircleOfFifths (exercises) where

import Data.Default
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE

import MusicTheory
import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Named qualified as Chord.Named
import MusicTheory.Chord.Named qualified as Named (Chord(..))
import MusicTheory.Chord.Voicing qualified as Voicing
import MusicTheory.Note.Octave qualified as Octave
import MusicTheory.Progression (Progression(..))
import MusicTheory.Progression qualified as Progression
import MusicTheory.Reference
import MusicTheory.Scale (Scale(..))
import MusicTheory.Scale qualified as Scale
import MusicTheory.Util.List qualified as List

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
      title = "Along the circle of fifths"
    , intro = mempty
    , elems = [
          triads
        , fourWayClose
        ]
    }

triads :: Ly.SectionElem
triads = Ly.SectionScore Ly.Score{
      title = Just "Minor/major triads"
    , intro = mempty
    , staff = Ly.Staff{
          props = staffProps
        , elems =
            consecutiveProgressions
              initInversions
              [progression1, progression2]
              permissibleInversions
        }
    }
  where
    mkChord :: Scale -> Chord.Type -> Named.Chord Abs
    mkChord scale chordType =
        Voicing.wrtScale scale Voicing.Default Octave.middle $
          Chord.Named.chordI chordType

    progression1 :: Progression Abs
    progression1 = Progression $ NE.fromList [
          mkChord scale chordType
        | scale     <- counterclockwise
        | chordType <- cycle [Chord.MinorTriad, Chord.MajorTriad]
        ]

    progression2 :: Progression Abs
    progression2 = Progression $ NE.fromList [
          mkChord scale chordType
        | scale     <- counterclockwise
        | chordType <- cycle [Chord.MajorTriad, Chord.MinorTriad]
        ]

    initInversions :: [ChordInversion]
    initInversions = [
          ChordInversion (Inversion 0) noOctaveShift def
        , ChordInversion (Inversion 1) noOctaveShift def
        , ChordInversion (Inversion 2) noOctaveShift def
        ]

    permissibleInversions :: Chord.Type -> [Inversion]
    permissibleInversions _ = [Inversion i | i <- [0 .. 2]]

fourWayClose :: Ly.SectionElem
fourWayClose = Ly.SectionScore Ly.Score{
      title = Just "Minor7/dominant chords, using four-way close voicing"
    , intro = mempty
    , staff = Ly.Staff{
          props = staffProps
        , elems =
            consecutiveProgressions
              initInversions
              [progression1, progression2]
              permissibleInversions
        }
    }
  where
    mkChord :: Scale -> Chord.Type -> Named.Chord Abs
    mkChord scale chordType =
        Voicing.wrtScale scale Voicing.FourWayClose Octave.middle $
          Chord.Named.chordI chordType

    progression1 :: Progression Abs
    progression1 = Progression $ NE.fromList [
          mkChord scale chordType
        | scale     <- counterclockwise
        | chordType <- cycle [Chord.Minor7, Chord.Dominant7]
        ]

    progression2 :: Progression Abs
    progression2 = Progression $ NE.fromList [
          mkChord scale chordType
        | scale     <- counterclockwise
        | chordType <- cycle [Chord.Dominant7, Chord.Minor7]
        ]

    initInversions :: [ChordInversion]
    initInversions = [
          ChordInversion (Inversion 0) (OctaveShift 1) def
        , ChordInversion (Inversion 2) noOctaveShift   def
        ]

    permissibleInversions :: Chord.Type -> [Inversion]
    permissibleInversions _ = [Inversion i | i <- [0, 2]]

{-------------------------------------------------------------------------------
  Construct exercise
-------------------------------------------------------------------------------}

consecutiveProgressions ::
     [ChordInversion]
  -> [Progression Abs]
  -> (Chord.Type -> [Inversion])
  -> [Ly.StaffElem]
consecutiveProgressions initInversions progressions permissibleInversions =
    List.intercalate [Ly.StaffLinebreak] [
        go initInversion progression
      | initInversion <- initInversions
      , progression   <- progressions
      ]
  where
    go :: ChordInversion -> Progression Abs -> [Ly.StaffElem]
    go initInversion progression =
        map goChord (NE.toList withVoiceLeading)
      where
        withVoiceLeading :: NonEmpty (Named.Chord 'Abs)
        Progression withVoiceLeading =
            Progression.voiceLeading permissibleInversions $
              Progression.mapFirst
                (ChordInversion.apply initInversion)
                progression

    goChord :: Named.Chord Abs -> Ly.StaffElem
    goChord chord = Ly.StaffChord Ly.Chord{
          notes      = Chord.Named.getNotes chord
        , duration   = Ly.OneOver 1
        , name       = Just $ Chord.Named.getName chord
        , annotation = Ly.NoAnnotation
        , simplify   = True
        }

{-------------------------------------------------------------------------------
  Internal auxiliary
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
      Ly.hideTimeSignature  = True
    , Ly.omitMeasureNumbers = True
    }
