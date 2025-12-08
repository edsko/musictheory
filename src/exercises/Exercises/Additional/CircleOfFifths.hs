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
          triadsClockwise
        , triadsCounter
        , fourWayCloseClockwise
        , fourWayCloseCounter
        ]
    }

triadsClockwise :: Ly.SectionElem
triadsClockwise = Ly.SectionScore Ly.Score{
      title = Just "Clockwise major/minor triads"
    , intro = mempty
    , staff = Ly.Staff{
          props = staffProps
        , elems =
            alongCircleOfFifthsWith
              progression
              initInversions
              permissibleInversions
        }
    }
  where
    progression :: Progression Abs
    progression = Progression $ NE.fromList [
          Voicing.wrtScale scale Voicing.Default Octave.middle $
            Chord.Named.chordI chordType
        | scale     <- allMajor
        | chordType <- cycle [Chord.MajorTriad, Chord.MinorTriad]
        ]

    initInversions :: [ChordInversion]
    initInversions = [
          ChordInversion (Inversion 0) (OctaveShift 1) def
        , ChordInversion (Inversion 2) noOctaveShift   def
        , ChordInversion (Inversion 1) noOctaveShift   def
        ]

    permissibleInversions :: Chord.Type -> [Inversion]
    permissibleInversions _ = [Inversion i | i <- [0 .. 2]]

triadsCounter :: Ly.SectionElem
triadsCounter = Ly.SectionScore Ly.Score{
      title = Just "Counter-clockwise minor/major triads"
    , intro = mempty
    , staff = Ly.Staff{
          props = staffProps
        , elems =
            alongCircleOfFifthsWith
              progression
              initInversions
              permissibleInversions
        }
    }
  where
    progression :: Progression Abs
    progression = Progression $ NE.fromList [
          Voicing.wrtScale scale Voicing.Default Octave.middle $
            Chord.Named.chordI chordType
        | scale     <- List.rotate (-1) $ reverse allMajor
        | chordType <- cycle [Chord.MinorTriad, Chord.MajorTriad]
        ]

    initInversions :: [ChordInversion]
    initInversions = [
          ChordInversion (Inversion 0) noOctaveShift def
        , ChordInversion (Inversion 1) noOctaveShift def
        , ChordInversion (Inversion 2) noOctaveShift def
        ]

    permissibleInversions :: Chord.Type -> [Inversion]
    permissibleInversions _ = [Inversion i | i <- [0 .. 2]]

fourWayCloseClockwise :: Ly.SectionElem
fourWayCloseClockwise = Ly.SectionScore Ly.Score{
      title = Just "Clockwise dominant/minor7 chords"
    , intro = mempty
    , staff = Ly.Staff{
          props = staffProps
        , elems =
            alongCircleOfFifthsWith
              progression
              initInversions
              permissibleInversions
        }
    }
  where
    progression :: Progression Abs
    progression = Progression $ NE.fromList [
          Voicing.wrtScale scale Voicing.FourWayClose Octave.middle $
            Chord.Named.chordI chordType
        | scale     <- allMajor
        | chordType <- cycle [Chord.Dominant7, Chord.Minor7]
        ]

    initInversions :: [ChordInversion]
    initInversions = [
          ChordInversion (Inversion 2) (OctaveShift (-1)) def
        , ChordInversion (Inversion 0) noOctaveShift      def
        ]

    permissibleInversions :: Chord.Type -> [Inversion]
    permissibleInversions _ = [Inversion i | i <- [0, 2]]

fourWayCloseCounter :: Ly.SectionElem
fourWayCloseCounter = Ly.SectionScore Ly.Score{
      title = Just "Counter-clockwise minor7/dominant chords"
    , intro = mempty
    , staff = Ly.Staff{
          props = staffProps
        , elems =
            alongCircleOfFifthsWith
              progression
              initInversions
              permissibleInversions
        }
    }
  where
    progression :: Progression Abs
    progression = Progression $ NE.fromList [
          Voicing.wrtScale scale Voicing.FourWayClose Octave.middle $
            Chord.Named.chordI chordType
        | scale     <- List.rotate (-1) $ reverse allMajor
        | chordType <- cycle [Chord.Minor7, Chord.Dominant7]
        ]

    initInversions :: [ChordInversion]
    initInversions = [
          ChordInversion (Inversion 0) (OctaveShift 1) def
        , ChordInversion (Inversion 2) noOctaveShift   def
        ]

    permissibleInversions :: Chord.Type -> [Inversion]
    permissibleInversions _ = [Inversion i | i <- [0, 2]]

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

alongCircleOfFifthsWith ::
     Progression Abs
  -> [ChordInversion]
  -> (Chord.Type -> [Inversion])
  -> [Ly.StaffElem]
alongCircleOfFifthsWith progression initInversions permissibleInversions =
    List.intercalate [Ly.StaffLinebreak] $ map goInitInversion initInversions
  where
    -- .. for each choice of initial inversion
    goInitInversion :: ChordInversion -> [Ly.StaffElem]
    goInitInversion initInversion =
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

allMajor :: [Scale]
allMajor = [
      Scale.named (Scale.Name root Scale.Major)
    | root <- Scale.defaultRoots Scale.Major
    ]

staffProps :: Ly.StaffProps
staffProps = def{
      Ly.hideTimeSignature  = True
    , Ly.omitMeasureNumbers = True
    }
