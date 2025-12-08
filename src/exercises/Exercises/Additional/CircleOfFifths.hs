module Exercises.Additional.CircleOfFifths (exercises) where

import Data.Default
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE

import MusicTheory
import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Named qualified as Chord.Named
import MusicTheory.Chord.Named qualified as Named (Chord(..))
import MusicTheory.Chord.Voicing (Voicing)
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
          Ly.SectionScore Ly.Score{
              title = Just "Clockwise major/minor triads"
            , intro = mempty
            , staff = Ly.Staff{
                  props = staffProps
                , elems =
                    let chordTypes = cycle [
                            Chord.MajorTriad
                          , Chord.MinorTriad
                          ]
                    in alongCircleOfFifthsWith
                         (zip allMajor chordTypes)
                         [ ChordInversion (Inversion 0) (OctaveShift 1) def
                         , ChordInversion (Inversion 2) noOctaveShift   def
                         , ChordInversion (Inversion 1) noOctaveShift   def
                         ]
                         (\_chordType -> [Inversion i | i <- [0 .. 2]])
                         Voicing.Default
                }
            }
        , Ly.SectionScore Ly.Score{
              title = Just "Counter-clockwise minor/major triads"
            , intro = mempty
            , staff = Ly.Staff{
                  props = staffProps
                , elems =
                    let chordTypes = cycle $ [
                            Chord.MinorTriad
                          , Chord.MajorTriad
                          ]
                    in alongCircleOfFifthsWith
                         (zip (List.rotate (-1) $ reverse allMajor) chordTypes)
                         [ ChordInversion (Inversion 0) noOctaveShift def
                         , ChordInversion (Inversion 1) noOctaveShift def
                         , ChordInversion (Inversion 2) noOctaveShift def
                         ]
                         (\_chordType -> [Inversion i | i <- [0 .. 2]])
                         Voicing.Default
                }
            }
        , Ly.SectionScore Ly.Score{
              title = Just "Clockwise dominant/minor7 chords"
            , intro = mempty
            , staff = Ly.Staff{
                  props = staffProps
                , elems =
                    let chordTypes = cycle [
                            Chord.Dominant7
                          , Chord.Minor7
                          ]
                    in alongCircleOfFifthsWith
                         (zip allMajor chordTypes)
                         [ ChordInversion (Inversion 2) (OctaveShift (-1)) def
                         , ChordInversion (Inversion 0) noOctaveShift      def
                         ]
                         (\_chordType -> [Inversion i | i <- [0, 2]])
                         Voicing.FourWayClose
                }
            }
        , Ly.SectionScore Ly.Score{
              title = Just "Counter-clockwise minor7/dominant chords"
            , intro = mempty
            , staff = Ly.Staff{
                  props = staffProps
                , elems =
                    let chordTypes = cycle [
                            Chord.Minor7
                          , Chord.Dominant7
                          ]
                    in alongCircleOfFifthsWith
                         (zip (List.rotate (-1) $ reverse allMajor) chordTypes)
                         [ ChordInversion (Inversion 0) (OctaveShift 1) def
                         , ChordInversion (Inversion 2) noOctaveShift   def
                         ]
                         (\_chordType -> [Inversion i | i <- [0, 2]])
                         Voicing.FourWayClose
                }
            }
        ]
    }
  where
    staffProps :: Ly.StaffProps
    staffProps = def{
          Ly.hideTimeSignature  = True
        , Ly.omitMeasureNumbers = True
        }

alongCircleOfFifthsWith ::
     [(Scale, Chord.Type)]
  -> [ChordInversion]
  -> (Chord.Type -> [Inversion])
  -> Voicing
  -> [Ly.StaffElem]
alongCircleOfFifthsWith scales initInversions permissibleInversions voicing =
    List.intercalate [Ly.StaffLinebreak] $ map goInitInversion initInversions
  where
    chords :: Progression Abs
    chords = Progression $ NE.fromList [
          Voicing.wrtScale scale voicing Octave.middle $
            Chord.Named.chordI chordType
        | (scale, chordType) <- scales
        ]

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
                chords

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

allMajor :: [Scale]
allMajor = [
      Scale.named (Scale.Name root Scale.Major)
    | root <- Scale.defaultRoots Scale.Major
    ]

{-
majorMinor, minorMajor :: [Scale]
majorMinor = List.alternate (List.odds allMajor) (List.evens allMinor)
minorMajor = List.alternate (List.odds allMinor) (List.evens allMajor)
-}