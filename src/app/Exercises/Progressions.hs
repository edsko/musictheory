-- | Utilities for generating chord progression exercises
--
-- Intended for qualified import.
--
-- > import Exercises.Progressions qualified as Progressions
module Exercises.Progressions (
    Setup(..)
  , Exercise(..)
  , exercise
  ) where

import Data.Default
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE

import MusicTheory
import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Named qualified as Chord.Named
import MusicTheory.Chord.Named qualified as Named (Chord(..))
import MusicTheory.Chord.Voicing (Voicing)
import MusicTheory.Note (Note)
import MusicTheory.Note qualified as Note
import MusicTheory.Note.Octave qualified as Octave
import MusicTheory.Progression (Progression(..))
import MusicTheory.Progression qualified as Progression
import MusicTheory.Reference
import MusicTheory.Scale (Scale(..))
import MusicTheory.Scale qualified as Scale

import Lilypond qualified as Ly
import Lilypond.Markup qualified as Ly (Markup)

import Exercises.Util.ChordInversion (ChordInversion(..))
import Exercises.Util.ChordInversion qualified as ChordInversion

{-------------------------------------------------------------------------------
  Construct chord progression exercise
-------------------------------------------------------------------------------}

data Setup = Setup{
      title :: String
    , intro :: Maybe Ly.Markup
    }

data Exercise = Exercise{
      progressionName :: Progression.Name
    , voicing         :: Voicing

      -- | Inversion for initial chord
    , startingInversion :: Scale.Root -> [ChordInversion]

      -- | Inversions for voice leading
    , permissibleInversions :: Chord.Type -> [Inversion]
    }

exercise :: Scale.Type -> Setup -> Exercise -> [Ly.SectionElem]
exercise scaleType setup ex = [
      Ly.SectionPageBreak
    , Ly.SectionScore Ly.Score{
          title = Just setup.title
        , intro = setup.intro
        , staff = Ly.Staff{
              props = staffProps
            , elems = exerciseIn scaleType ex scales
            }
        }
    ]
  where
    staffProps :: Ly.StaffProps
    staffProps = def{
          Ly.hideTimeSignature  = True
        , Ly.omitMeasureNumbers = True
        }

    scales :: [Scale.Root]
    scales = Scale.defaultRoots scaleType

exerciseIn :: Scale.Type -> Exercise -> [Scale.Root] -> [Ly.StaffElem]
exerciseIn scaleType ex scales =
    concatMap goScale scales
  where
    -- .. for each scale
    goScale :: Scale.Root -> [Ly.StaffElem]
    goScale scaleRoot = concat [
          [Ly.StaffKeySignature scale.name]
        , concatMap (goInitInversion prog') $
            ex.startingInversion scale.name.root
        , [Ly.StaffLinebreak]
        ]
      where
        scale :: Scale
        scale = Scale.named $ Scale.Name scaleRoot scaleType

        prog :: Progression Rel
        prog = Progression.named ex.progressionName

        prog' :: Progression Abs
        prog' = Progression.wrtScale scale ex.voicing Octave.middle prog

    -- .. and for each choice of initial inversion
    goInitInversion :: Progression Abs -> ChordInversion -> [Ly.StaffElem]
    goInitInversion prog initInversion =
        zipWith goChord
          (NE.toList withVoiceLeading)
          (initInversion.annotation : repeat Ly.NoAnnotation)
      where
        withVoiceLeading :: NonEmpty (Named.Chord Abs)
        Progression withVoiceLeading =
            Progression.voiceLeading ex.permissibleInversions $
              Progression.mapFirst
                (ChordInversion.apply initInversion)
                prog

    goChord :: Named.Chord Abs -> Ly.Annotation -> Ly.StaffElem
    goChord chord ann =
        checkKnownChord scaleType (Chord.Named.getRoot chord).note $
          Ly.StaffChord Ly.Chord{
              notes      = Chord.Named.getNotes chord
            , duration   = Ly.OneOver 1
            , name       = Just $ Chord.Named.getName chord
            , annotation = ann
            }

-- | Chords in a prog should also be shown in the chords exercises
checkKnownChord :: Scale.Type -> Note -> a -> a
checkKnownChord scaleType chordRoot x =
    if chordRoot `elem` allKnown
      then x
      else error $ "Unknown chord: " ++ show (scaleType, chordRoot)
  where
    allKnown :: [Note]
    allKnown = map Scale.rootNote $ concat [
          Scale.defaultRoots scaleType
        , Scale.enharmonicRoots scaleType
        ]
