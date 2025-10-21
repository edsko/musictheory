module Exercises.Progressions (
    progressionExercise
  ) where

import Data.Default
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE

import MusicTheory
import MusicTheory.Chord.Named qualified as Named (Chord(..))
import MusicTheory.Note qualified as Note
import MusicTheory.Progression (Progression(Progression))
import MusicTheory.Progression qualified as Progression
import MusicTheory.Progression.Name qualified as Progression (Name)
import MusicTheory.Scale qualified as Scale

import Lilypond qualified as Ly

progressionExercise ::
     Progression.Name
     -- ^ Progression type
  -> [(Inversion, OctaveShift)]
     -- ^ Inversion and octave shift for initial chord
  -> [Inversion]
     -- ^ Permissable inversions (for voice leading)
  -> Ly.ScoreElem
progressionExercise typ initInversions permissibleInversions =
    Ly.ScoreStaff props $
      concatMap goScale Scale.scaleNames
  where
    props :: Ly.StaffProps
    props = def{
          Ly.hideTimeSignature  = True
        , Ly.omitMeasureNumbers = True
        }

    -- .. for each scale
    goScale :: Scale.Name -> [Ly.StaffElem]
    goScale scale = concat [
          concatMap (goInitInversion progression) initInversions
        , [Ly.StaffLinebreak]
        ]
      where
        progression :: Progression
        progression = Progression.wrtMajorScale Note.middleOctave scale typ

    -- .. and for each choice of initial inversion
    goInitInversion :: Progression -> (Inversion, OctaveShift) -> [Ly.StaffElem]
    goInitInversion progression (initInversion, initOctaveShift) =
        map goChord (NE.toList withVoiceLeading)
      where
        Progression (first :| rest) = progression

        first' :: Named.Chord
        first' = transposeOctave initOctaveShift $ invert initInversion first

        withVoiceLeading :: NonEmpty Named.Chord
        Progression withVoiceLeading =
            Progression.voiceLeading permissibleInversions $
              Progression (first' :| rest)

    goChord :: Named.Chord -> Ly.StaffElem
    goChord chord = Ly.StaffNamedChord chord (Ly.OneOver 1)
