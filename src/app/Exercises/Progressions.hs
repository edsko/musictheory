module Exercises.Progressions (
    progressionExercise
  ) where

import Data.Default
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE

import MusicTheory
import MusicTheory.Chord.Named qualified as Named (Chord)
import MusicTheory.Note.Octave qualified as Octave
import MusicTheory.Progression (Progression(..))
import MusicTheory.Progression qualified as Progression
import MusicTheory.Reference
import MusicTheory.Scale (Scale)

import Lilypond qualified as Ly

{-------------------------------------------------------------------------------
  Construct chord progression exercise
-------------------------------------------------------------------------------}

progressionExercise ::
     Progression Relative
     -- ^ Progression type
  -> [(Inversion, OctaveShift)]
     -- ^ Inversion and octave shift for initial chord
  -> [Inversion]
     -- ^ Permissable inversions (for voice leading)
  -> [Scale]
     -- ^ Scales to show the progression in
  -> Ly.ScoreElem
progressionExercise progression initInversions permissibleInversions scales =
    Ly.ScoreStaff props $
      concatMap goScale scales
  where
    props :: Ly.StaffProps
    props = def{
          Ly.hideTimeSignature  = True
        , Ly.omitMeasureNumbers = True
        }

    -- .. for each scale
    goScale :: Scale -> [Ly.StaffElem]
    goScale scale = concat [
          concatMap (goInitInversion progression') initInversions
        , [Ly.StaffLinebreak]
        ]
      where
        progression' :: Progression Absolute
        progression' = wrtScale Octave.middle scale progression

    -- .. and for each choice of initial inversion
    goInitInversion ::
         Progression Absolute
      -> (Inversion, OctaveShift)
      -> [Ly.StaffElem]
    goInitInversion progression' (initInversion, initOctaveShift) =
        map goChord (NE.toList withVoiceLeading)
      where
        Progression (first :| rest) = progression'

        first' :: Named.Chord Absolute
        first' = transposeOctave initOctaveShift $ invert initInversion first

        withVoiceLeading :: NonEmpty (Named.Chord Absolute)
        Progression withVoiceLeading =
            Progression.voiceLeading permissibleInversions $
              Progression (first' :| rest)

    goChord :: Named.Chord Absolute -> Ly.StaffElem
    goChord chord = Ly.StaffNamedChord chord (Ly.OneOver 1)
