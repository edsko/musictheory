module Exercises.Progressions (
    progressionExercise
  ) where

import Data.Default
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE

import MusicTheory
import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Named qualified as Named (Chord)
import MusicTheory.Chord.Voicing (Voicing)
import MusicTheory.Note.Octave qualified as Octave
import MusicTheory.Progression (Progression(..))
import MusicTheory.Progression qualified as Progression
import MusicTheory.Reference
import MusicTheory.Scale (Scale(..))

import Lilypond qualified as Ly

{-------------------------------------------------------------------------------
  Construct chord progression exercise
-------------------------------------------------------------------------------}

progressionExercise ::
     Progression Rel
     -- ^ Progression type
  -> Voicing
     -- ^ Chord voicing style
  -> [(Inversion, OctaveShift)]
     -- ^ Inversion and octave shift for initial chord
  -> (Chord.Type -> [Inversion])
     -- ^ Permissable inversions (for voice leading)
  -> [Scale]
     -- ^ Scales to show the progression in
  -> Ly.Staff
progressionExercise progression voicing initInv permissiveInv scales =
    Ly.Staff{
        props = def{
            Ly.hideTimeSignature  = True
          , Ly.omitMeasureNumbers = True
          }
      , elems = concatMap goScale scales
      }
  where
    -- .. for each scale
    goScale :: Scale -> [Ly.StaffElem]
    goScale scale = concat [
          [Ly.StaffKeySignature scale.name]
        , concatMap (goInitInversion progression') initInv
        , [Ly.StaffLinebreak]
        ]
      where
        progression' :: Progression Abs
        progression' =
            Progression.wrtScale scale voicing Octave.middle progression

    -- .. and for each choice of initial inversion
    goInitInversion ::
         Progression Abs
      -> (Inversion, OctaveShift)
      -> [Ly.StaffElem]
    goInitInversion progression' (initInversion, initOctaveShift) =
        map goChord (NE.toList withVoiceLeading)
      where
        Progression (first :| rest) = progression'

        first' :: Named.Chord Abs
        first' = transposeOctave initOctaveShift $ invert initInversion first

        withVoiceLeading :: NonEmpty (Named.Chord Abs)
        Progression withVoiceLeading =
            Progression.voiceLeading permissiveInv $
              Progression (first' :| rest)

    goChord :: Named.Chord Abs -> Ly.StaffElem
    goChord chord = Ly.StaffNamedChord chord (Ly.OneOver 1)
