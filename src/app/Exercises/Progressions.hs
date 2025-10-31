module Exercises.Progressions (
    progressionExercise
  ) where

import Data.Default
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE

import MusicTheory
import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Named qualified as Chord.Named
import MusicTheory.Chord.Named qualified as Named (Chord(..))
import MusicTheory.Chord.Voicing (Voicing)
import MusicTheory.Note.Octave qualified as Octave
import MusicTheory.Progression (Progression(..))
import MusicTheory.Progression qualified as Progression
import MusicTheory.Reference
import MusicTheory.Scale (Scale(..))
import MusicTheory.Scale qualified as Scale

import Lilypond qualified as Ly

import Exercises.Util.ChordInversion (ChordInversion(..))
import Exercises.Util.ChordInversion qualified as ChordInversion

{-------------------------------------------------------------------------------
  Construct chord progression exercise
-------------------------------------------------------------------------------}

progressionExercise ::
     Progression Rel
     -- ^ Progression type
  -> Voicing
     -- ^ Chord voicing style
  -> (Scale.Root -> [ChordInversion])
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
        , concatMap (goInitInversion progression') (initInv scale.name.root)
        , [Ly.StaffLinebreak]
        ]
      where
        progression' :: Progression Abs
        progression' =
            Progression.wrtScale scale voicing Octave.middle progression

    -- .. and for each choice of initial inversion
    goInitInversion ::
         Progression Abs
      -> ChordInversion
      -> [Ly.StaffElem]
    goInitInversion progression' initInversion =
        zipWith
          goChord
          (NE.toList withVoiceLeading)
          (initInversion.annotation : repeat Ly.NoAnnotation)
      where
        withVoiceLeading :: NonEmpty (Named.Chord Abs)
        Progression withVoiceLeading =
            Progression.voiceLeading permissiveInv $
              Progression.mapFirst
                (ChordInversion.apply initInversion)
                progression'

    goChord :: Named.Chord Abs -> Ly.Annotation -> Ly.StaffElem
    goChord chord ann = Ly.StaffChord Ly.Chord{
        notes      = Chord.Named.getNotes chord
      , duration   = Ly.OneOver 1
      , name       = Just $ Chord.Named.getName chord
      , annotation = ann
      }
