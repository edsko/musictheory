-- | Utilities for generating chord exercises
module Exercises.Chords (
    -- * Construct exercise
    ChordInversion(..)
  , chordExercise
  ) where

import Data.Default

import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Named qualified as Chord.Named
import MusicTheory.Chord.Named qualified as Named (Chord)
import MusicTheory.Chord.Voicing (Voicing)
import MusicTheory.Chord.Voicing qualified as Voicing
import MusicTheory.Note.Octave qualified as Octave
import MusicTheory.Reference
import MusicTheory.Scale (Scale)
import MusicTheory.Scale qualified as Scale

import Lilypond qualified as Ly

import Exercises.Util.ChordInversion (ChordInversion(..))
import Exercises.Util.ChordInversion qualified as ChordInversion

{-------------------------------------------------------------------------------
  Construct chords exercise

  We show these chords as the /I/ chord in all /major/ scales.
-------------------------------------------------------------------------------}

chordExercise ::
     Chord.Type
  -> Voicing
  -> Int -- ^ Number of inversions shown for each chord
  -> (Scale.Root -> [ChordInversion])
  -> Ly.Staff
chordExercise typ voicing numInversions inversions =
    Ly.Staff{
        props = def{
            Ly.hideTimeSignature  = True
          , Ly.omitMeasureNumbers = True
          , Ly.timeSignature      = Ly.TimeSignature numInversions 1
          }
      , elems = mconcat [
            chordsOfTypeIn typ voicing inversions firstHalf
          , [Ly.StaffLinebreak]
          , chordsOfTypeIn typ voicing inversions secondHalf
          ]
      }

chordsOfTypeIn ::
     Chord.Type
  -> Voicing
  -> (Scale.Root -> [ChordInversion])
  -> [Scale.Root]
  -> [Ly.StaffElem]
chordsOfTypeIn chordType voicing inversionsFor scales =
    concatMap goScale scales
  where
    -- Show all inversions for the specified scale
    --
    -- We show the chord name only once
    goScale :: Scale.Root -> [Ly.StaffElem]
    goScale scaleRoot =
        zipWith
          (goInversion $ Voicing.wrtScale scale voicing Octave.middle chord)
          (inversionsFor scaleRoot)
          (True : repeat False)
      where
        -- .. always use a major scale as context
        scale :: Scale
        scale = Scale.named $ Scale.Name scaleRoot Scale.Major

        -- .. and always show the I chord in that scale
        chord :: Named.Chord Rel
        chord = Chord.Named.chordI chordType

    goInversion ::
         Named.Chord Abs
      -> ChordInversion
      -> Bool  -- Show chord name?
      -> Ly.StaffElem
    goInversion chord' chordInversion showChordName =
        if showChordName
          then Ly.StaffNamedChord                         chord''  duration ann
          else Ly.StaffUnnamedChord (Chord.Named.getNotes chord'') duration ann
      where
        ChordInversion{annotation = ann} = chordInversion

        chord'' :: Named.Chord Abs
        chord'' = ChordInversion.apply chordInversion chord'

    duration :: Ly.Duration
    duration = Ly.OneOver 1

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Split the scales into two halves
firstHalf, secondHalf :: [Scale.Root]
firstHalf  = take 6 Scale.allMajorRoots
secondHalf = drop 6 Scale.allMajorRoots
