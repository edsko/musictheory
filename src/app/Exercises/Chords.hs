-- | Utilities for generating chord exercises
module Exercises.Chords (
    -- * Construct exercise
    chordExercise
  ) where

import Data.Default

import MusicTheory
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

import Exercises.Lilypond.Style qualified as Style

{-------------------------------------------------------------------------------
  Construct chords exercise

  We show these chords as the /I/ chord in all /major/ scales.
-------------------------------------------------------------------------------}

chordExercise ::
     Chord.Type
     -- ^ Chord type
  -> Voicing
     -- ^ Chord voicing style
  -> [(Inversion, OctaveShift)]
     -- ^ Inversions to show for each chord
     --
     -- For each inversion, we also allow for an octave shift, to ensure that
     -- the inversions don't result in chords too high up the stave.
  -> Ly.Staff Style.Class
chordExercise typ voicing inversions =
    Ly.Staff{
        props = def{
            Ly.hideTimeSignature  = True
          , Ly.omitMeasureNumbers = True
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
  -> [(Inversion, OctaveShift)]
  -> [Scale.Root]
  -> [Ly.StaffElem]
chordsOfTypeIn chordType voicing inversions scales =
    concatMap goScale scales
  where
    -- Show all inversions for the specified scale
    --
    -- We show the chord name only once
    goScale :: Scale.Root -> [Ly.StaffElem]
    goScale scaleRoot =
        zipWith
          (goInversion $ Voicing.wrtScale scale voicing Octave.middle chord)
          inversions
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
      -> (Inversion, OctaveShift)
      -> Bool  -- Show chord name?
      -> Ly.StaffElem
    goInversion chord' (inversion, octaveShift) showChordName =
        if showChordName
          then Ly.StaffNamedChord                         chord''  duration
          else Ly.StaffUnnamedChord (Chord.Named.getNotes chord'') duration
      where
        chord'' :: Named.Chord Abs
        chord'' = transposeOctave octaveShift $ invert inversion chord'

    -- Make sure all inversions fit within a single measure
    --
    -- This ensures accidentals are not shown more than once.
    duration :: Ly.Duration
    duration = Ly.OneOver (fromIntegral $ length inversions)

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Split the scales into two halves
firstHalf, secondHalf :: [Scale.Root]
firstHalf  = take 6 Scale.allMajorRoots
secondHalf = drop 6 Scale.allMajorRoots
