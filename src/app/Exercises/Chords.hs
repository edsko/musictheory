-- | Utilities for generating chord exercises
module Exercises.Chords (
    chordExercise
  ) where

import Data.Default

import MusicTheory
import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Named qualified as Chord.Named
import MusicTheory.Chord.Named qualified as Named (Chord)
import MusicTheory.Note.Octave qualified as Octave
import MusicTheory.Reference
import MusicTheory.Scale (Scale)
import MusicTheory.Scale qualified as Scale

import Lilypond qualified as Ly

{-------------------------------------------------------------------------------
  Construct chords exercise

  We show these chords as the /I/ chord in all /major/ scales.
-------------------------------------------------------------------------------}

chordExercise ::
     Chord.Type
     -- ^ Chord type
  -> [(Inversion, OctaveShift)]
     -- ^ Inversions to show for each chord
     --
     -- For each inversion, we also allow for an octave shift, to ensure that
     -- the inversions don't result in chords too high up the stave.
  -> Ly.ScoreElem
chordExercise typ inversions =
    Ly.ScoreStaff props $ mconcat [
        chordsOfTypeIn typ inversions firstHalf
      , [Ly.StaffLinebreak]
      , chordsOfTypeIn typ inversions secondHalf
      ]
  where
    props :: Ly.StaffProps
    props = def{
          Ly.hideTimeSignature  = True
        , Ly.omitMeasureNumbers = True
        }

chordsOfTypeIn ::
     Chord.Type
  -> [(Inversion, OctaveShift)]
  -> [Scale.Root]
  -> [Ly.StaffElem]
chordsOfTypeIn chordType inversions scales =
    concatMap goScale scales
  where
    -- Show all inversions for the specified scale
    --
    -- We show the chord name only once
    goScale :: Scale.Root -> [Ly.StaffElem]
    goScale scaleRoot =
        zipWith
          (goInversion $ wrtScale Octave.middle scale chord)
          inversions
          (True : repeat False)
      where
        -- .. always use a major scale as context
        scale :: Scale
        scale = Scale.named $ Scale.Name scaleRoot Scale.Major

        -- .. and always show the I chord in that scale
        chord :: Named.Chord Relative
        chord = Chord.Named.chordI chordType

    goInversion ::
         Named.Chord Absolute
      -> (Inversion, OctaveShift)
      -> Bool  -- Show chord name?
      -> Ly.StaffElem
    goInversion chord' (inversion, octaveShift) showChordName =
        if showChordName
          then Ly.StaffNamedChord                         chord''  duration
          else Ly.StaffUnnamedChord (Chord.Named.getNotes chord'') duration
      where
        chord'' :: Named.Chord Absolute
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
--
-- We repeat "C" at the end for nice symmetry.
firstHalf, secondHalf :: [Scale.Root]
firstHalf  = [ "C"  , "G"  , "D"  , "A"  , "E"  , "B" , "F♯" ]
secondHalf = [ "G♭" , "D♭" , "A♭" , "E♭" , "B♭" , "F" , "C"  ]
