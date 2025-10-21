-- | Utilities for generating chord exercises
module Exercises.Chords (
    chordExercise
  ) where

import Data.Default

import MusicTheory
import MusicTheory.Chord.Named qualified as Chord.Named
import MusicTheory.Chord.Named qualified as Named (Chord(..))
import MusicTheory.Chord.Type qualified as Chord (Type)
import MusicTheory.Note qualified as Note
import MusicTheory.Scale qualified as Scale

import Lilypond qualified as Ly

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
  -> [Scale.Name]
  -> [Ly.StaffElem]
chordsOfTypeIn typ inversions scales = concat [
      -- Show the chord name only once
      zipWith (aux scale) (True : repeat False) inversions
    | scale <- scales
    ]
  where
    aux ::
         Scale.Name
      -> Bool       -- ^ Show the chord name
      -> (Inversion, OctaveShift)
      -> Ly.StaffElem
    aux scale showChordName (inversion, octaveShift) =
        if showChordName
          then Ly.StaffNamedChord   chord       duration
          else Ly.StaffUnnamedChord chord.notes duration
      where
        chord :: Named.Chord
        chord = transposeOctave octaveShift . invert inversion $
                  Chord.Named.wrtMajorScale Note.middleOctave scale typ

        -- Make sure all inversions fit within a single measure
        --
        -- This ensures accidentals are not shown more than once.
        duration :: Ly.Duration
        duration = Ly.OneOver (fromIntegral $ length inversions)

-- | Split the scales into two halves
--
-- We repeat "C" at the end for nice symmetry.
firstHalf, secondHalf :: [Scale.Name]
firstHalf  = [ "C"  , "G"  , "D"  , "A"  , "E"  , "B" , "F♯" ]
secondHalf = [ "G♭" , "D♭" , "A♭" , "E♭" , "B♭" , "F" , "C"  ]
