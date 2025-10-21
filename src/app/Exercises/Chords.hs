-- | Utilities for generating chord exercises
module Exercises.Chords (
    chordExercise
  ) where

import Data.Default

import MusicTheory.Chord qualified as Chord
import MusicTheory.Note  qualified as Note
import MusicTheory.Scale qualified as Scale

import Lilypond qualified as Ly
import MusicTheory

chordExercise ::
     Chord.Type
     -- ^ Chord type
  -> [(Chord.Inversion, Int)]
     -- ^ Inversions to show for each chord
     --
     -- For each inversion, we also allow for an octave shift, to ensure that
     -- the inversions don't result in chords too high up the stave.
  -> Ly.ScoreElem
chordExercise typ inversions = Ly.ScoreStaff props $ mconcat [
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
  -> [(Chord.Inversion, Int)]
  -> [Scale.Name]
  -> [Ly.StaffElem]
chordsOfTypeIn typ inversions scales = concat [
      -- Show the chord name only once
      zipWith
        (aux scale)
        ( Just (Chord.Name (Scale.rootNote scale) typ)
        : repeat Nothing
        )
        inversions
    | scale <- scales
    ]
  where
    aux ::
         Scale.Name
      -> Maybe Chord.Name
      -> (Chord.Inversion, Int)
      -> Ly.StaffElem
    aux scale mName (inversion, octaveShift) = Ly.StaffChord $ Ly.Chord{
          name  = mName
        , notes = transposeOctave octaveShift . Chord.invert inversion $
                    Chord.wrtMajorScale Note.middleOctave scale typ

          -- Make sure all inversions fit within a single measure
          --
          -- This ensures accidentals are not shown more than once.
        , duration = Ly.OneOver (fromIntegral $ length inversions)
        }

-- | Split the scales into two halves
--
-- We repeat "C" at the end for nice symmetry.
firstHalf, secondHalf :: [Scale.Name]
firstHalf  = [ "C"  , "G"  , "D"  , "A"  , "E"  , "B" , "F♯" ]
secondHalf = [ "G♭" , "D♭" , "A♭" , "E♭" , "B♭" , "F" , "C"  ]
