{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Module for use in ghci
module MusicTheory.Interactive where

-- To re-generate this list:
--
-- grep -rIh '> import' src/musictheory/

import MusicTheory
import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Named qualified as Chord.Named
import MusicTheory.Chord.Named qualified as Named (Chord)
import MusicTheory.Chord.Unnamed qualified as Chord.Unnamed
import MusicTheory.Chord.Unnamed qualified as Unnamed (Chord(..))
import MusicTheory.Note (Note(Note))
import MusicTheory.Note qualified as Note
import MusicTheory.Note.Octave (Octave(..))
import MusicTheory.Note.Octave qualified as Octave
import MusicTheory.Progression (Progression(..))
import MusicTheory.Progression qualified as Progression
import MusicTheory.Reference
import MusicTheory.Scale qualified as Scale
