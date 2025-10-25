{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Module for use in ghci
--
-- Some things to try:
--
-- > Scale.named (Scale.Name "G♭" Scale.Major)
-- > Voicing.wrtScale Scale.cMajor Voicing.StdJazz Octave.middle $ Chord.Named.chordI Chord.Dominant7
-- > Progression.wrtScale Scale.cMinor Voicing.StdJazz Octave.middle $ Progression.named Progression.Minor251
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
import MusicTheory.Chord.Voicing (Voicing)
import MusicTheory.Chord.Voicing qualified as Voicing
import MusicTheory.Interval qualified as Interval
import MusicTheory.Interval qualified as Interval
import MusicTheory.Note (Note(Note))
import MusicTheory.Note qualified as Note
import MusicTheory.Note.Octave (Octave(..))
import MusicTheory.Note.Octave qualified as Octave
import MusicTheory.Progression (Progression(..))
import MusicTheory.Progression qualified as Progression
import MusicTheory.Reference
import MusicTheory.Scale (Scale(..))
import MusicTheory.Scale qualified as Scale
