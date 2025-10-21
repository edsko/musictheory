{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Module for use in ghci
module MusicTheory.Interactive where

import MusicTheory
import MusicTheory.Chord.Name qualified as Chord (Name(..))
import MusicTheory.Chord.Name qualified as Chord.Name
import MusicTheory.Chord.Named qualified as Chord.Named
import MusicTheory.Chord.Named qualified as Named (Chord(..))
import MusicTheory.Chord.Type qualified as Chord (Type(..))
import MusicTheory.Chord.Type qualified as Chord.Type
import MusicTheory.Chord.Unnamed qualified as Chord.Unnamed
import MusicTheory.Chord.Unnamed qualified as Unnamed (Chord(..))
import MusicTheory.Note (Note(Note))
import MusicTheory.Note qualified as Note
import MusicTheory.Progression (Progression(Progression))
import MusicTheory.Progression qualified as Progression
import MusicTheory.Progression.Name qualified as Progression (Name(..))
import MusicTheory.Progression.Name qualified as Progression.Name
import MusicTheory.Scale (Scale)
import MusicTheory.Scale qualified as Scale
