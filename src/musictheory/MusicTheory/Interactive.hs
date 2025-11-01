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

{-------------------------------------------------------------------------------
  Examples

  Written so that they can easily be copy-pasted. For multiple lines, use

  > :{
  >   ..
  > :}
-------------------------------------------------------------------------------}

-- | Inspect scale:
example1 :: Scale
example1 =
    Scale.named (Scale.Name "G♭" Scale.Major)

-- | Voice chord in given scale
example2 :: Named.Chord Abs
example2 =
    Voicing.wrtScale Scale.cMajor Voicing.FourWayClose Octave.middle $
      Chord.Named.chordI Chord.Dominant7

-- | Voice chord progression in given scale
example3 :: Progression Abs
example3 =
    Progression.wrtScale Scale.cMinor Voicing.FourWayClose Octave.middle $
      Progression.named $ Progression.Minor251 Progression.WithSevenFlat9

-- | Apply voice leading
example4 :: Progression Abs
example4 =
    let permissibleInversions :: Chord.Type -> [Inversion]
        permissibleInversions = \case
            Chord.SevenFlat9 -> [Inversion 0, Inversion 2]
            Chord.Minor7     -> [Inversion 0, Inversion 2]
            _ -> undefined
    in Progression.voiceLeading permissibleInversions $
         Progression.mapFirst (invert $ Inversion 1) $
           Progression.wrtScale Scale.aMinor Voicing.FourWayClose Octave.middle $
             Progression.named $ Progression.Minor251 Progression.WithSevenFlat9

-- | Move to playable range
example5 :: Maybe (Named.Chord Abs)
example5 =
    let chord :: Named.Chord Abs
        chord = Voicing.wrtScale Scale.cMajor Voicing.FourWayClose(Octave.middle) $
                  Chord.Named.chordI Chord.Major7
    in (\shift -> transposeOctave shift chord) <$>
          Chord.Named.moveToRange ("D3", "G4") chord
