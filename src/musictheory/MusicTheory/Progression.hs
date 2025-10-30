{-# LANGUAGE OverloadedLists #-}

-- | Chord progressions
--
-- Intended for qualified import.
--
-- > import MusicTheory.Progression (Progression(..))
-- > import MusicTheory.Progression qualified as Progression
module MusicTheory.Progression (
    Progression(..)
    -- * Combinators
  , mapFirst
  , wrtScale
  , voiceLeading
    -- * Standard chord progressions
  , Name(..)
  , named
  ) where

import Data.List.NonEmpty (NonEmpty(..))

import MusicTheory
import MusicTheory.Chord qualified as Chord
import MusicTheory.Chord.Named qualified as Chord.Named
import MusicTheory.Chord.Named qualified as Named (Chord)
import MusicTheory.Chord.Voicing (Voicing)
import MusicTheory.Chord.Voicing qualified as Voicing
import MusicTheory.Note.Octave (Octave)
import MusicTheory.Reference
import MusicTheory.Scale (Scale)
import MusicTheory.Util

{-------------------------------------------------------------------------------
  Basic definitions
-------------------------------------------------------------------------------}

newtype Progression r = Progression (NonEmpty (Named.Chord r))
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Combinators
-------------------------------------------------------------------------------}

mapFirst :: (Named.Chord r -> Named.Chord r) -> Progression r -> Progression r
mapFirst f (Progression (c :| cs)) = Progression (f c :| cs)

-- | Choose chord voicings
wrtScale :: Scale -> Voicing -> Octave -> Progression Rel -> Progression Abs
wrtScale scale voicing octave (Progression chords) = Progression $
    Voicing.wrtScale scale voicing octave <$> chords

-- | Choose inversions to minimize distance between successive chords
--
-- Fails if there is no unique solution.
voiceLeading ::
     (Chord.Type -> [Inversion]) -- ^ Permissible inversions
  -> Progression Abs -> Progression Abs
voiceLeading permissibleInversions = \(Progression chords) -> Progression $
    case chords of
      c :| cs -> c :| go c cs
  where
    go ::
         Named.Chord Abs  -- Previous chord (for voice leading)
      -> [Named.Chord Abs] -> [Named.Chord Abs]
    go _    []        = []
    go prev (next:cs) =
        let next' = minimize (distance prev) allOptions
         in next' : go next' cs
      where
        possibleInversions :: [Named.Chord Abs]
        possibleInversions = [
              invert i next
            | i <- permissibleInversions (Chord.Named.getType next)
            ]

        -- We consider all inversions
        --
        -- * in their \"natural\" octave
        -- * one octave lower (because inversion tends to move everything up)
        -- * one octave higher (to match a previous inverted chord)
        allOptions :: [Named.Chord Abs]
        allOptions = concatMap addOctaves possibleInversions
          where
            addOctaves :: Named.Chord Abs -> [Named.Chord Abs]
            addOctaves c = [
                c
              , transposeOctave (OctaveShift (-1)) c
              , transposeOctave (OctaveShift (-2)) c
              , transposeOctave (OctaveShift   1 ) c
              , transposeOctave (OctaveShift   2 ) c
              ]

{-------------------------------------------------------------------------------
  Standard progressions
-------------------------------------------------------------------------------}

-- | Standard progressions
data Name =
    Major251
  | Minor251

named :: Name -> Progression Rel
named = Progression . fmap Chord.Named.Rel . \case
    Major251 -> [
        Chord.Name "2" Chord.Minor7
      , Chord.Name "5" Chord.Dominant7
      , Chord.Name "1" Chord.Major7
      ]
    Minor251 -> [
        Chord.Name "2" Chord.HalfDiminished
      , Chord.Name "5" Chord.SevenFlat9
      , Chord.Name "1" Chord.Minor7
      ]