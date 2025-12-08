{-# LANGUAGE OverloadedLists #-}

-- | Chord progressions
--
-- Intended for qualified import.
--
-- > import MusicTheory.Progression (Progression, ProgressionF(..))
-- > import MusicTheory.Progression qualified as Progression
module MusicTheory.Progression (
    Progression
  , ProgressionF(..)
    -- * Combinators
  , mapFirst
  , wrtScale
  , voiceLeading
    -- * Standard chord progressions
  , Name(..)
  , UseSevenFlat9(..)
  , named
  ) where

import Control.Monad.State
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

newtype ProgressionF f r = Progression (f (Named.Chord r))

deriving instance Show (f (Named.Chord r)) => Show (ProgressionF f r)

type Progression = ProgressionF NonEmpty

{-------------------------------------------------------------------------------
  Combinators
-------------------------------------------------------------------------------}

mapFirst :: forall f r.
     Traversable f
  => (Named.Chord r -> Named.Chord r) -> ProgressionF f r -> ProgressionF f r
mapFirst f (Progression chords) = Progression $
    flip evalState True $ traverse aux chords
  where
    aux :: Named.Chord r -> State Bool (Named.Chord r)
    aux c = state $ \isFirst -> (
          if isFirst then f c else c
        , False
        )

-- | Choose chord voicings
wrtScale ::
     Functor f
  => Scale -> Voicing -> Octave -> ProgressionF f Rel -> ProgressionF f Abs
wrtScale scale voicing octave (Progression chords) = Progression $
    Voicing.wrtScale scale voicing octave <$> chords

-- | Choose inversions to minimize distance between successive chords
--
-- Fails if there is no unique solution.
voiceLeading ::
     Traversable f
  => (Chord.Type -> [Inversion]) -- ^ Permissible inversions
  -> ProgressionF f Abs -> ProgressionF f Abs
voiceLeading permissibleInversions = \(Progression chords) -> Progression $
    flip evalState Nothing $ traverse aux chords
  where
    aux :: Named.Chord Abs -> State (Maybe (Named.Chord Abs)) (Named.Chord Abs)
    aux next = state $ \case
        Nothing ->
          -- First chord in the sequence; leave as-is
          (next, Just next)
        Just prev ->
          let next' = minimize (distance prev) allOptions
          in (next', Just next')
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
    Major251 UseSevenFlat9
  | Minor251 UseSevenFlat9

data UseSevenFlat9 = WithSevenFlat9 | WithoutSevenFlat9

named :: Name -> Progression Rel
named = Progression . fmap Chord.Named.Rel . \case
    Major251 useSevenFlat9 -> [
        Chord.Name "2" $ Chord.Minor7
      , Chord.Name "5" $ case useSevenFlat9 of
                           WithoutSevenFlat9 -> Chord.Dominant7
                           WithSevenFlat9    -> Chord.SevenFlat9
      , Chord.Name "1" $ Chord.Major7
      ]
    Minor251 useSevenFlat9 -> [
        Chord.Name "2" Chord.HalfDiminished
      , Chord.Name "5" $ case useSevenFlat9 of
                           WithSevenFlat9    -> Chord.SevenFlat9
                           WithoutSevenFlat9 -> Chord.Altered
      , Chord.Name "1" Chord.Minor7
      ]