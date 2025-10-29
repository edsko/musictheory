-- | Type A/B chords (in four way close voicings)
--
-- > import Exercises.Util.TypeAB (TypeAB(..))
-- > import Exercises.Util.TypeAB qualified as TypeAB
module Exercises.Util.TypeAB(
    TypeAB(..)
    -- * Combinators
  , markIf
  , markInversion
  ) where

import MusicTheory

import Lilypond qualified as Ly

import Exercises.Chords (ChordInversion(..))

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data TypeAB x = TypeAB{
      typeA :: x
    , typeB :: x
    }
  deriving stock (Show, Foldable)

{-------------------------------------------------------------------------------
  Combinators
-------------------------------------------------------------------------------}

markIf :: (x -> Bool) -> (x -> Ly.Annotation -> y) -> TypeAB x -> TypeAB y
markIf p f TypeAB{typeA, typeB} = TypeAB{
      typeA = if p typeA then f typeA "(type A)" else f typeA Ly.NoAnnotation
    , typeB = if p typeB then f typeB "(type B)" else f typeB Ly.NoAnnotation
    }

markInversion ::
     ((Inversion, OctaveShift) -> Bool)
  -> TypeAB (Inversion, OctaveShift)
  -> TypeAB ChordInversion
markInversion p =
    markIf p $ \(inversion, octaveShift) annotation -> ChordInversion{
        inversion
      , octaveShift
      , annotation
      }
