-- | General purpose definitions, intended for unqualified import.
module MusicTheory (
    -- * Classes
    TransposeOctave(..)
  ) where

{-------------------------------------------------------------------------------
  Classes
-------------------------------------------------------------------------------}

class TransposeOctave a where
  -- | Move to higher or lower octave
  transposeOctave :: Int -> a -> a
