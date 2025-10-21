{-# LANGUAGE OverloadedLists #-}

-- | Named standard progression
--
-- Intended for qualified import.
--
-- > import MusicTheory.Progression.Name qualified as Progression (Name(..))
-- > import MusicTheory.Progression.Name qualified as Progression.Name
module MusicTheory.Progression.Name (
    Name(..)
    -- * Expansion to scale degrees
  , wrtMajorScale
  ) where

import Data.List.NonEmpty (NonEmpty)

import MusicTheory.Chord.Type qualified as Chord (Type)
import MusicTheory.Chord.Type qualified as Chord.Type
import MusicTheory.Scale qualified as Scale

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Standard progressions
data Name =
    -- | Using standard Jazz voicing
    StdJazz_Major251

{-------------------------------------------------------------------------------
  Expansion to scale degrees

  TODO: There is duplicated logic between here and "MusicTheory.Chord.Type",
  but it's not trivial to avoid it, for two reasons:

  - A progression such as 2-5-1 is all with respect to a /single/ scale;
    we should not think of these as chords in different scales, with potentially
    different note spellings.
  - We cannot easily shift scale degrees, without knowing what the underlying
    scale looks like. For example, shifting degree "3" by one semitone in a
    major scale results in degree "4", but shifting degree "2" should either
    result in "♯2" or "♭2" -- and it's unclear which.

  This requires more thought, but for now my conclusion is that how we think
  of these chords as relating to notes in the context key is non-obvious.
-------------------------------------------------------------------------------}

-- | Major scale degrees
--
-- All scale degrees refer to the /parent/ scale. For example, for a 2-5-1
-- progression using the standard ninth voicing:
--
-- >  1    2    3    4    5    6    7   (8)   9  (10)  11  (12)  13  (14)
-- >  C    D    E    F    G    A    B   (C)   D  ( E)   F  ( G)   A  ( B)
-- > ------------------------------------------------
-- >       ._________*_________*_________*_________*                    (iim7)
-- >                      ._________*_________*_________*_________*     (V7)
-- >  ._________*_________*_________*_________*                         (Imaj7)
wrtMajorScale :: Name -> NonEmpty (Chord.Type, NonEmpty Scale.Degree)
wrtMajorScale = \case
    StdJazz_Major251 -> [
        (Chord.Type.StdJazz_Minor    , [ "4" , "6" ,  "8" , "10" ])
      , (Chord.Type.StdJazz_Dominant , [ "7" , "9" , "11" , "13" ])
      , (Chord.Type.StdJazz_Major    , [ "1" , "3" ,  "5" ,  "7" ])
      ]
