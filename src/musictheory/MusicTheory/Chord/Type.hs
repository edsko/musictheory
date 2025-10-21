{-# LANGUAGE OverloadedLists #-}

-- | Chord types
--
-- Intended for qualified import.
--
-- > import MusicTheory.Chord.Type qualified as Chord (Type(..))
-- > import MusicTheory.Chord.Type qualified as Chord.Type
module MusicTheory.Chord.Type (
    Type(..)
    -- * Expansion to scale degrees
  , wrtMajorScale
  ) where

import Data.List.NonEmpty (NonEmpty)

import MusicTheory.Scale qualified as Scale

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Type =
    -- Basic chords
    Basic_MajorTriad
  | Basic_MinorTriad
  | Basic_MajorSeventh
  | Basic_MinorSeventh
  | Basic_DominantSeventh

    -- Standard Jazz voicings
  | StdJazz_Major
  | StdJazz_Minor
  | StdJazz_Dominant
  | StdJazz_HalfDiminished
  | StdJazz_Altered
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Expansion to scale degrees
-------------------------------------------------------------------------------}

-- | Scale degrees for chord, wrt to the corresponding /major/ scale
--
-- > wrtMajorScale TriadMajor == [ "1" ,  "3" , "5" ]
-- > wrtMajorScale TriadMinor == [ "1" , "♭3" , "5" ]
wrtMajorScale :: Type -> NonEmpty Scale.Degree
wrtMajorScale = \case
    --- Basic chords
    Basic_MajorTriad       -> [ "1" ,  "3" , "5"       ]
    Basic_MinorTriad       -> [ "1" , "♭3" , "5"       ]
    Basic_MajorSeventh     -> [ "1" ,  "3" , "5",  "7" ]
    Basic_MinorSeventh     -> [ "1" , "♭3" , "5", "♭7" ]
    Basic_DominantSeventh  -> [ "1" ,  "3" , "5", "♭7" ]

    -- Standard Jazz voicings
    StdJazz_Major          -> [ "3" ,  "5" ,  "7" ,  "9" ]
    StdJazz_Minor          -> [ "3" , "♭5" , "♭7" ,  "9" ]
    StdJazz_Dominant       -> [ "3" , "13" , "♭7" ,  "9" ]
    StdJazz_HalfDiminished -> [ "1" , "♭3" , "♭5" , "♭7" ]
    StdJazz_Altered        -> [ "3" , "♯5" , "♭7" , "♯9" ]
