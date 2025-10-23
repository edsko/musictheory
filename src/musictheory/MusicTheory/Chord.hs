{-# LANGUAGE OverloadedLists #-}

-- | Supporting definitions for working with chords
--
-- Intended for qualified import.
--
-- > import MusicTheory.Chord qualified as Chord
module MusicTheory.Chord (
    -- * Chord names
    Type(..)
  , Name(..)
    -- * Scale degrees
  , scaleDegrees
  ) where

import Data.List.NonEmpty (NonEmpty)

import MusicTheory.Reference
import MusicTheory.Scale qualified as Scale

{-------------------------------------------------------------------------------
  Chord names
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
  | StdJazz_AlteredFlat9
  | StdJazz_Sus
  deriving stock (Show)

data Name r = Name{
      root :: Reference r
    , typ  :: Type
    }

deriving instance IsReferenceKind r => Show (Name r)

instance MakeAbsolute Name where
  wrtScale octave scale Name{root, typ} = Name{
        root = absoluteReference octave scale root
      , typ
      }

{-------------------------------------------------------------------------------
  Expansion to scale degrees
-------------------------------------------------------------------------------}

scaleDegrees :: Scale.Type -> Type -> Scale.Degree -> NonEmpty Scale.Degree
scaleDegrees Scale.Major = scaleDegreesMajor
scaleDegrees Scale.Minor = scaleDegreesMinor

-- | Scale degrees for chord, wrt to the corresponding /major/ scale
--
-- > wrtMajorScale TriadMajor == [ "1" ,  "3" , "5" ]
-- > wrtMajorScale TriadMinor == [ "1" , "♭3" , "5" ]
scaleDegreesMajor :: Type -> Scale.Degree -> NonEmpty Scale.Degree

scaleDegreesMajor typ (Scale.Degree 1 Nothing) =
    case typ of
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
      StdJazz_AlteredFlat9   -> [ "3" ,  "5" , "♭7" , "♭9" ]
      StdJazz_Sus            -> [ "2" ,  "4" , "13" , "♭7" ]

-- Chords with their root at different scale degrees
scaleDegreesMajor typ root =
    case (typ, root) of
      -- Major 2-5-1 using standard jazz voicing:
      --
      -- >  1    2    3    4    5    6    7   (8)   9  (10)  11  (12)  13  (14)
      -- >  C    D    E    F    G    A    B   (C)   D  ( E)   F  ( G)   A  ( B)
      -- > ------------------------------------------------
      -- >       ._________*_________*_________*_________*                    (iim7)
      -- >                      ._________*_________*_________*_________*     (V7)
      -- >  ._________*_________*_________*_________*                         (Imaj7)
      (StdJazz_Minor    , "2") -> [ "4" , "6" ,  "8" , "10" ]
      (StdJazz_Dominant , "5") -> [ "7" , "9" , "11" , "13" ]

      -- We don't implement all possible combinations here
      _otherwise -> error $ "TODO: " ++ show (typ, root)

scaleDegreesMinor :: Type -> Scale.Degree -> NonEmpty Scale.Degree
scaleDegreesMinor typ root =
    case (typ, root) of
      -- Minor 2-5-1 using standard jazz voicing
      --
      -- >  1    2    3    4    5    6    7   (8)   9  (10)  11  (12)  13  (14)
      -- >  C    D    E♭   F    G    A♭   B♭  (C)   D  (E♭)   F  ( G)  A♭  (B♭)
      -- > ------------------------------------------------
      -- >       *_________*_________*_________*                              (iim7.-5)
      -- >                      ._________*_________*_________*_________*     (V7.-9)
      -- >  ._________*_________*_________*_________*                         (Imaj7)
      --
      -- NOTE: The "Flat9" "AlteredFlat9" does not refer to scale degrees, but
      -- rather to the interval between its nineth (second).
      --
      -- NOTE: In the context of the (natural) minor scale, we need to sharpen
      -- scale degree 7 to get the dominant chord (rather than a minor chord).
      (StdJazz_HalfDiminished , "2") -> [  "2" , "4" ,  "6" ,  "8" ]
      (StdJazz_AlteredFlat9   , "5") -> [ "♯7" , "9" , "11" , "13" ]
      (StdJazz_Minor          , "1") -> [  "3" , "5" ,  "7" ,  "9" ]

      -- We don't implement all possible combinations here
      _otherwise -> error $ "TODO: " ++ show (typ, root)
