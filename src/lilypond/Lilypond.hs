-- | Basic Lilypond structure
--
-- > import Lilypond (Lilypond)
-- > import Lilypond qualified as Ly
module Lilypond (
    -- * Document structure
    Lilypond(..)
  , Header(..)
  , Score(..)
  , ScoreHeader(..)
  , ScoreElem(..)
    -- * Staff elements
  , StaffElem(..)
  , Chord(..)
  , ChordName(..)
  , Duration(..)
  ) where

import MusicTheory.Chord qualified as Chord
import MusicTheory.Note qualified as Note

{-------------------------------------------------------------------------------
  Document structure
-------------------------------------------------------------------------------}

data Lilypond = Lilypond{
      header :: Header
    , scores :: [Score]
    }

data Header = Header{
      title    :: String
    , composer :: String
    }

data Score = Score{
      header :: ScoreHeader
    , elems  :: ScoreElem
    }

data ScoreHeader = ScoreHeader{
      piece :: String
    }

data ScoreElem =
    Staff [StaffElem]

{-------------------------------------------------------------------------------
  Notes
-------------------------------------------------------------------------------}

data StaffElem =
    StaffChord Chord
  | StaffLinebreak

data Chord = Chord{
      name     :: Maybe ChordName
    , notes    :: Chord.Chord
    , duration :: Duration
    }

data ChordName = ChordName Note.Simple Chord.Type

data Duration =
    Whole
  | Half
  | Quarter
  | Eighth
  | Sixteenth
  | Thirtysecond
