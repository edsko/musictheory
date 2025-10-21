-- | Basic Lilypond structure
--
-- > import Lilypond (Lilypond)
-- > import Lilypond qualified as Ly
module Lilypond (
    -- * Document structure
    Lilypond(..)
  , Book(..)
  , Bookpart(..)
  , Section(..)
  , Score(..)
  , ScoreElem(..)
    -- * Staff elements
  , StaffProps(..)
  , StaffElem(..)
  , Chord(..)
  , Duration(..)
  ) where

import Data.Default

import MusicTheory.Chord qualified as Chord

{-------------------------------------------------------------------------------
  Document structure
-------------------------------------------------------------------------------}

data Lilypond = Lilypond{
      books  :: [Book]
    }

data Book = Book{
      title  :: String
    , author :: String
    , parts  :: [Bookpart]
    }

data Bookpart = Bookpart{
      title    :: String
    , sections :: [Section]
    }

data Section = Section{
      title  :: String
    , scores :: [Score]
    }

data Score = Score{
      title :: String
    , elems :: ScoreElem
    }

data ScoreElem =
    ScoreStaff StaffProps [StaffElem]

{-------------------------------------------------------------------------------
  Staff elements
-------------------------------------------------------------------------------}

data StaffProps = StaffProps{
      hideTimeSignature  :: Bool
    , omitMeasureNumbers :: Bool
    }

instance Default StaffProps where
  def = StaffProps{
        hideTimeSignature  = False
      , omitMeasureNumbers = False
      }

data StaffElem =
    StaffChord Chord
  | StaffLinebreak

data Chord = Chord{
      name     :: Maybe Chord.Name
    , notes    :: Chord.Chord
    , duration :: Duration
    }

data Duration =
    Whole
  | Half
  | Quarter
  | Eighth
  | Sixteenth
  | Thirtysecond
