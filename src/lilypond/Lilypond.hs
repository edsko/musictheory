-- | Basic Lilypond structure
--
-- > import Lilypond (Lilypond)
-- > import Lilypond qualified as Ly
module Lilypond (
    -- * Document structure
    Lilypond(..)
  , Book(..)
  , BookPart(..)
  , BookPartElem(..)
  , Score(..)
  , ScoreElem(..)
    -- * Header
  , Header(..)
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
      header :: Header
    , books  :: [Book]
    }

data Book = Book{
      header :: Header
    , parts  :: [BookPart]
    }

data BookPart = BookPart{
      header :: Header
    , elems  :: [BookPartElem]
    }

data BookPartElem =
    BookPartScore Score

data Score = Score{
      header :: Header
    , elems  :: ScoreElem
    }

data ScoreElem =
    ScoreStaff StaffProps [StaffElem]

{-------------------------------------------------------------------------------
  Header
-------------------------------------------------------------------------------}

-- | Header
--
-- TODO: These should not be strings: they support markup.
-- <https://lilypond.org/doc/v2.24/Documentation/notation/creating-titles-headers-and-footers#default-layout-of-bookpart-and-score-titles>
data Header = Header{
      dedication  :: Maybe String
    , title       :: Maybe String
    , subtitle    :: Maybe String
    , subsubtitle :: Maybe String
    , instrument  :: Maybe String
    , poet        :: Maybe String
    , composer    :: Maybe String
    , meter       :: Maybe String
    , arranger    :: Maybe String
    , tagline     :: Maybe String
    , copyright   :: Maybe String
    , piece       :: Maybe String
    , opus        :: Maybe String
    }

instance Default Header where
  def = Header{
        dedication  = Nothing
      , title       = Nothing
      , subtitle    = Nothing
      , subsubtitle = Nothing
      , instrument  = Nothing
      , poet        = Nothing
      , composer    = Nothing
      , meter       = Nothing
      , arranger    = Nothing
      , tagline     = Nothing
      , copyright   = Nothing
      , piece       = Nothing
      , opus        = Nothing
      }

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
