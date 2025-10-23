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
  , Duration(..)
  ) where

import Data.Default

import MusicTheory.Chord.Named qualified as Named (Chord)
import MusicTheory.Chord.Unnamed qualified as Unnamed (Chord(..))
import MusicTheory.Reference
import MusicTheory.Scale qualified as Scale

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
    , intro  :: Maybe String
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
    StaffNamedChord   (  Named.Chord Absolute) Duration
  | StaffUnnamedChord (Unnamed.Chord Absolute) Duration
  | StaffLinebreak
  | StaffComment String
  | StaffKeySignature Scale.Name

data Duration =
    OneOver Word

{-------------------------------------------------------------------------------
  Markup
-------------------------------------------------------------------------------}
