-- | Markup
--
-- Intended for qualified import.
--
-- > import Lilypond.Markup qualified as Ly.Markup
module Lilypond.Markup (
    Markup(..)
  , Music(..)
  , render
    -- * Styling
  , Style(..)
  , Stylesheet
    -- * Auxiliary
  , withinScope
  ) where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Semigroup
import Data.String

import MusicTheory.Interval (Interval)

import Lilypond.Part qualified as Ly.Part
import Lilypond.Util.Doc (Doc)
import Lilypond.Util.Doc qualified as Doc

{-------------------------------------------------------------------------------
  Markup
-------------------------------------------------------------------------------}

-- | Markup
data Markup =
    -- | Empty
    Empty

    -- | Plain text (support 'IsString')
  | Text String

    -- | Quoted text
  | Quoted String

    -- | Music element
    --
    -- <https://lilypond.org/doc/v2.24/Documentation/notation/music>
  | Music Music

    -- | Word wrap
    --
    -- This results in @\wordwrap { .. }@.
  | Wordwrap Markup

    -- | Support 'Semigroup' and 'Monoid'
  | MConcat [Markup]

    -- | Apply style class
  | Style Style Markup

data Music =
    Interval Interval

instance IsString  Markup where fromString = Text
instance Monoid    Markup where mconcat    = MConcat
instance Semigroup Markup where sconcat    = MConcat . NonEmpty.toList

{-------------------------------------------------------------------------------
  Classes
-------------------------------------------------------------------------------}

-- | Styling of individual elements
data Style =
    -- | Book part or (sub)section title
    PartTitle Ly.Part.Label

    -- | Exercise title
    --
    -- These are distinguished from general 'PartTitle', so we can use
    -- consistent styling no matter the nesting depth.
  | ScoreTitle Ly.Part.Label

-- | How to interpret styles
type Stylesheet = Style -> Doc -> Doc

{-------------------------------------------------------------------------------
  Rendering
-------------------------------------------------------------------------------}

render :: Stylesheet -> Markup -> Doc
render stylesheet = withinScope "markup" . go
  where
    go :: Markup -> Doc
    go Empty              = mempty
    go (Text      str)    = Doc.line $ str
    go (Quoted    str)    = Doc.line $ "\"" ++ str ++ "\""
    go (Music     music)  = goMusic music
    go (Wordwrap  markup) = withinScope "wordwrap" $ go markup
    go (MConcat   markup) = foldMap go markup
    go (Style cls markup) = stylesheet cls $ go markup

    -- Our unicode rendering works just fine in Lilypond
    goMusic :: Music -> Doc
    goMusic (Interval atal) = Doc.line $ show atal

withinScope :: String -> Doc -> Doc
withinScope label body = mconcat [
      Doc.line $ "\\" ++ label ++ " {"
    , Doc.indent body
    , "}"
    ]
