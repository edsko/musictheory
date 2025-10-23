-- | Very basic pretty-printing infrastructure
--
-- Intended for qualified import.
--
-- > import Lilypond.Util.Doc (Doc)
-- > import Lilypond.Util.Doc qualified as Doc
module Lilypond.Util.Doc (
    Doc -- opaque
    -- * Construction
  , line
  , lines
  , indent
    -- * Rendering
  , render
  ) where

import Prelude hiding (lines)

import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Semigroup
import Data.String (IsString(..))

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Doc =
    Line String   -- ^ String without newlines
  | MConcat [Doc] -- ^ Vertical composition
  | Indent Doc    -- ^ Indent

instance IsString  Doc where fromString = Line
instance Monoid    Doc where mconcat    = MConcat
instance Semigroup Doc where sconcat    = MConcat . NonEmpty.toList

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

indent :: Doc -> Doc
indent = Indent

line :: String -> Doc
line = Line

lines :: [String] -> Doc
lines = mconcat . map line

{-------------------------------------------------------------------------------
  Rendering
-------------------------------------------------------------------------------}

renderLines :: Doc -> [String]
renderLines = \case
    Line str     -> [str]
    MConcat docs -> mconcat $ map renderLines docs
    Indent doc   -> map ("  " ++) $ renderLines doc

render :: Doc -> String
render = List.intercalate "\n" . renderLines