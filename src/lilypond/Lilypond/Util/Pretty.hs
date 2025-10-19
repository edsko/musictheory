-- | Very basic pretty-printing infrastructure
--
-- Intended for qualified import.
--
-- > import Lilypond.Util.Pretty (Doc)
-- > import Lilypond.Util.Pretty qualified as Doc
module Lilypond.Util.Pretty (
    Doc -- opaque
    -- * Construction
  , fromString
  , fromStrings
  , indent
    -- * Rendering
  , render
  ) where

import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Semigroup
import Data.String (IsString)
import Data.String qualified as String

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Doc =
    FromString String -- ^ String without newlines
  | MConcat [Doc]     -- ^ Vertical composition
  | Indent Doc        -- ^ Indent

instance IsString  Doc where fromString = FromString
instance Monoid    Doc where mconcat    = MConcat
instance Semigroup Doc where sconcat    = MConcat . NonEmpty.toList

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

indent :: Doc -> Doc
indent = Indent

fromString :: String -> Doc
fromString = String.fromString

fromStrings :: [String] -> Doc
fromStrings = mconcat . map fromString

{-------------------------------------------------------------------------------
  Rendering
-------------------------------------------------------------------------------}

renderLines :: Doc -> [String]
renderLines = \case
    FromString str -> [str]
    MConcat docs   -> mconcat $ map renderLines docs
    Indent doc     -> map ("  " ++) $ renderLines doc

render :: Doc -> String
render = List.intercalate "\n" . renderLines