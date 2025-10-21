-- | Very basic pretty-printing infrastructure
--
-- Intended for qualified import.
--
-- > import Lilypond.Util.Pretty (Doc)
-- > import Lilypond.Util.Pretty qualified as Doc
module Lilypond.Util.Pretty (
    Doc -- opaque
    -- * Construction
  , line
  , lines
  , indent
  , when
  , whenJust
    -- * Rendering
  , render
  ) where

import Prelude hiding (lines)

import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Semigroup
import Data.String (IsString)
import Data.String qualified as String

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

when :: Bool -> Doc -> Doc
when False = mempty
when True  = id

whenJust :: Maybe a -> (a -> Doc) -> Doc
whenJust Nothing  _ = mempty
whenJust (Just x) f = f x

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