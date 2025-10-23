-- | Markup
--
-- Intended for qualified import.
--
-- > import Lilypond.Markup qualified as Ly (Markup)
-- > import Lilypond.Markup qualified as Ly.Markup
module Lilypond.Markup (
    Markup(..)
  , scope
  , strip
  , render
    -- * Classes
  , IsClass(..)
  , applyClasses
    -- * Auxiliary
  , withinScope
  ) where

import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Semigroup
import Data.String
import Data.Void

import Lilypond.Util.Doc (Doc)
import Lilypond.Util.Doc qualified as Doc

{-------------------------------------------------------------------------------
  Markup
-------------------------------------------------------------------------------}

-- | Markup
--
-- We generalize over a datatype @cls@, intended to mimick CSS \"classes\": ways
-- to give names to predefined bits of markup.
data Markup cls =
    -- | Plain text (support 'IsString')
    --
    -- This results in a quoted string.
    Text String

    -- | Word wrap
    --
    -- This results in @\wordwrap { .. }@.
  | Wordwrap String

    -- | Support 'Semigroup' and 'Monoid'
  | MConcat [Markup cls]

    -- | Apply style class
  | Style cls (Markup cls)

    -- | Low-level: modify the generated string
    --
    -- This is primarily useful for the interpretation of style classes.
  | Wrap (Doc -> Doc) (Markup cls)

instance IsString  (Markup cls) where fromString = Text
instance Monoid    (Markup cls) where mconcat    = MConcat
instance Semigroup (Markup cls) where sconcat    = MConcat . NonEmpty.toList

-- | Introduce scope
--
-- For example, @scope "bold"@ generates something like
--
-- > \bold { .. }
scope :: String -> Markup cls -> Markup cls
scope label = Wrap (withinScope label)

-- | Strip all markup, leaving only plain text
strip :: Markup cls -> String
strip = go
  where
    go :: Markup cls -> String
    go (Text     str)    = str
    go (Wordwrap str)    = str
    go (MConcat  markup) = List.intercalate " " $ map go markup
    go (Wrap _   markup) = go markup
    go (Style _  markup) = go markup

{-------------------------------------------------------------------------------
  Style classes
-------------------------------------------------------------------------------}

-- | Translate class into low-level markup primitives
--
-- This is a datatype rather than a typeclass, so the interpration of these
-- classes can be changed at runtime
newtype IsClass cls = IsClass{
      applyClass :: cls -> Markup Void -> Markup Void
    }

applyClasses :: forall cls. IsClass cls -> Markup cls -> Markup Void
applyClasses IsClass{applyClass} = go
  where
    go :: Markup cls -> Markup Void
    go (Text      str)    = Text     str
    go (Wordwrap  str)    = Wordwrap str
    go (MConcat   markup) = MConcat        $ map go markup
    go (Wrap f    markup) = Wrap f         $ go markup
    go (Style cls markup) = applyClass cls $ go markup

{-------------------------------------------------------------------------------
  Rendering
-------------------------------------------------------------------------------}

render :: Markup Void -> Doc
render = withinScope "markup" . go
  where
    go :: Markup Void -> Doc
    go (Text      str)    = Doc.line $ "\"" ++ str ++ "\""
    go (Wordwrap  str)    = withinScope "wordwrap" $ Doc.line str
    go (MConcat   markup) = foldMap go markup
    go (Wrap f    markup) = f $ go markup
    go (Style cls _)      = absurd cls

withinScope :: String -> Doc -> Doc
withinScope label body = mconcat [
      Doc.line $ "\\" ++ label ++ " {"
    , Doc.indent body
    , "}"
    ]
