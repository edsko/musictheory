-- | Markup
--
-- Intended for qualified import.
--
-- > import Lilypond.Markup qualified as Ly (Markup)
-- > import Lilypond.Markup qualified as Ly.Markup
module Lilypond.Markup (
    Markup(..)
  , Music(..)
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

import MusicTheory.Interval (Interval)

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
  | Wordwrap (Markup cls)

    -- | Support 'Semigroup' and 'Monoid'
  | MConcat [Markup cls]

    -- | Apply style class
  | Style cls (Markup cls)

    -- | Low-level: modify the generated string
    --
    -- This is primarily useful for the interpretation of style classes.
  | Wrap (Doc -> Doc) (Markup cls)

data Music =
    Interval Interval

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
strip = List.intercalate " " . go
  where
    go :: Markup cls -> [String]
    go Empty             = []
    go (Text     str)    = [str]
    go (Quoted   str)    = [str]
    go (Music    music)  = [goMusic music]
    go (Wordwrap markup) = go markup
    go (MConcat  markup) = concatMap go markup
    go (Wrap _   markup) = go markup
    go (Style _  markup) = go markup

    goMusic :: Music -> String
    goMusic (Interval d) = show d

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
    go Empty              = Empty
    go (Text      str)    = Text   str
    go (Quoted    str)    = Quoted str
    go (Music     music)  = Music  music
    go (Wordwrap  markup) = Wordwrap       $ go markup
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
    go Empty              = mempty
    go (Text      str)    = Doc.line $ str
    go (Quoted    str)    = Doc.line $ "\"" ++ str ++ "\""
    go (Music     music)  = goMusic music
    go (Wordwrap  markup) = withinScope "wordwrap" $ go markup
    go (MConcat   markup) = foldMap go markup
    go (Wrap f    markup) = f $ go markup
    go (Style cls _)      = absurd cls

    -- Our unicode rendering works just fine in Lilypond
    goMusic :: Music -> Doc
    goMusic (Interval atal) = Doc.line $ show atal

withinScope :: String -> Doc -> Doc
withinScope label body = mconcat [
      Doc.line $ "\\" ++ label ++ " {"
    , Doc.indent body
    , "}"
    ]
