-- | Monad for rendering Lilypond
--
-- Intended for qualified import.
--
-- > import Lilypond.Render.Monad (RenderM)
-- > import Lilypond.Render.Monad qualified as RenderM
module Lilypond.Render.Monad (
    RenderM -- opaque
  , run
    -- * Use 'RenderM' as 'Doc'
  , line
  , lines
  , indent
  , concat
    -- * Conditionals
  , when
  , whenJust
    -- * Document structure
  , withinScope
    -- * Table of contents
  , bookPart
  , section
  , score
  , setupTableOfContents
    -- * Markup
  , markup
  ) where

import Prelude hiding (lines, concat)
import Prelude qualified

import Control.Applicative
import Control.Monad.Reader (Reader)
import Control.Monad.Reader qualified as Reader
import Control.Monad.State (StateT)
import Control.Monad.State qualified as State
import Data.List qualified as List
import Data.Semigroup
import Data.String (IsString(..))

import Lilypond.Markup qualified as Ly (Markup)
import Lilypond.Markup qualified as Ly.Markup
import Lilypond.Util.Doc (Doc)
import Lilypond.Util.Doc qualified as Doc

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Monad used for rendering Lilypond
newtype RenderM cls a = WrapRenderM{
      unwrapRenderM :: StateT RenderState (Reader (RenderEnv cls)) a
    }
  deriving newtype (
      Functor
    , Applicative
    , Monad
    )

run :: Ly.Markup.IsClass cls -> RenderM cls a -> a
run isClass =
      flip Reader.runReader (initRenderContext isClass)
    . flip State.evalStateT initRenderState
    . unwrapRenderM

{-------------------------------------------------------------------------------
  Make @RenderM Doc@ useable in much the same way as @Doc@ itself
-------------------------------------------------------------------------------}

instance IsString  (RenderM cls Doc) where fromString = pure . fromString
instance Semigroup (RenderM cls Doc) where sconcat    = fmap sconcat . sequence
instance Monoid    (RenderM cls Doc) where mconcat    = fmap mconcat . sequence

line :: String -> RenderM cls Doc
line = pure . Doc.line

lines :: [String] -> RenderM cls Doc
lines = pure . Doc.lines

indent :: RenderM cls Doc -> RenderM cls Doc
indent = liftA Doc.indent

concat :: [RenderM cls String] -> RenderM cls String
concat = fmap Prelude.concat . sequence

{-------------------------------------------------------------------------------
  Conditionals
-------------------------------------------------------------------------------}

when :: Bool -> RenderM cls Doc -> RenderM cls Doc
when False = mempty
when True  = id

whenJust :: Maybe a -> (a -> RenderM cls Doc) -> RenderM cls Doc
whenJust Nothing  _ = mempty
whenJust (Just x) f = f x

{-------------------------------------------------------------------------------
  Document structure
-------------------------------------------------------------------------------}

withinScope :: String -> RenderM cls Doc -> RenderM cls Doc
withinScope = fmap . Ly.Markup.withinScope

{-------------------------------------------------------------------------------
  Table of contents

  Lilypond has only rudimentary support for a table of contents. We need to
  setup additional infrastructure to get more structure, and we need to keep
  track of nesting manually.

  <https://lilypond.org/doc/v2.24/Documentation/notation/table-of-contents>

  NOTE: Scheme identifiers containing numbers must be wrapped in quotes.
-------------------------------------------------------------------------------}

-- | Internal: generate new TOC label
newTocLabel :: RenderM cls TocLabel
newTocLabel = WrapRenderM $ State.state $ \st -> (
      st.nextTocLabel
    , st{nextTocLabel = succ st.nextTocLabel}
    )

-- | Internal: get parent sections
getParents :: RenderM cls [TocLabel]
getParents = WrapRenderM $ Reader.asks tocParents

-- | Internal: render label for use in Lilypond
renderTocPath ::
     [TocLabel]  -- ^ Path in reverse order
  -> String
renderTocPath =
      List.intercalate "."
    . map (\(TocLabel l) -> "\"L" ++ show l ++ "\"")
    . reverse

nest :: TocLabel -> RenderM cls Doc -> RenderM cls Doc
nest label (WrapRenderM doc) = WrapRenderM $ Reader.local aux doc
  where
    aux :: RenderEnv cls -> RenderEnv cls
    aux env = env{tocParents = label : env.tocParents}

bookPart :: Ly.Markup cls -> RenderM cls Doc -> RenderM cls Doc
bookPart title contents = do
    tocLabel <- newTocLabel
    withinScope "bookpart" $ mconcat [
        line $ List.intercalate " " [
            "\\tocBookpart"
          , renderTocPath [tocLabel]
          , "\"" ++ Ly.Markup.strip title ++ "\""
          ]
      , nest tocLabel contents
      ]

-- | Section
--
-- Sections are concept /we/ introduce; they do not exist as an actual Lilypond
-- structure.
section :: Ly.Markup cls -> RenderM cls Doc -> RenderM cls Doc
section title contents = do
    tocLabel <- newTocLabel
    parents  <- getParents
    mconcat [
        line $ List.intercalate " " [
            "\\tocItem"
          , renderTocPath (tocLabel : parents)
          , "\"" ++ Ly.Markup.strip title ++ "\""
          ]
      , nest tocLabel contents
      ]

score :: Ly.Markup cls -> RenderM cls Doc -> RenderM cls Doc
score title contents = do
    tocLabel <- newTocLabel
    parents  <- getParents
    mconcat [
        -- @tocItem@ must be outside the @score@ scope
        line $ List.intercalate " " [
            "\\tocItem"
          , renderTocPath (tocLabel : parents)
          , "\"" ++ Ly.Markup.strip title ++ "\""
          ]
      , withinScope "score" $ nest tocLabel contents
      ]

-- | Setup necessary Lilypond infrastructure to render the ToC
--
-- We use a different syntax only for the book part titles, and leave the rest
-- to Lilypond.
setupTableOfContents :: Doc
setupTableOfContents = Doc.lines [
      "\\paper {"
    , "  tocBookpart_markup = \\markup \\large \\column {"
    , "    \\hspace #1"
    , "    { \\bold \\fromproperty #'toc:text }"
    , "    \\hspace #1"
    , "  }"
    , "}"
    , "tocBookpart ="
    , "  #(define-music-function (label text) (symbol? markup?)"
    , "     (add-toc-item! 'tocBookpart_markup text label))"
    ]

{-------------------------------------------------------------------------------
  Dealing with markup
-------------------------------------------------------------------------------}

markup :: Ly.Markup cls -> RenderM cls Doc
markup x = do
    isClass <- WrapRenderM $ Reader.asks markupClass
    return $ Ly.Markup.render $ Ly.Markup.applyClasses isClass x

{-------------------------------------------------------------------------------
  Context
-------------------------------------------------------------------------------}

newtype TocLabel = TocLabel Int
  deriving newtype (Enum)

data RenderEnv cls = RenderEnv{
      tocParents  :: [TocLabel]
    , markupClass :: Ly.Markup.IsClass cls
    }

initRenderContext :: Ly.Markup.IsClass cls -> RenderEnv cls
initRenderContext isClass = RenderEnv{
      tocParents  = []
    , markupClass = isClass
    }

data RenderState = RenderState{
      nextTocLabel :: TocLabel
    }

initRenderState :: RenderState
initRenderState = RenderState{
      nextTocLabel = TocLabel 1
    }