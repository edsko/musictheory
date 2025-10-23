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
    -- * Conditionals
  , when
  , whenJust
    -- * Document structure
  , scope
    -- * Table of contents
  , bookPart
  , section
  , score
  , setupTableOfContents
  ) where

import Prelude hiding (lines)

import Control.Applicative
import Control.Monad.Reader (Reader)
import Control.Monad.Reader qualified as Reader
import Control.Monad.State (StateT)
import Control.Monad.State qualified as State
import Data.List qualified as List
import Data.Semigroup
import Data.String (IsString(..))

import Lilypond.Util.Doc (Doc)
import Lilypond.Util.Doc qualified as Doc

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Monad used for rendering Lilypond
newtype RenderM a = WrapRenderM{
      unwrapRenderM :: StateT RenderState (Reader RenderEnv) a
    }
  deriving newtype (
      Functor
    , Applicative
    , Monad
    )

run :: RenderM a -> a
run =
      flip Reader.runReader initRenderContext
    . flip State.evalStateT initRenderState
    . unwrapRenderM

{-------------------------------------------------------------------------------
  Make @RenderM Doc@ useable in much the same way as @Doc@ itself
-------------------------------------------------------------------------------}

instance IsString  (RenderM Doc) where fromString = pure . fromString
instance Semigroup (RenderM Doc) where sconcat    = fmap sconcat . sequence
instance Monoid    (RenderM Doc) where mconcat    = fmap mconcat . sequence

line :: String -> RenderM Doc
line = pure . Doc.line

lines :: [String] -> RenderM Doc
lines = pure . Doc.lines

indent :: RenderM Doc -> RenderM Doc
indent = liftA Doc.indent

{-------------------------------------------------------------------------------
  Conditionals
-------------------------------------------------------------------------------}

when :: Bool -> RenderM Doc -> RenderM Doc
when False = mempty
when True  = id

whenJust :: Maybe a -> (a -> RenderM Doc) -> RenderM Doc
whenJust Nothing  _ = mempty
whenJust (Just x) f = f x

{-------------------------------------------------------------------------------
  Document structure
-------------------------------------------------------------------------------}

scope :: String -> RenderM Doc -> RenderM Doc
scope name body = mconcat [
      line $ "\\" ++ name ++ "{"
    , indent body
    , "}"
    ]

{-------------------------------------------------------------------------------
  Table of contents

  Lilypond has only rudimentary support for a table of contents. We need to
  setup additional infrastructure to get more structure, and we need to keep
  track of nesting manually.

  <https://lilypond.org/doc/v2.24/Documentation/notation/table-of-contents>

  NOTE: Scheme identifiers containing numbers must be wrapped in quotes.
-------------------------------------------------------------------------------}

-- | Internal: generate new TOC label
newTocLabel :: RenderM TocLabel
newTocLabel = WrapRenderM $ State.state $ \st -> (
      st.nextTocLabel
    , st{nextTocLabel = succ st.nextTocLabel}
    )

-- | Internal: get parent sections
getParents :: RenderM [TocLabel]
getParents = WrapRenderM $ Reader.asks tocParents

-- | Internal: render label for use in Lilypond
renderTocPath ::
     [TocLabel]  -- ^ Path in reverse order
  -> String
renderTocPath =
      List.intercalate "."
    . map (\(TocLabel l) -> "\"L" ++ show l ++ "\"")
    . reverse

nest :: TocLabel -> RenderM Doc -> RenderM Doc
nest label (WrapRenderM doc) = WrapRenderM $ Reader.local aux doc
  where
    aux :: RenderEnv -> RenderEnv
    aux env = env{tocParents = label : env.tocParents}

bookPart :: String -> RenderM Doc -> RenderM Doc
bookPart title contents = do
    tocLabel <- newTocLabel
    scope "bookpart" $ mconcat [
        line $ List.intercalate " " [
            "\\tocBookpart"
          , renderTocPath [tocLabel]
          , "\"" ++ title ++ "\""
          ]
      , nest tocLabel contents
      ]

-- | Section
--
-- Sections are concept /we/ introduce; they do not exist as an actual Lilypond
-- structure.
section :: String -> RenderM Doc -> RenderM Doc
section title contents = do
    tocLabel <- newTocLabel
    parents  <- getParents
    mconcat [
        line $ List.intercalate " " [
            "\\tocItem"
          , renderTocPath (tocLabel : parents)
          , "\"" ++ title ++ "\""
          ]
      , nest tocLabel contents
      ]

score :: String -> RenderM Doc -> RenderM Doc
score title contents = do
    tocLabel <- newTocLabel
    parents  <- getParents
    mconcat [
        -- @tocItem@ must be outside the @score@ scope
        line $ List.intercalate " " [
            "\\tocItem"
          , renderTocPath (tocLabel : parents)
          , "\"" ++ title ++ "\""
          ]
      , scope "score" $ nest tocLabel contents
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
  Context
-------------------------------------------------------------------------------}

newtype TocLabel = TocLabel Int
  deriving newtype (Enum)

data RenderEnv = RenderEnv{
      tocParents :: [TocLabel]
    }

initRenderContext :: RenderEnv
initRenderContext = RenderEnv{
      tocParents = []
    }

data RenderState = RenderState{
      nextTocLabel :: TocLabel
    }

initRenderState :: RenderState
initRenderState = RenderState{
      nextTocLabel = TocLabel 1
    }