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
  , bookpart
  , section
  , score
  , setupPaper
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
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Semigroup
import Data.String (IsString(..))

import Lilypond.Markup qualified as Ly (Markup)
import Lilypond.Markup qualified as Ly.Markup
import Lilypond.Part qualified as Ly.Part
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

run :: Ly.Markup.Stylesheet -> RenderM a -> a
run isClass =
      flip Reader.runReader (initRenderContext isClass)
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

concat :: [RenderM String] -> RenderM String
concat = fmap Prelude.concat . sequence

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

withinScope :: String -> RenderM Doc -> RenderM Doc
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
newPartLabel :: RenderM Ly.Part.Label
newPartLabel = WrapRenderM $ State.state $ \st ->
    let nextTocLabel = mapHead succ st.currTocLabel
     in ( Ly.Part.Label nextTocLabel
        , st{currTocLabel = nextTocLabel}
        )

nest :: RenderM Doc -> RenderM Doc
nest (WrapRenderM mkDoc) = WrapRenderM $ do
    State.modify $ \st-> st{
        currTocLabel = NE.cons 0 st.currTocLabel
      }
    doc <- mkDoc
    State.modify $ \st-> st{
        currTocLabel =
          case st.currTocLabel of
            _ :| []     -> error "impossible"
            _ :| (x:xs) -> x :| xs
      }
    return doc

bookpart :: String -> (Ly.Part.Label -> RenderM Doc) -> RenderM Doc
bookpart title contents = do
    label <- newPartLabel
    withinScope "bookpart" $ mconcat [
        tocItem "\\tocBookpart" label title
      , nest $ contents label
      ]

-- | (Sub)section
--
-- Sections and subsections are a concept /we/ introduce; they do not exist as
-- an actual Lilypond structure.
section :: String -> (Ly.Part.Label -> RenderM Doc) -> RenderM Doc
section title contents = do
    label <- newPartLabel
    mconcat [
        tocItem "\\tocItem" label title
      , nest $ contents label
      ]

score :: String -> (Ly.Part.Label -> RenderM Doc) -> RenderM Doc
score title contents = do
    label <- newPartLabel
    mconcat [
        -- @tocItem@ must be outside the @score@ scope
        tocItem "\\tocItem" label title
      , nest $ contents label
      ]

tocItem :: String -> Ly.Part.Label -> String -> RenderM Doc
tocItem cmd = \label title ->
    line $ List.intercalate " " [
        cmd
      , renderLabelPath (Ly.Part.path label)
      , "\"" ++ Ly.Part.render label ++ ". " ++ title ++ "\""
      ]
  where
    renderLabelPath :: [Ly.Part.Label] -> String
    renderLabelPath = List.intercalate "." . map renderTocLabel

    renderTocLabel :: Ly.Part.Label -> String
    renderTocLabel label = Prelude.concat [
          "\"L"
        , List.intercalate "_" $ map show (Ly.Part.index label)
        , "\""
        ]

-- | Setup necessary Lilypond infrastructure to render the ToC
--
-- We use a different syntax only for the book part titles, and leave the rest
-- to Lilypond.
setupPaper :: Doc
setupPaper = Doc.lines [
      "\\paper {"
    , "  ragged-bottom = ##t"
    , "  ragged-last-bottom = ##t"
    , "  score-markup-spacing = #'((padding . 2))"
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

markup :: Ly.Markup -> RenderM Doc
markup x = do
    env <- WrapRenderM Reader.ask
    return $ Ly.Markup.render env.stylesheet x

{-------------------------------------------------------------------------------
  Context
-------------------------------------------------------------------------------}

data RenderEnv = RenderEnv{
      stylesheet :: Ly.Markup.Stylesheet
    }

initRenderContext :: Ly.Markup.Stylesheet -> RenderEnv
initRenderContext stylesheet = RenderEnv{
      stylesheet
    }

data RenderState = RenderState{
      -- | ToC label for the current section
      --
      -- Stored in reverse order (most nested level first)
      currTocLabel :: NonEmpty Int
    }

initRenderState :: RenderState
initRenderState = RenderState{
      currTocLabel = 0 :| []
    }

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

mapHead :: (a -> a) -> NonEmpty a -> NonEmpty a
mapHead f (x :| xs) = f x :| xs