-- | Parts of a Lilypond document
--
-- Intended for qualified import.
--
-- > import Lilypond.Part qualified as Ly.Part
module Lilypond.Part (
    Label(..)
    -- * Query
  , index
  , path
  , render
  ) where

import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE

{-------------------------------------------------------------------------------
  Basic definitions
-------------------------------------------------------------------------------}

-- | Part label
--
-- Stored in reverse order: most deeply nested section first.
newtype Label = Label (NonEmpty Int)
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | Document part index
--
-- For example:
--
-- > Document structure    'index'
-- > ----------------------------
-- > First part            [1]
-- >   First subpart       [1, 1]
-- >   Second subpart      [1, 2]
-- > Second part           [2]
-- >   First subpart       [2, 1]
-- >   Second subpart      [2, 2]
-- >   Third subpart       [2, 3]
index :: Label -> [Int]
index (Label xs) = reverse $ NE.toList xs

-- | Labels of all enclosing parts, from root to most deeply nested
path :: Label -> [Label]
path (Label xs) = reverse . map Label . NE.toList $ NE.tails1 xs

-- | Render as a string to be used in titles
render :: Label -> String
render label = List.intercalate "." (map show $ index label)
