module MusicTheory.Util.Foldable (
    length
  ) where

import Prelude hiding (length)
import Prelude qualified

length :: Foldable f => f a -> Word
length = fromIntegral . Prelude.length
