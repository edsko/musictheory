-- | Miscellaneous utilities
module MusicTheory.Util (
    shiftIntegral
  , minimize
  ) where

import Data.Map (Map)
import Data.Map qualified as Map

-- | Shift integral value by specified delta
--
-- The point is that this allows to shift unsigned values down:
--
-- > shiftIntegral (-3) (4 :: Word)
shiftIntegral :: (Integral b, Integral a) => b -> a -> a
shiftIntegral delta x = fromInteger $ toInteger x + toInteger delta

-- | Find the unique option that minimizes the given measure
--
-- > minimize (fromIntegral . length) ["abc", "de", "fghi"] == "de"
--
-- Throws an exception if there is no unique answer.
--
-- > minimize (fromIntegral . length) ["abc", "de", "fghi", "XY"]
--
-- throws
--
-- > "*** Exception: no unique best option: ["XY","de"]
minimize :: forall a. Show a => (a -> Word) -> [a] -> a
minimize f xs =
    case Map.lookupMin xs' of
      Nothing        -> error "empty list"
      Just (_, [x])  -> x
      Just (_, opts) -> error $ "no unique best option: " ++ show opts
  where
    xs' :: Map Word [a]
    xs' = Map.fromListWith (++) $ map (\x -> (f x, [x])) xs
