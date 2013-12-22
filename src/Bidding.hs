module Bidding where
import Hand
import Data.Array
import Data.Functor
import qualified Data.Foldable as F
import Data.Traversable

gorenHCP x = case x of
    12 -> 4
    11 -> 3
    10 -> 2
    9 -> 1
    _ -> 0

hcpCountCards countf ints = F.foldl (+) 0 $ fmap countf ints
hcpCountHand countf (Hand ss) = hcpCountCards countf $ F.foldl (++) [] ss
hcpCountDeal countf (Deal hs) = fmap (hcpCountHand countf) hs