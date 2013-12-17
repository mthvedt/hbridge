module Bidding where
import Hand

gorenHCP x = case x of
    12 -> 4
    11 -> 3
    10 -> 2
    9 -> 1
    _ -> 0

hcpCount countf = gatherHand countf (+) 0
hcpCountDeal countf = mapDeal $ hcpCount countf