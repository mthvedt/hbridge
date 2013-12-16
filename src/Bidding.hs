module Bidding where
import Hand

hcpModel x = case x of
    12 -> 4
    11 -> 3
    10 -> 2
    9 -> 1
    _ -> 0