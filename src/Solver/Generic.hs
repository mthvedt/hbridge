{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Solver.Generic where

-- TODO eq key?
class (Eq k, Ord s) => OrderedSolvable b s k | b -> s k where
    key :: b -> k
    rank :: b -> s
    isFinal :: b -> Bool
    moves :: b -> [b]

data Nim = Nim {player :: Bool, nimber :: Int}
    deriving (Eq, Show)

    -- A test class for OrderedSolvable
instance OrderedSolvable Nim Int Nim where
    key = id
    rank (Nim p i) =
        case i of
            0 -> if p then 1 else -1
            _ -> 0
    isFinal (Nim _ i) = i == 0
    moves (Nim b i) = map (Nim $ not b) $ filter (>= 0) [(i - 2), (i - 3), (i - 4)]