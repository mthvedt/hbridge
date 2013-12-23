{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Solver.Generic where
import Data.List
import Data.Function

-- TODO eq key?
class (Eq k, Ord s) => OrderedSolvable p m s k | p -> m s k where
    key :: p -> k
    rank :: p -> s
    isFinal :: p -> Bool
    moves :: p -> [(m, p)]

data Nim = Nim {player :: Bool, nimber :: Int}
    deriving (Eq, Show)

legal (Nim _ i) = i >= 0
move (Nim p i) j = Nim (not p) (i - j)

    -- A test class for OrderedSolvable
instance OrderedSolvable Nim Int Int Nim where
    key = id
    rank (Nim p i) =
        case i of
            0 -> if p then 1 else -1
            _ -> 0
    isFinal (Nim _ i) = i `elem` [0, 1]
    moves n = filter (legal . snd) $ map (\i -> (i, move n i)) [2, 3, 4]

-- minimax1 :: (OrderedSolvable p m s k) => Bool -> p -> s
minimax1 player pos =
    if isFinal pos
    then rank pos
    else (if player then maximum else minimum) $ map (minimax1 (not player) . snd) $ moves pos
    
-- minimax :: (OrderedSolvable p m s k) => Bool -> p -> (m, p)
minimax player pos =
    (if player then maximumBy else minimumBy) (compare `on` minimax1 (not player) . snd) $ moves pos

playGame :: Nim -> IO ()
playGame pos =
    if isFinal pos
    then do
        putStrLn $ "Final position: " ++ show pos
        putStrLn $ "Final score: " ++ show (rank pos)
        putStrLn $ "Winner: " ++ show (player pos)
    else do
        let (m, newPos) = minimax (player pos) pos
        putStrLn $ "Position: " ++ show pos
        putStrLn $ "Move: " ++ show m
        playGame newPos