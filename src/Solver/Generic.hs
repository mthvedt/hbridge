{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Solver.Generic where
import Data.List
import Data.Function

-- TODO eq key?
class (Eq k, Ord v) => OrderedSolvable p m s v k | p -> m s v k where
    key :: p -> k
    score :: p -> s
    scorePlayer :: p -> s -> v
    isFinal :: p -> Bool
    moves :: p -> [(m, p)]

data Nim = Nim Bool Int
    deriving (Eq, Show)

legal (Nim _ i) = i >= 0
move (Nim p i) j = Nim (not p) (i - j)

    -- A test class for OrderedSolvable
instance OrderedSolvable Nim Int Int Int Nim where
    key = id
    score (Nim p i) =
        case i of
            0 -> if p then 1 else -1
            1 -> if p then 1 else -1
            _ -> 0
    scorePlayer (Nim p _) i = if p then i else -i
    isFinal (Nim _ i) = i `elem` [0, 1]
    moves n = filter (legal . snd) $ map (\i -> (i, move n i)) [2, 3, 4]

-- minimax1 :: (OrderedSolvable pos m s v k) => pos -> s
minimax1 pos =
    if isFinal pos
    then score pos
    else maximumBy (compare `on` scorePlayer pos) $ map (minimax1 . snd) $ moves pos
    
-- minimax :: (OrderedSolvable p m s k) => Bool -> p -> (m, p)
minimax pos =
    maximumBy (compare `on` scorePlayer pos . minimax1 . snd) $ moves pos

-- playGame :: Nim -> IO ()
playGame pos =
    if isFinal pos
    then do
        putStrLn $ "Final position: " ++ show pos
        putStrLn $ "Final score: " ++ show (score pos)
    else do
        let (m, newPos) = minimax pos
        putStrLn $ "Position: " ++ show pos
        putStrLn $ "Move: " ++ show m
        playGame newPos