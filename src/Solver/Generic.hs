{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Solver.Generic where
import Data.List
import Data.Function

-- A game tree that is solvable using your typical tree solving algorithms.
-- An GameTree is completely specified by p, the game positions (= tree nodes).
-- p: a position type
-- k: a position key
-- m: a move type
-- s: a score type
-- v: a player score type
class (Eq k, Ord v) => GameTree p m s v k | p -> m s v k where
    -- The key for this position.
    -- Two positions with the same key are considered identical
    -- (but can have different scores).
    key :: p -> k
    -- A score is a value that represents the player score of various players.
    -- The goal of each player in a game is to maximize player score.
    score :: p -> s
    -- Given a score, yields the player score for the current player.
    -- 'Perfect play' is defined as the sequence of moves s.t. each step tries to maximize the final score.
    -- Some solution algorithms might also look at the current score for any position to try to exclude certain lines of play.
    scorePlayer :: p -> s -> v
    -- True if the game is over.
    isFinal :: p -> Bool
    -- Given a position, returns the (move, position) pairs reachable.
    -- Note that the OrderedSolvable typeclass doesn't establish a behavior for the move type.
    moves :: p -> [(m, p)]

data Nim = Nim Bool Int
    deriving (Eq, Show)

legal (Nim _ i) = i >= 0
move (Nim p i) j = Nim (not p) (i - j)

    -- A test class for GameTree
instance GameTree Nim Int Int Int Nim where
    key = id
    score (Nim p i) =
        case i of
            0 -> if p then 1 else -1
            1 -> if p then 1 else -1
            _ -> 0
    scorePlayer (Nim p _) i = if p then i else -i
    isFinal (Nim _ i) = i `elem` [0, 1]
    moves n = filter (legal . snd) $ map (\i -> (i, move n i)) [2, 3, 4]

-- minimax1 :: (GameTree p m s v k) => p -> s
minimax1 pos =
    if isFinal pos
    then score pos
    else maximumBy (compare `on` scorePlayer pos) $ map (minimax1 . snd) $ moves pos
    
-- minimax :: (GameTree p m s v k) =>  p -> (m, p)
minimax pos =
    maximumBy (compare `on` scorePlayer pos . minimax1 . snd) $ moves pos

-- minimaxP :: (GameTree p m s v k) =>  p -> ([m], s)
minimaxP pos =
    if isFinal pos
    then ([], score pos)
    else let scorer (m, p) = let (ms, s) = minimaxP p
                                 in (m:ms, s)
          in maximumBy (compare `on` scorePlayer pos . snd) $ map scorer $ moves pos

-- playGame :: Nim -> IO ()

playGame pos =
    if isFinal pos
    then do
        putStrLn $ "Final position: " ++ show pos
        putStrLn $ "Final score: " ++ show (score pos)
    else do
        let (m:ms, s) = minimaxP pos
        putStrLn $ "Position: " ++ show pos
        putStrLn $ "Move: " ++ show m
        putStrLn $ "Prinicpal variation: " ++ show ms
        putStrLn $ "Predicted score: " ++ show s
        playGame $ move pos m
