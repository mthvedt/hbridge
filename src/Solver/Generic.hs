{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Solver.Generic where
import Data.List
import Data.Function

-- A two player zero sum game with alternating turns.
-- p: a position type
-- k: a position key
-- m: a move type
-- s: a score type
class (Eq k, Ord s) => GameTree p m s k | p -> m s k where
    -- The key for this position.
    -- Two positions with the same key are considered identical
    -- (but can have different scores).
    key :: p -> k
    player :: p -> Bool
    -- The goal of the True player is to maximize score, the False player to minimze it.
    score :: p -> s
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
instance GameTree Nim Int Ordering Nim where
    key = id
    player (Nim p _) = p
    score (Nim p i) =
        case i of
            0 -> if p then GT else LT
            1 -> if p then LT else GT
            _ -> EQ
    isFinal (Nim _ i) = i `elem` [0, 1]
    moves n = filter (legal . snd) $ map (\i -> (i, move n i)) [2, 3, 4]

-- minimax1 :: (GameTree p m s k) => p -> s
minimax1 pos =
    if isFinal pos
    then score pos
    else (if player pos then maximum else minimum) $ map (minimax1 . snd) $ moves pos
    
-- minimax :: (GameTree p m s k) =>  p -> (m, p)
minimax pos =
    (if player pos then maximumBy else minimumBy) (compare `on` minimax1 . snd) $ moves pos

minimaxP :: (GameTree p m s k) =>  p -> ([m], s)
minimaxP pos =
    if isFinal pos
    then ([], score pos)
    else let scorer (m, p) = let (ms, s) = minimaxP p
                                 in (m:ms, s)
          in (if player pos then maximumBy else minimumBy) (compare `on` snd) $ map scorer $ moves pos

-- solveAlphaBeta1 pos alpha beta =

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
