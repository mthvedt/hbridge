{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Solver.Generic where
import Data.List
import Data.Function

-- A two player zero sum game with alternating turns.
-- p: a position type
-- k: a position key
-- m: a move type
-- s: a score type
class (Eq k, Num s, Ord s) => GameTree p m s k | p -> m s k where
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
    children :: p -> [p]
    children p = map snd $ moves p

data Nim = Nim Bool Int
    deriving (Eq, Show)

legal (Nim _ i) = i >= 0
move (Nim p i) j = Nim (not p) (i - j)

    -- A test class for GameTree
instance GameTree Nim Int Int Nim where
    key = id
    player (Nim p _) = p
    score (Nim p i) =
        case i of
            0 -> if p then 1 else -1
            1 -> if p then -1 else 1
            _ -> 0
    isFinal (Nim _ i) = i `elem` [0, 1]
    moves n = filter (legal . snd) $ map (\i -> (i, move n i)) [2, 3, 4]
    
-- minimax1 :: (GameTree p m s k) => p -> s
minimax pos =
    if isFinal pos
    then score pos
    else maximum $ map maximin $ children pos

maximin pos =
    if isFinal pos
    then score pos
    else maximum $ map minimax $ children pos

solveAlphaBeta1 alpha beta (p:ps) =
    case ps of
        [] -> knownscore
        _ -> if newbeta <= newalpha
             then knownscore
             else scorecmp pscore $ solveAlphaBeta1 newalpha newbeta ps
    where scorecmp = if player p then max else min
          currscore = score p
          pscore = if isFinal p then currscore else solveAlphaBeta1 alpha beta $ children p
          newalpha = if player p then max alpha pscore else alpha
          newbeta = if not $ player p then min beta pscore else beta
          knownscore = if player p then alpha else beta

solveWith :: (GameTree p m s k) => (p -> s) -> p -> (m, s)
solveWith solvef pos =
    (if player pos then maximumBy else minimumBy) (compare `on` snd) $ zip (map fst $ moves pos) (map solvef $ children pos)

-- playGame :: Nim -> IO ()

playGame pos =
    if isFinal pos
    then do
        putStrLn $ "Final position: " ++ show pos
        putStrLn $ "Final score: " ++ show (score pos)
    else do
        let (m, s) = solveWith minimax pos
        putStrLn $ "Position: " ++ show pos
        putStrLn $ "Move: " ++ show m
        -- putStrLn $ "Prinicpal variation: " ++ show ms
        -- putStrLn $ "Predicted score: " ++ show s
        playGame $ move pos m
