{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, RankNTypes #-}
module Solver.Generic where
import Data.List
import Data.Function
import Control.Monad
import Control.Monad.ST
import Data.STRef

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

scoref f pos =
    if isFinal pos
    then score pos
    else f pos

scorefM :: (GameTree p m s k) => (p -> ST x s) -> p -> ST x s
scorefM f pos =
    if isFinal pos
    then return $ score pos
    else f pos

minimax :: (GameTree p m s k) => p -> s
minimax = scoref $ \pos -> maximum $ map maximin $ children pos

maximin :: (GameTree p m s k) => p -> s
maximin = scoref $ \pos -> maximum $ map minimax $ children pos

minimaxM :: (GameTree p m s k) => p -> ST x s
minimaxM = scorefM $ \pos -> liftM maximum $ mapM maximinM $ children pos

maximinM :: (GameTree p m s k) => p -> ST x s
maximinM = scorefM $ \pos -> liftM minimum $ mapM minimaxM $ children pos

solveWith :: (GameTree p m s k) => (p -> s) -> p -> (m, s)
solveWith solvef pos =
    (if player pos then maximumBy else minimumBy) (compare `on` snd) $ zip (map fst $ moves pos) (map solvef $ children pos)

solveWithM :: (GameTree p m s k) => (p -> ST x s) -> p -> ST x (m, s)
solveWithM solvef pos = do
    solutions <- mapM solvef $ children pos
    return $ (if player pos then maximumBy else minimumBy) (compare `on` snd) $ zip (map fst $ moves pos) solutions

runSolveWith :: (GameTree p m s k) => (forall x. (p -> ST x s)) -> p -> (m, s)
runSolveWith solvef pos = runST $ solveWithM solvef pos

-- playGame :: Nim -> IO ()

playGame pos =
    if isFinal pos
    then do
        putStrLn $ "Final position: " ++ show pos
        putStrLn $ "Final score: " ++ show (score pos)
    else do
        let (m, s) = runSolveWith minimaxM pos
        putStrLn $ "Position: " ++ show pos
        putStrLn $ "Move: " ++ show m
        -- putStrLn $ "Prinicpal variation: " ++ show ms
        -- putStrLn $ "Predicted score: " ++ show s
        playGame $ move pos m
