{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Solver.Generic where
import Data.List
import Data.Function
import Control.Monad.ST
import qualified Data.HashTable.Class as HC
import qualified Data.HashTable.ST.Basic as HST

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

class (Eq k) => MemoTable t k s | t -> k s where
    saveT :: t -> k -> s -> t
    lookupT :: t -> k -> Maybe s

-- Trivial memo table
data Void = Void
instance MemoTable Void Nim Int where
    saveT t _ _ = t
    lookupT _ _ = Nothing

doMemo :: (GameTree p m s k, MemoTable t k s) => (t -> p -> (s, t)) -> t -> p -> (s, t)
doMemo f t p = case lookupT t k of
    Just r -> (r, t)
    Nothing -> (r, saveT t2 k r)
        where (r, t2) = f t p
    where k = key p

mapHelper :: (GameTree p m s k, MemoTable t k s) => (t -> p -> (s, t)) -> t -> [p] -> ([s], t)
mapHelper f t (p:ps) = (s:ss, t3)
    where (s, t2) = doMemo f t p
          (ss, t3) = case ps of
                [] -> ([], t2)
                _ -> mapHelper f t2 ps
    
minimax :: (GameTree p m s k, MemoTable t k s) => t -> p -> (s, t)
minimax t pos =
    if isFinal pos
    then (score pos, t)
    else (maximum ss, t2) 
    where (ss, t2) = mapHelper maximin t $ children pos

maximin t pos =
    if isFinal pos
    then (score pos, t)
    else (minimum ss, t2) 
    where (ss, t2) = mapHelper minimax t $ children pos

solveWith :: (GameTree p m s k, MemoTable t k s) => t -> (t -> p -> (s, t)) -> p -> (m, s)
solveWith t solvef pos =
    maximumBy (compare `on` snd) $ zip (map fst $ moves pos) (fst $ mapHelper solvef t $ children pos)

-- playGame :: Nim -> IO ()

playGame pos =
    if isFinal pos
    then do
        putStrLn $ "Final position: " ++ show pos
        putStrLn $ "Final score: " ++ show (score pos)
    else do
        putStrLn $ "Position: " ++ show pos
        let (m, s) = solveWith Void minimax pos
        putStrLn $ "Move: " ++ show m
        -- putStrLn $ "Prinicpal variation: " ++ show ms
        -- putStrLn $ "Predicted score: " ++ show s
        playGame $ move pos m
