{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, RankNTypes #-}
module Solver.Generic where
import Data.List
import Data.Function
import Control.Monad
import Control.Monad.ST
import Data.STRef
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Basic as HST
import Data.Hashable

-- A two player zero sum game with alternating turns.
-- p: a position type
-- k: a position key
-- m: a move type
-- s: a score type
class (Eq k, Hashable k, Num s, Ord s) => GameTree p m s k | p -> m s k where
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
    -- Note that the GameTree typeclass doesn't establish a behavior for the move type.
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

instance Hashable Nim where
    hashWithSalt s (Nim p i) = hashWithSalt s (p, i)

type HashTable x k v = HST.HashTable x k v

scoref :: (GameTree p m s k) => (HashTable q k s -> p -> ST q s) -> HashTable q k s -> p -> ST q s
scoref f t pos
    | isFinal pos = return $ score pos
    | otherwise = do
        let k = key pos
        mr <- H.lookup t $ key pos
        case mr of
            Just r -> return r
            Nothing -> do
                let s = score pos
                H.insert t k s
                return s

minimax :: (GameTree p m s k) => HashTable q k s -> p -> ST q s
minimax = scoref $ \t pos -> liftM (if player pos then maximum else minimum) $ mapM (minimax t) $ children pos

solveWithM :: (GameTree p m s k) => (HashTable q k s -> p -> ST q s) -> HashTable q k s -> p -> ST q (p, m, s)
solveWithM solvef t pos = do
    solutions <- mapM (solvef t) $ children pos
    return $ (if player pos then maximumBy else minimumBy) (compare `on` (\(_, _, x) -> x))
        $ zipWith (\(m, p) s -> (p, m, s)) (moves pos) solutions

runSolveWith :: (GameTree p m s k) => (forall q. (HashTable q k s -> p -> ST q s)) -> p -> (p, m, s)
runSolveWith solvef pos = runST $ do
    t <- HST.new
    solveWithM solvef t pos

printPlayGame :: (GameTree p m s k, Show p, Show m, Show s) => p -> IO ()
printPlayGame pos =
    if isFinal pos
    then do
        putStrLn $ "Final position:\n" ++ show pos
        putStrLn $ "Final score: " ++ show (score pos)
    else do
        let (p, m, s) = runSolveWith minimax pos
        putStrLn $ "Position:\n" ++ show pos
        putStrLn $ "Move: " ++ show m
        -- putStrLn $ "Prinicpal variation: " ++ show ms
        -- putStrLn $ "Predicted score: " ++ show s
        printPlayGame $ p
