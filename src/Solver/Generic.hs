{-# LANGUAGE TypeFamilies, FlexibleContexts, RankNTypes #-}
module Solver.Generic where
import Data.List
import Data.Function
import Control.Monad
import Control.Monad.ST
import Data.STRef
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Basic as HST
import qualified Data.Map as Map
import Data.Hashable

-- A two player zero sum game with alternating turns.
-- p: a position type
-- k: a position key
-- m: a move type
-- s: a score type
class (Ord (Move p), Ord (Score p), Hashable (Key p), Eq (Key p)) => GameTree p where
    type Move p :: *
    type Score p :: *
    type Key p :: *
    -- The key for this position.
    -- Two positions with the same key are considered identical
    -- (but can have different scores).
    key :: p -> Key p
    player :: p -> Bool
    -- The goal of the True player is to maximize score, the False player to minimze it.
    score :: p -> Score p
    -- True if the game is over.
    isFinal :: p -> Bool
    isFinal = null . moves
    -- Given a position, returns the (move, position) pairs reachable.
    moves :: p -> [(Move p, p)]
    children :: p -> [p]
    children p = map snd $ moves p

class ShowBig p where
    showbig :: p -> String

movemap :: (GameTree p) => p -> Map.Map (Move p) p
movemap = Map.fromList . moves

data Nim = Nim Bool Int
    deriving (Eq, Show)

legal (Nim _ i) = i >= 0
move (Nim p i) j = Nim (not p) (i - j)

-- A test class for GameTree
instance GameTree Nim where
    type Move Nim = Int
    type Score Nim = Int
    type Key Nim = Nim
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
type GameTable q p = HashTable q (Key p) (Score p)

scoref :: (GameTree p) => (GameTable q p -> p -> ST q (Score p)) -> GameTable q p -> p -> ST q (Score p)
scoref f t pos
    | isFinal pos = return $ score pos
    | otherwise = do
        let k = key pos
        mr <- H.lookup t $ key pos
        case mr of
            Just r -> return r
            Nothing -> do
                -- let s = score pos
                s <- f t pos
                H.insert t k s
                return s
 
-- TODO does hashtable work this way?
minimax :: (GameTree p) => GameTable q p -> p -> ST q (Score p)
minimax = scoref $ \t pos -> liftM (if player pos then maximum else minimum) $ mapM (minimax t) $ children pos

solveWithM solvef t pos = do
    solutions <- mapM (solvef t) $ children pos
    return $ (if player pos then maximumBy else minimumBy) (compare `on` (\(_, _, x) -> x))
        $ zipWith (\(m, p) s -> (p, m, s)) (moves pos) solutions

runSolveWith :: (GameTree p) => (forall q. (GameTable q p -> p -> ST q (Score p))) -> p -> (p, Move p, Score p)
runSolveWith solvef pos = runST $ do
    t <- HST.new
    solveWithM solvef t pos

solveLineM solvef t pos = do
    (pos2, move, s) <- solveWithM solvef t pos
    let pm = (pos2, move)
    if isFinal pos2
        then return ([pm], s)
        else do (pms, s) <- solveLineM solvef t pos2
                return (pm:pms, s)

runSolveLine :: (GameTree p) => (forall q. (GameTable q p -> p -> ST q (Score p))) -> p -> ([(p, Move p)], Score p)
runSolveLine solvef pos = runST $ do
    t <- HST.new
    solveLineM solvef t pos

minimaxLine :: (GameTree p) => p -> ([(p, Move p)], Score p)
minimaxLine = runSolveLine minimax

printPosMove _ (p, m) = do
    putStrLn $ "Move: " ++ show m
    print p

printLine :: (GameTree p, Show p, Show (Move p), Show (Score p)) => p -> IO ()
printLine pos = do
    putStrLn $ show pos
    let (pms, score) = minimaxLine pos
    foldM printPosMove () pms
    putStrLn $ "Final score: " ++ show score
