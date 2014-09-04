{-# LANGUAGE ExistentialQuantification, TypeFamilies, FlexibleContexts, RankNTypes #-}
module Solver.Generic where
import Data.List
import Data.Function
import Data.Functor
import qualified Data.Maybe
import Control.Monad
import Control.Monad.ST
import Data.STRef
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Basic as HST
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Hashable

-- A two player zero sum game with alternating turns.
-- p: a position type
-- k: a position key
-- m: a move type
-- s: a score type
-- TODO eliminate Ord (Move p)
class (Ord (Move p), Ord (Score p), Hashable (Key p), Eq (Key p)) => GameTree p where
    type Move p :: *
    type Score p :: *
    type Key p :: *
    -- The key for this position.
    -- Two positions with the same key are considered identical
    -- (but can have different scores).
    key :: p -> Key p
    player :: p -> Bool
    -- The goal of the True player is to maximize score, the False player to minimze it
    score :: p -> Score p
    -- True if the game is over.
    isFinal :: p -> Bool
    isFinal = null . moves
    -- Given a position, returns the (move, position) pairs reachable.
    moves :: p -> [(Move p, p)]
    children :: p -> [p]
    children p = map snd $ moves p

makeMove :: (GameTree p) => p -> Move p -> Maybe p
makeMove p m = lookup m $ moves p

-- A class to represent GameTrees that 'lose' information as they are played.
-- A Tracable lets us 'reconstruct' some type from a series of moves.
class (GameTree p) => Tracable p where
    type TMove p :: *
    type Target p :: *
    traceInit :: Target p -> p
    -- detrace: Reconstruct a game
    detrace :: p -> [Move p] -> Target p
    -- tracemove: Break down a move given an original position and a line of play
    traceMove :: TMove p -> Target p -> [Move p] -> Move p
    -- detrace move: Reconstruct a move given a line of play
    detraceMove :: Move p -> [Move p] -> TMove p

newtype TrivialTracable g = TrivialTracable g

instance (GameTree g) => GameTree (TrivialTracable g) where
    type Move (TrivialTracable g) = Move g
    type Key (TrivialTracable g) = Key g
    type Score (TrivialTracable g) = Score g
    key (TrivialTracable g) = key g
    player (TrivialTracable g) = player g
    score (TrivialTracable g) = score g
    isFinal (TrivialTracable g) = isFinal g
    moves (TrivialTracable g) = (\(m, p) -> (m, TrivialTracable p)) <$> moves g
    children (TrivialTracable g) = TrivialTracable <$> children g

instance (GameTree g) => Tracable (TrivialTracable g) where
    type TMove (TrivialTracable g) = Move g
    type Target (TrivialTracable g) = g
    traceInit = TrivialTracable
    detrace (TrivialTracable g) _ = g
    traceMove m _ _ = m
    detraceMove m _ = m

data Line p = Line { lpos :: p, origpos :: Target p, moveseq :: Seq.Seq (Move p) }
newline :: (Tracable g) => Target g -> Line g
newline g = Line (traceInit g) g Seq.empty

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
type SolverF p = forall q. GameTable q p -> p -> ST q (Score p)

memoScore :: (GameTree p) => SolverF p -> GameTable q p -> p -> ST q (Score p)
memoScore f t pos
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
-- The solver fn only worries about scores. Once scores are figured out
-- looking up the moves is sufficiently fast
minimax1 :: (GameTree p) => GameTable q p -> p -> ST q (Score p)
minimax1 t pos = optf scoredPositions
    where optf :: (Monad m, Ord o) => m [o] -> m o
          optf = liftM $ if player pos then maximum else minimum
          scoredPositions = mapM (minimax t) $ children pos
          
minimax :: (GameTree p) => GameTable q p -> p -> ST q (Score p)
minimax = memoScore minimax1

solveWithM :: (GameTree p) => SolverF p -> GameTable q p -> p -> ST q (Move p, Score p)
solveWithM solvef t pos =
    -- Assumed that this will populate our memo table with solutions
    let optby = if player pos then maximumBy else minimumBy
        scoreMoveM t (m, p2) = do
            s <- solvef t p2
            return (m, s)
    in do scoredMoves <- mapM (scoreMoveM t) $ moves pos
          return $ optby (compare `on` snd) scoredMoves

runSolveWith :: (GameTree p) => SolverF p -> p -> (Move p, Score p)
runSolveWith solvef pos = runST $ do
    t <- HST.new
    solveWithM solvef t pos

solveLineM :: (GameTree p) => SolverF p -> GameTable q p -> p ->
                  ST q ([Move p], Score p)
solveLineM solvef t pos = do
    (move, s) <- solveWithM solvef t pos
    let pos2 = Data.Maybe.fromJust $ lookup move $ moves pos
    if isFinal pos2
        then return ([move], s)
        else do (moves, s) <- solveLineM solvef t pos2
                return (move:moves, s)

runSolveLine :: (GameTree p) => SolverF p -> p -> ([Move p], Score p)
runSolveLine solvef pos  = runST $ do
    t <- HST.new
    solveLineM solvef t pos

-- TODO does the hash table even do anything? need a SolverMonad
-- TODO NEXT instead return a line
--minimaxLine :: (GameTree p) => p -> Line p
minimaxLine :: (GameTree p) => p -> ([Move p], Score p)
minimaxLine = runSolveLine minimax

printPosMove p m = do
    putStrLn $ "Move: " ++ show m
    print p
    return $ Data.Maybe.fromJust $ makeMove p m

printLine :: (GameTree p, Show p, Show (Move p), Show (Score p)) => p -> IO ()
printLine pos = do
    putStrLn $ show pos
    let (moves, score) = minimaxLine pos
    foldM printPosMove pos moves
    putStrLn $ "Final score: " ++ show score
