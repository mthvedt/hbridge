{-# LANGUAGE TypeFamilies, FlexibleContexts, ExistentialQuantification, RankNTypes #-}
module Solver.Generic where
import Data.List
import qualified Data.Foldable as F
import Data.Function
import Data.Functor
import qualified Data.Maybe
import qualified Control.Category as C
import Control.Monad
import Control.Monad.ST
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Basic as HST
import qualified Data.Sequence as Seq
import Data.Hashable

-- TODO how it should be:
-- A 'reduced position' that can be built from a normal game.
-- p -> (rp, rs) where s is 'reconstruction information' that can rebuild the orig game

-- A two player zero sum game with alternating turns.
class (Real (Score p)) => Game p where
    data Move p :: *
    type Score p :: *
    -- TODO player type
    player :: p -> Bool
    -- The goal of the True player is to maximize score, the False player to minimze it
    -- Score tells us how the score has *changed* since the last position
    score :: p -> Score p
    isFinal :: p -> Bool
    move :: p -> Move p -> Maybe p

class (Game p, Hashable (Key p), Eq (Key p)) => Solvable p where
    -- The key for this position.
    -- Two positions with the same key are considered identical
    -- (but can have different scores).
    type Key p :: *
    key :: p -> Key p
    -- Given a position, returns the (move, position) pairs reachable.
    moves :: p -> [(Move p, p)]
    -- Children: a potentially faster implementation of moves
    children :: p -> [p]
    children p = map snd $ moves p

fmove :: (Game p) => p -> Move p -> p
fmove p m = Data.Maybe.fromJust $ move p m

data View p0 p1 = View (p0 -> (p1, Move p1 -> Move p0))
trivialView :: View p p
trivialView = View $ \p -> (p, id)

instance C.Category View where
    id = trivialView
    (.) = flip composeView

composeView :: View p0 p1 -> View p1 p2 -> View p0 p2
composeViewH v0 v1 p0 = (p2, f0 . f1)
    where (p1, f0) = v0 p0
          (p2, f1) = v1 p1
composeView (View v0) (View v1) = View $ \p -> composeViewH v0 v1 p

data Line p = Line { lpos :: p, moveseq :: Seq.Seq (Move p) }
newline :: (Game g) => g -> Line g
newline g = Line g Seq.empty

lineView :: View (Line p) p
lineView = View $ \l -> (lpos l, LMove)

instance (Game g) => Game (Line g) where
    newtype Move (Line g) = LMove (Move g)
    type Score (Line g) = Score g
    player = player . lpos
    score = score . lpos
    isFinal = isFinal . lpos
    move (Line p ms) (LMove m) = do
        p2 <- move p m
        return $ Line p2 $ ms Seq.|> m

data Nim = Nim Bool Int
    deriving (Eq, Show)

legal (Nim _ i) = i >= 0
mnimmove (Nim p i) j = Nim (not p) (i - j)

-- A test class 
instance Game Nim where
    newtype Move Nim = NimMove Int
    type Score Nim = Int
    player (Nim p _) = p
    score (Nim p i) =
        case i of
            0 -> if p then 1 else -1
            1 -> if p then -1 else 1
            _ -> 0
    isFinal (Nim _ i) = i `elem` [0, 1]
    move p (NimMove i) = do
        let p2 = mnimmove p i
        guard $ legal p2
        return p2

instance Solvable Nim where
    type Key Nim = Nim
    key = id
    moves n = filter (legal . snd) $ map (\i -> (NimMove i, mnimmove n i)) [2, 3, 4]

instance Hashable Nim where
    hashWithSalt s (Nim p i) = hashWithSalt s (p, i)

type HashTable x k v = HST.HashTable x k v
type GameTable q p = HashTable q (Key p) (Score p)
type SolverF p = forall q. GameTable q p -> p -> ST q (Score p)

memoScore :: (Solvable p) => SolverF p -> GameTable q p -> p -> ST q (Score p)
memoScore f t pos
    | isFinal pos = return $ score pos
    | otherwise = do
        let k = key pos
        mr <- H.lookup t $ key pos
        case mr of
            Just r -> return r
            Nothing -> do
                s <- f t pos
                H.insert t k s
                return s
        
-- TODO does hashtable work this way?
-- The solver fn only worries about scores. Once scores are figured out
-- looking up the moves is sufficiently fast
minimax1 :: (Solvable p) => GameTable q p -> p -> ST q (Score p)
minimax1 t pos = do
    let optf = liftM $ if player pos then maximum else minimum
        scoredPositions = mapM (minimax t) $ children pos
    childScore <- optf scoredPositions
    return $ childScore + score pos
          
minimax :: (Solvable p) => GameTable q p -> p -> ST q (Score p)
minimax = memoScore minimax1

solveWithM :: (Solvable p) => SolverF p -> GameTable q p -> p -> ST q (Move p, Score p)
solveWithM solvef t pos =
    -- Assumed that this will populate our memo table with solutions
    let optby = if player pos then maximumBy else minimumBy
        scoreMoveM t (m, p2) = do
            s <- solvef t p2
            return (m, s)
    in do scoredMoves <- mapM (scoreMoveM t) $ moves pos
          return $ optby (compare `on` snd) scoredMoves

runSolveWith :: (Solvable p) => SolverF p -> p -> (Move p, Score p)
runSolveWith solvef pos = runST $ do
    t <- HST.new
    solveWithM solvef t pos

solveLineM :: (Solvable p) => SolverF p -> GameTable q p -> p ->
                  ST q ([Move p], Score p)
solveLineM solvef t pos = do
    (move, s) <- solveWithM solvef t pos
    let pos2 = fmove pos move
    if isFinal pos2
        then return ([move], s)
        else do (moves, s) <- solveLineM solvef t pos2
                return (move:moves, s)

runSolveLine :: (Solvable p) => SolverF p -> p -> ([Move p], Score p)
runSolveLine solvef pos  = runST $ do
    t <- HST.new
    solveLineM solvef t pos

-- TODO does the hash table even do anything? need a SolverMonad
-- TODO NEXT instead return a line
minimaxLine :: (Solvable p) => p -> ([Move p], Score p)
minimaxLine = runSolveLine minimax

printPosMove p m = do
    putStrLn $ "Move: " ++ show m
    print p
    return $ fmove p m

-- TODO use a line here instead
printLine :: (Solvable p, Show p, Show (Move p), Show (Score p)) => p -> IO ()
printLine pos = do
    putStrLn $ show pos
    let (moves, score) = minimaxLine pos
    foldM printPosMove pos moves
    putStrLn $ "Final score: " ++ show score
