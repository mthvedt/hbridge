module Solver where
import Hand
import Control.Monad.State
import Data.List
import Data.Functor
import Data.Array
import Solver.Generic
import Data.Maybe

candidatePlaysH hand strain =
    concat $ map (\s -> map (\c -> Card c s) $ getSuit hand s) suits
    where suits = case strain of
            (Trump trump) -> (trump:(filter (trump /=) $ reverse [Club ..]))
            Notrump -> reverse [Club ..]

data DDState = DDState 
    {deal :: Deal, trump :: Strain,
    hotseat :: Direction, highPlayer :: Maybe Direction, highCard :: Maybe Card, playCount :: Int,
    nsTricks :: Int}
    deriving (Eq, Show)

initDDState d trump declarer = DDState d trump declarer Nothing Nothing 0 0
candidatePlays state = candidatePlaysH (getHand d h) t
    where d = deal state
          h = hotseat state
          t = trump state

compareCards (Trump trump) (Card r1 s1) (Card r2 s2)
    | s1 == s2 = r1 > r2
    | trump == s2 = False
    | otherwise = True

compareCards Notrump (Card r1 s1) (Card r2 s2)
    | s1 == s2 = r1 > r2
    | otherwise = True

compareCardsM strain c1 (Just c2) = compareCards strain c1 c2
compareCardsM strain c1 Nothing = True

tryResolve (DDState d t hs hp hc 4 tc) =
    DDState d t hs Nothing Nothing 0 $ tc + 1 - (fromEnum . pair $ fromJust hp)

tryResolve x = x

playCardS (DDState d t hs hp hc pc ns) card =
    -- todo: resolve trick
    tryResolve $ DDState (playCardD d hs card) t (rotate hs) nhp nhc (pc + 1) ns
    where winner = compareCardsM t card hc
          nhc = if winner then Just card else hc
          nhp = if winner then Just hs else hp

data DDLine = DDLine {state :: DDState, plays :: [Card]}
    deriving (Eq, Show)

initDDLine deal trump declarer = DDLine (DDState deal trump declarer Nothing Nothing 0 0) []

-- data SolverState = SolverState {nodeCount :: Int}