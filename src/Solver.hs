module Solver where
import Hand
import Control.Monad.State
import Data.List
import Data.Functor
import Data.Array
import Solver.Generic

candidatePlaysH hand strain =
    concat $ map (\s -> map (\c -> Card c s) $ getSuit hand s) suits
    where suits = case strain of
            (Trump trump) -> (trump:(filter (trump /=) $ reverse [Club ..]))
            Notrump -> reverse [Club ..]

data DDState = DDState 
    {deal :: Deal, trump :: Strain,
    hotseat :: Direction, highPlayer :: Maybe Direction, highCard :: Maybe Card, playCount :: Int}
    deriving (Eq, Show)

initDDState d trump declarer = DDState d trump declarer Nothing Nothing 0
candidatePlays state = candidatePlaysH (getHand d h) t
    where d = deal state
          h = hotseat state
          t = trump state

playCardS (DDState d t hs hp hc pc) card =
    -- todo: resolve trick
    DDState (playCardD d hs card) t (rotate hs) nhp nhc (pc + 1)
    where nhp = hp
          nhc = hc -- TODO

data DDLine = DDLine {state :: DDState, plays :: [Card], nsTricks :: Int}
    deriving (Eq, Show)

initDDLine deal trump declarer = DDLine (DDState deal trump declarer Nothing Nothing 0) [] 0

-- data SolverState = SolverState {nodeCount :: Int}