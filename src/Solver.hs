module Solver where
import Hand
import Control.Monad.State

data DDState = DDState 
    {deal :: Deal, trump :: Suit,
    hotseat :: Direction, highPlayer :: Maybe Direction, highCard :: Maybe Card, playCount :: Int}

candidatePlays hand trump =
    concat $ map (\s -> map (\c -> Card c s) $ getSuit hand s) (trump:(filter (\x -> x /= trump) [Spade, Heart, Diamond, Club]))

data DDLine = DDLine {state :: DDState, plays :: [Card], nsTricks :: Int}

initDDLine deal trump declarer = DDLine (DDState deal trump declarer Nothing Nothing 0) [] 0

-- data SolverState = SolverState {nodeCount :: Int}