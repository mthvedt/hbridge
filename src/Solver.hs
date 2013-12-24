module Solver where
import Hand
import Control.Monad.State
import Data.List
import Data.Functor
import Data.Array
import Solver.Generic

candidatePlays hand trump =
    concat $ map (\s -> map (\c -> Card c s) $ getSuit hand s) (trump:(filter (trump /=) $ reverse [Club ..]))

data DDState = DDState 
    {deal :: Deal, trump :: Strain,
    hotseat :: Direction, highPlayer :: Maybe Direction, highCard :: Maybe Card, playCount :: Int}
    deriving (Eq, Show)

data DDLine = DDLine {state :: DDState, plays :: [Card], nsTricks :: Int}
    deriving (Eq, Show)

initDDLine deal trump declarer = DDLine (DDState deal trump declarer Nothing Nothing 0) [] 0

-- data SolverState = SolverState {nodeCount :: Int}