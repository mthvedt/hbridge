{-# LANGUAGE MultiParamTypeClasses #-}
module Solver where
import Hand
import Control.Monad.State
import Data.List
import Data.Functor
import Data.Array
import Solver.Generic
import Data.Maybe
import Data.Hashable

candidatePlaysH hand msuit =
    case concat $ map (\s -> map (\c -> Card c s) $ getSuit hand s) suits of
        [] -> candidatePlaysH hand Nothing
        x -> x
    where suits = case msuit of
            (Just suit) -> [suit]
            Nothing -> reverse [Club ..]

data DDState = DDState 
    {deal :: Deal, trump :: Strain,
    hotseat :: Direction, highPlayer :: Maybe Direction, highCard :: Maybe Card, playCount :: Int,
    nsTricks :: Int}
    deriving (Eq)

instance Show DDState where
    show (DDState d t hs hp hc pc ns) = intercalate "\n" $ combineBlocks [[info, north, none], [east, center, west], [none, south, none]]
        where none = blockOut [[]]
              info = blockOut [[]]
              center = blockOut [[]]
              [north, east, south, west] = handBlocks <$> getHands d

initDDState d trump declarer = DDState d trump declarer Nothing Nothing 0 0
candidatePlays state = candidatePlaysH (getHand d h) c
    where d = deal state
          h = hotseat state
          c = suit `liftM` highCard state

compareCards (Trump trump) (Card r1 s1) (Card r2 s2)
    | s1 == s2 = r1 > r2
    | trump == s2 = False
    | otherwise = True

compareCards Notrump (Card r1 s1) (Card r2 s2)
    | s1 == s2 = r1 > r2
    | otherwise = True

compareCardsM strain c1 (Just c2) = compareCards strain c1 c2
compareCardsM strain c1 Nothing = True

tryResolve dds@(DDState d t hs hp hc pc tc)
    | pc `mod` 4 == 0 = DDState d t (fromJust hp) Nothing Nothing pc $ tc + 1 - (fromEnum . pair $ fromJust hp)
    | otherwise = dds

playCardS (DDState d t hs hp hc pc ns) card =
    -- todo: resolve trick
    tryResolve $ DDState (playCardD d hs card) t (rotate hs) nhp nhc (pc + 1) ns
    where winner = compareCardsM t card hc
          nhc = if winner then Just card else hc
          nhp = if winner then Just hs else hp

instance Hashable DDState where
    hashWithSalt i (DDState a b c d e f g) = hashWithSalt i (a, b, c, d, e, f, g)

instance GameTree DDState Card Int DDState where
    key = id
    player d = case pair $ hotseat d of
        NorthSouth -> True
        EastWest -> False
    score = nsTricks
    isFinal = (== 52) . playCount
    moves dds = map movef $ candidatePlays dds
        where movef play = (play, playCardS dds play)

data DDLine = DDLine {state :: DDState, plays :: [Card]}
    deriving (Eq, Show)

initDDLine deal trump declarer = DDLine (DDState deal trump declarer Nothing Nothing 0 0) []

-- data SolverState = SolverState {nodeCount :: Int}