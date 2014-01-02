{-# LANGUAGE MultiParamTypeClasses #-}
module Solver where
import Hand
import Control.Monad.State
import Data.List
import Data.List.Split
import Data.Functor
import Data.Array.IArray
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
    {deal :: Deal, trump :: Strain, lead :: Maybe Suit,
    hotseat :: Direction, highPlayer :: Maybe Direction, highCard :: Maybe Card, playCount :: Int,
    nsTricks :: Int}
    deriving (Eq, Show)

blockOutState (DDState d t l hs hp hc pc ns) center = intercalate "\n" $ combineBlocks [[info, north, none], [west, center, east], [none, south, none]]
    where none = blockOut [[]]
          info = blockOut ["Trump " ++ show1 t, "NS " ++ show ns]
          [north, east, south, west] = handBlocks <$> getHands d

initDDState d trump declarer = DDState d trump Nothing declarer Nothing Nothing 0 0
candidatePlays state = candidatePlaysH (getHand d h) c
    where d = deal state
          h = hotseat state
          c = lead state

-- card 1 is the current high card, card 2 is the played card
-- true if the current high card is higher. order matters
compareCards (Trump trump) (Card r1 s1) (Card r2 s2)
    | s1 == s2 = r1 > r2
    | trump == s2 = False
    | otherwise = True

compareCards Notrump (Card r1 s1) (Card r2 s2)
    | s1 == s2 = r1 > r2
    | otherwise = True

compareCardsM strain (Just c1) c2 = compareCards strain c1 c2
compareCardsM strain Nothing c2 = False

tryResolve dds@(DDState d t l hs hp hc pc tc)
    | pc `mod` 4 == 0 = DDState d t Nothing (fromJust hp) hp Nothing pc $ tc + 1 - (fromEnum . pair $ fromJust hp)
    | otherwise = dds

playCardS (DDState d t l hs hp hc pc ns) card@(Card cr cs) =
    tryResolve $ DDState (playCardD d hs card) t nl (rotate 1 hs) nhp nhc (pc + 1) ns
    where winner = compareCardsM t hc card
          nl = case l of
                Just _ -> l
                Nothing -> Just cs
          nhc = if winner then hc else Just card
          nhp = if winner then hp else Just hs

instance Hashable DDState where
    hashWithSalt i (DDState d t l hs hp hc pc ns) = hashWithSalt i (d, t, l, hs, hc, ns)

instance GameTree DDState Card Int DDState where
    key = id
    player d = case pair $ hotseat d of
        NorthSouth -> True
        EastWest -> False
    score = nsTricks
    isFinal = (== 12) . playCount
    moves dds = map movef $ candidatePlays dds
        where movef play = (play, playCardS dds play)

printStart p = blockOutState p $ blockOut [[]]

-- A trick: a set of 4 moves
printTrick pms =
    blockOutState finalp center
    where [firstp, _, _, finalp] = fst <$> pms
          firstMover = rotate (-1) $ hotseat firstp
          winner = fromJust $ highPlayer finalp
          movesByDir = take 4 . drop (4 - fromEnum firstMover) . cycle $ show . snd <$> pms
          moveMarker x i
              | i == firstMover = "<" ++ x ++ ">"
              | i == winner = "*" ++ x ++ "*"
              | otherwise = " " ++ x ++ " "
          [mn, me, ms, mw] = zipWith moveMarker movesByDir [North ..]
          moveStrs = [[], "     " ++ mn ++ "     ", unwords [mw, ms, me], []]
          center = blockOut moveStrs

showLine initp line = intercalate ("\n" ++ replicate 44 '=' ++ "\n") $ (printStart initp):(printTrick <$> chunksOf 4 line)