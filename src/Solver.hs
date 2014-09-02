{-# LANGUAGE MultiParamTypeClasses #-}
module Solver where
import Hand
import Data.List
import Data.List.Split
import Data.Functor
import Data.Array.IArray
import Solver.Generic
import Solver.Interactive
import Data.Maybe
import Data.Hashable

candidatePlaysH deal dir msuit =
    case concat $ map (\s -> map (\c -> Card (Rank c) s) $ candidatePlaysD deal dir s) suits of
        [] -> candidatePlaysH deal dir Nothing
        x -> x
    where suits = case msuit of
            (Just suit) -> [suit]
            Nothing -> reverse [Club ..]

data DDState = DDState
    {deal :: FastDeal, trump :: Strain, lead :: Maybe Suit,
    hotseat :: Direction, highPlayer :: Maybe Direction, highCard :: Maybe Card, playsLeft :: Int,
    nsTricks :: Int}
    deriving (Eq, Show)

blockOutState (DDState d t l hs hp hc pc ns) center = intercalate "\n" $ combineBlocks [[info, north, none], [west, center, east], [none, south, none]]
    where none = blockOut [[]]
          info = blockOut ["Trump " ++ show1 t, "NS " ++ show ns]
          [north, east, south, west] = handBlocks <$> getHands d

initDDState d trump declarer = DDState d trump Nothing declarer Nothing Nothing c 0
    -- where c = foldr (++) $ map length $ concatMap getSuits $ getHands d
    where c = sum $ handCount <$> getHands d

candidatePlays state = candidatePlaysH d h c
    where d = deal state
          h = hotseat state
          c = lead state

-- card 1 is the current high card, card 2 is the played card
-- true if the current high card is equal or higher. order matters
compareCards (Trump trump) (Card r1 s1) (Card r2 s2)
    | s1 == s2 = r1 >= r2
    | trump == s2 = False
    | otherwise = True

compareCards Notrump (Card r1 s1) (Card r2 s2)
    | s1 == s2 = r1 > r2
    | otherwise = True

compareCardsM strain (Just c1) c2 = compareCards strain c1 c2
compareCardsM strain Nothing c2 = False

tryResolve dds@(DDState d t l hs hp hc pl tc)
    | pl `mod` 4 == 0 = DDState d t Nothing (fromJust hp) hp Nothing pl $ tc + 1 - (fromEnum . pair $ fromJust hp)
    | otherwise = dds

playCardS (DDState d t l hs hp hc pl ns) card@(Card cr cs) =
    tryResolve $ DDState (playCardD d hs cs (unrank cr)) t nl (rotate 1 hs) nhp nhc (pl - 1) ns
    where winner = compareCardsM t hc card
          nl = case l of
                Just _ -> l
                Nothing -> Just cs
          nhc = if winner then hc else Just card
          nhp = if winner then hp else Just hs

newtype GameTreeKey = GameTreeKey (FastDeal, Maybe Suit, Maybe Card, Int)
    deriving (Eq)

instance Hashable GameTreeKey where
    hashWithSalt i (GameTreeKey t) = hashWithSalt i t

instance GameTree DDState Card Int GameTreeKey where
    key (DDState d t l hs hp hc pl ns) = GameTreeKey (d, l, hc, ns)
    player d = case pair $ hotseat d of
        NorthSouth -> True
        EastWest -> False
    score = nsTricks
    isFinal = (== 0) . playsLeft
    moves dds = map movef $ candidatePlays dds
        where movef play = (play, playCardS dds play)

instance InteractiveGame DDState Card Int GameTreeKey where
    acceptMove p s = Just p

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

-- TODO unshift cards
reconstructLine :: [(DDState, Card)] -> [(DDState, Card)]
reconstructLine [] = []
reconstructLine ((p0, m0):pms) = (unshiftS p0, m0):(reconstructLine npms)
    where npms = (\(p, m) -> (unshiftS p, (unshiftCard m0 m))) <$> pms
          unshiftS (DDState d t l hs hp hc pc ns) =
              DDState (unshiftD d m0) t l hs hp hc pc ns

showLine :: DDState -> [(DDState, Card)] -> String
showLine initp line = intercalate ("\n" ++ replicate 44 '=' ++ "\n") $ (printStart initp):(printTrick <$> chunksOf 4 (reconstructLine line))
