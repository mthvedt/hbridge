{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module Solver where
import Hand
import Data.List
import Data.List.Split
import Data.Functor
import Solver.Generic
import Solver.Interactive
import Data.Maybe
import Data.Hashable
import qualified Data.Bimap as BM

candidatePlaysH :: IDeal d => d -> Direction -> Maybe Suit -> [Card]
candidatePlaysH d dir msuit =
    case concat $ map (\s -> map (\c -> Card (Rank c) s) $ candidatePlaysD d dir s) suits of
        [] -> candidatePlaysH d dir Nothing
        x -> x
    where suits = case msuit of
            (Just s) -> [s]
            Nothing -> reverse [Club ..]

data DDState = DDState
    {deal :: FastDeal, trump :: Strain, lead :: Maybe Suit,
    hotseat :: Direction, highPlayer :: Maybe Direction, highCard :: Maybe Card, playsLeft :: Int,
    nsTricks :: Int}
    deriving (Eq, Show)

blockOutState :: DDState -> Block -> [Char]
blockOutState (DDState d t _ _ _ _ _ ns) center = intercalate "\n" $ combineBlocks [[info, north, none], [west, center, east], [none, south, none]]
    where none = blockOut [[]]
          info = blockOut ["Trump " ++ show1 t, "NS " ++ show ns]
          [north, east, south, west] = handBlocks <$> getHands d

initDDState :: FastDeal -> Strain -> Direction -> DDState
initDDState d t declarer = DDState d t Nothing declarer Nothing Nothing c 0
    -- where c = foldr (++) $ map length $ concatMap getSuits $ getHands d
    where c = sum $ handCount <$> getHands d

candidatePlays :: DDState -> [Card]
candidatePlays state = candidatePlaysH d h c
    where d = deal state
          h = hotseat state
          c = lead state

compareCards :: Strain -> Card -> Card -> Bool
-- card 1 is the current high card, card 2 is the played card
-- true if the current high card is equal or higher. order matters
compareCards (Trump t) (Card r1 s1) (Card r2 s2)
    | s1 == s2 = r1 >= r2
    | t == s2 = False
    | otherwise = True

compareCards Notrump (Card r1 s1) (Card r2 s2)
    | s1 == s2 = r1 > r2
    | otherwise = True

compareCardsM :: Strain -> Maybe Card -> Card -> Bool
compareCardsM s (Just c1) c2 = compareCards s c1 c2
compareCardsM _ Nothing _ = False

tryResolve :: DDState -> DDState
tryResolve dds@(DDState d t _ _ hp _ pl tc)
    | pl `mod` 4 == 0 = DDState d t Nothing (fromJust hp) hp Nothing pl $ tc + 1 - (fromEnum . pair $ fromJust hp)
    | otherwise = dds

playCardS :: DDState -> Card -> DDState
playCardS (DDState d t l hs hp hc pl ns) card@(Card cr cs) =
    tryResolve $ DDState (playCardD d hs cs (unrank cr)) t nl (rotate 1 hs) nhp nhc (pl - 1) ns
    where winner = compareCardsM t hc card
          nl = case l of
                Just _ -> l
                Nothing -> Just cs
          nhc = if winner then hc else Just card
          nhp = if winner then hp else Just hs

newtype DDKey = DDKey (FastDeal, Maybe Suit, Maybe Card, Int)
    deriving (Eq)

instance Hashable DDKey where
    hashWithSalt i (DDKey t) = hashWithSalt i t

instance Game DDState where
    newtype Move DDState = DDMove Card
    type Score DDState = Int
    player d = case pair $ hotseat d of
        NorthSouth -> True
        EastWest -> False
    score = nsTricks
    isFinal = (== 0) . playsLeft
    move p m = lookup m $ moves p

instance Show (Move DDState) where
    show (DDMove c) = show c

instance Eq (Move DDState) where
    (==) (DDMove c1) (DDMove c2) = (==) c1 c2

instance ShowRead (Move DDState) where
    showmap = BM.fromList $ (\(c, s) -> (DDMove c, s)) <$> BM.toList showmap

instance Ord (Move DDState) where
    compare (DDMove c1) (DDMove c2) = compare c1 c2

instance Solvable DDState where
    type Key DDState = DDKey
    key (DDState d _ l _ _ hc _ ns) = DDKey (d, l, hc, ns)
    -- TODO have moves be less complicated
    moves dds = map movef $ candidatePlays dds
        where movef play = (DDMove play, playCardS dds play)

instance InteractiveGame DDState where
    parseMove _ s = BM.lookupR s showmap
    showmove = show
    -- TODO more generic printing
    --showbig = blockOutState
    showgame = show

{-
instance Tracable DDState where
    type Target p = undefined
    traceInit tp = undefined p
    detrace p ms = undefined tp
    traceMove p tm ms = undefined move
-}

printStart :: DDState -> [Char]
printStart p = blockOutState p $ blockOut [[]]

-- A trick: a set of 4 moves
printTrick :: Show b => [(DDState, b)] -> [Char]
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
