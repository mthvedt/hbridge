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
        [] -> if msuit == Nothing then [] else candidatePlaysH d dir Nothing
        x -> x
    where suits = case msuit of
            (Just s) -> [s]
            Nothing -> reverse [Club ..]

data DDState d = DDState
    {deal :: d, trump :: Strain, lead :: Maybe Suit,
    hotseat :: Direction, highPlayer :: Maybe Direction, highCard :: Maybe Card, playsLeft :: Int,
    nsScore :: Int}
    deriving (Eq, Show)

blockOutState :: IDeal d => Line (DDState d) -> Block -> [Char]
blockOutState (Line (DDState d t _ _ _ _ _ _) sc _) center = intercalate "\n" $ combineBlocks [[info, north, none], [west, center, east], [none, south, none]]
    where none = blockOut [[]]
          info = blockOut ["Trump " ++ show1 t, "NS " ++ show sc]
          [north, east, south, west] = handBlocks <$> getHands d

initDDState :: IDeal d => d -> Strain -> Direction -> DDState d
initDDState d t declarer = DDState d t Nothing declarer Nothing Nothing c 0
    -- where c = foldr (++) $ map length $ concatMap getSuits $ getHands d
    where c = sum $ handCount <$> getHands d

candidatePlays :: IDeal d => DDState d -> [Card]
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

tryResolve :: DDState d -> DDState d
tryResolve dds@(DDState d t _ _ hp _ pl _)
    | pl `mod` 4 == 0 = DDState d t Nothing (fromJust hp) hp Nothing pl $ 1 - (fromEnum . pair $ fromJust hp)
    | otherwise = dds

playCardS :: IDeal d => DDState d -> Card -> DDState d
playCardS (DDState d t l hs hp hc pl _) card@(Card cr cs) =
    tryResolve $ DDState (playCardD d hs cs (unrank cr)) t nl (rotate 1 hs) nhp nhc (pl - 1) 0
    where winner = compareCardsM t hc card
          nl = case l of
                Just _ -> l
                Nothing -> Just cs
          nhc = if winner then hc else Just card
          nhp = if winner then hp else Just hs

newtype (Eq d) => DDKey d = DDKey (d, Maybe Suit, Maybe Card)
    deriving (Eq)

instance (Eq d, Hashable d) => Hashable (DDKey d) where
    hashWithSalt i (DDKey t) = hashWithSalt i t

instance (Eq d, Hashable d, IDeal d) => Game (DDState d) where
    newtype Move (DDState d) = DDMove { theCard :: Card }
    type Score (DDState d) = Int
    player d = case pair $ hotseat d of
        NorthSouth -> True
        EastWest -> False
    score = nsScore
    isFinal = (== 0) . playsLeft
    move p m = lookup m $ moves p

instance Show (Move (DDState d)) where
    show = show . theCard

instance Eq (Move (DDState d)) where
    (==) (DDMove c1) (DDMove c2) = (==) c1 c2

instance ShowRead (Move (DDState d)) where
    showmap = BM.fromList $ (\(c, s) -> (DDMove c, s)) <$> BM.toList showmap

instance Ord (Move (DDState d)) where
    compare (DDMove c1) (DDMove c2) = compare c1 c2

instance (Hashable d, Eq d, IDeal d) => Solvable (DDState d) where
    type Key (DDState d) = DDKey d
    key (DDState d _ l _ _ hc _ _) = DDKey (d, l, hc)
    -- TODO have moves be less complicated
    moves dds = map movef $ candidatePlays dds
        where movef play = (DDMove play, playCardS dds play)
    initScores d = (0, (playsLeft d + 3) `div` 4)

instance (Hashable d, Eq d, IDeal d) => InteractiveGame (DDState d) where
    parseMove _ s = BM.lookupR s showmap
    showmove = show
    -- TODO more generic printing
    --showbig = blockOutState
    showgame = show

compress :: DDState Deal -> (DDState FastDeal, [Card])
compress (DDState deal a b c d mhs f g) = (DDState (FastDeal deal2) a b c d hs2 f g, cs)
    where cs = sortBy compareRank $ fulldeck \\ getCardsD deal
          deal2 = foldl shiftCardD deal cs
          hs2 = case mhs of
                    Just hs -> Just $ foldl shiftCard hs cs
                    Nothing -> Nothing
decompress :: Move (DDState d0) -> [Card] -> Move (DDState d1)
-- TODO shouldn't need to flip
decompress (DDMove c) cs = DDMove $ foldl (flip unshiftCard) c $ reverse cs

fastHandView :: View (DDState Deal) (DDState FastDeal)
fastHandViewH dds = (dds2, flip decompress cards)
    where (dds2, cards) = compress dds
fastHandView = View fastHandViewH

printStart :: IDeal d => Line (DDState d) -> [Char]
printStart p = blockOutState p $ blockOut [[]]

-- A trick: a set of 4 moves
printTrick :: (IDeal d, Show b) => [(Line (DDState d), b)] -> [Char]
printTrick pms =
    blockOutState finall center
    where [firstl, _, _, finall] = fst <$> pms
          firstp = currpos firstl
          firstMover = rotate (-1) $ hotseat firstp
          winner = fromJust . highPlayer $ currpos finall
          movesByDir = take 4 . drop (4 - fromEnum firstMover) . cycle $ show . snd <$> pms
          moveMarker x i
              | i == firstMover = "<" ++ x ++ ">"
              | i == winner = "*" ++ x ++ "*"
              | otherwise = " " ++ x ++ " "
          [mn, me, ms, mw] = zipWith moveMarker movesByDir [North ..]
          moveStrs = [[], "     " ++ mn ++ "     ", unwords [mw, ms, me], []]
          center = blockOut moveStrs

-- TODO generify
-- TODO use the line apparatus somehow
showLine :: (IDeal d, Hashable d, Eq d) => DDState d -> [Move (DDState d)] -> String
showLine initp ms = intercalate ("\n" ++ replicate 44 '=' ++ "\n") pstrs
    where initl = newline initp
          ps = tail $ scanl (fmovev lineView) initl ms
          pms = zip ps ms
          pstrs = (printStart initl):(printTrick <$> chunksOf 4 pms)
