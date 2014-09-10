{-# LANGUAGE FunctionalDependencies #-}
module Hand where
import System.Random
import System.Random.Shuffle
import Control.Monad
import Control.Monad.Random
import Control.Monad.Random.Class
import Data.List.Split
import Data.Functor
import Data.List
import Data.Maybe
import Data.Array
import Data.Hashable
import qualified Data.Bimap as BM

-- TODO we can probably implement this better with Control.Lens
class (Ord x) => ShowRead x where
    showmap :: BM.Bimap x String

show1 :: (ShowRead x) => x -> String
show1 x = fromJust $ BM.lookup x showmap

read1 :: (ShowRead x) => String -> x
read1 s = fromJust $ BM.lookupR s showmap

data Suit = Club | Diamond | Heart | Spade
    deriving (Show, Enum, Eq, Ord, Ix)

instance ShowRead Suit where
    showmap = BM.fromList [(Club, "c"), (Diamond, "d"), (Heart, "h"), (Spade, "s")]

instance Hashable Suit where
    hashWithSalt i = hashWithSalt i . fromEnum

data Direction = North | East | South | West
    deriving (Show, Enum, Eq, Ord, Ix)

instance ShowRead Direction where
    showmap = BM.fromList $ (\x -> (x, show x)) <$> [North ..]

instance Hashable Direction where
    hashWithSalt i = hashWithSalt i . fromEnum

rotate :: (Enum e) => Int -> e -> e
rotate i x = toEnum $ (fromEnum x + i) `mod` 4

data Side = NorthSouth | EastWest
    deriving (Show, Enum, Eq, Ord)

pair :: Direction -> Side
pair x = toEnum $ fromEnum x `mod` 2

newtype Rank = Rank Int
    deriving (Eq, Ord)

unrank (Rank i) = i

instance ShowRead Rank where
    showmap = BM.fromList $ concat [((\x -> (Rank x, show $ x + 2)) <$> [0 .. 7]),
                                    [(Rank 8, "T"),
                                     (Rank 9, "J"),
                                     (Rank 10, "Q"),
                                     (Rank 11, "K"),
                                     (Rank 12, "A")]]

instance Hashable Rank where
    hashWithSalt i = hashWithSalt i . unrank

data Strain = Trump Suit | Notrump
    deriving (Show, Eq, Ord)

instance Hashable Strain where
    hashWithSalt i (Trump s) = hashWithSalt i s
    hashWithSalt i _ = hashWithSalt i (4 :: Int)

instance ShowRead Strain where
    showmap = BM.insert Notrump "n" $ BM.fromList $ (\(k, v) -> (Trump k, v)) <$> BM.assocs showmap
    --showmap = BM.fromList $ (\(k, v) -> (Trump k, v)) <$> BM.assocs showmap

data Bid = Bid {level :: Int, strain :: Strain}
    deriving (Eq)

instance Show Bid where
    show (Bid l s) = show l ++ show1 s

data Card = Card {rank :: Rank, suit :: Suit}
    deriving (Eq)

instance Hashable Card where
    hashWithSalt i (Card r s) = hashWithSalt i (r, s)

instance Ord Card where
   compare (Card r1 s1) (Card r2 s2)
        | s1 /= s2 = compare s1 s2
        | r1 /= r2 = compare r1 r2
        | otherwise = EQ

compareRank :: Card -> Card -> Ordering
compareRank (Card r1 _) (Card r2 _) = compare r1 r2

instance Show Card where
    show (Card r s) = show1 s ++ show1 r

instance ShowRead Card where
    showmap = BM.fromList $ (\x -> (x, show x)) <$> fulldeck

partialDeck i = [Card (Rank v) s | v <- [(13 - i) .. 12], s <- [Club ..]]
fulldeck = partialDeck 13
randPartialDeckM i = do
    d <- shuffleM $ partialDeck i
    return $ take (i * 4) d
randDeckM :: (RandomGen g) => Rand g [Card]
randDeckM = randPartialDeckM 13

newtype Hand = Hand (Array Suit [Rank])
    deriving (Eq)

instance Hashable Hand where
    hashWithSalt i (Hand x) = hashWithSalt i $ elems x

handBlocks1 (Hand ss) =
    let showSuit n s = foldl (++) (show1 n) . map show1 . reverse $ sort s
        in reverse . zipWith showSuit [Club ..] $ elems ss

pad 0 _ xs = xs
pad i p (c:cs) = c:(pad (i - 1) p cs)
pad i p [] = p:(pad (i - 1) p [])

type Block = [String]
blockOut :: [String] -> Block

blockOut x = pad 14 ' ' <$> pad 4 "" x
handBlocks = blockOut . handBlocks1

combineBlocksRow :: [Block] -> Block
combineBlocksRow = foldl (zipWith (\x y -> x ++ " " ++ y)) $ repeat []

combineBlocks :: [[Block]] -> Block
combineBlocks = intercalate [""] . fmap combineBlocksRow

instance Show Hand where
    show = unwords . handBlocks1

newHand cards =
    Hand . listArray (Club, Spade) $ reverse . sort . map rank <$> map (\x -> filter ((==) x . suit) cards) [Club ..]

getSuit :: Hand -> Suit -> [Rank]
getSuit (Hand ss) i = ss ! i

getCardsH :: Hand -> [Card]
getCardsH (Hand ss) = concat $ (\(s, rs) -> flip Card s <$> rs) <$> assocs ss

handCount :: Hand -> Int
handCount (Hand ss) = sum $ length <$> elems ss

playCardH (Hand ss) (Card r s) = Hand $ ss // [(s, newSuit)]
    where newSuit = delete r $ ss ! s

-- all cards lower than r2 are shifted up
shiftRank r2 r1 = Rank $ if ri1 < ri2 then ri1 + 1 else ri1
    where ri1 = unrank r1
          ri2 = unrank r2

-- all cards lower or equal to r2 are shifted down
unshiftRank r2 r1 = Rank $ if ri1 <= ri2 then ri1 - 1 else ri1
    where ri1 = unrank r1
          ri2 = unrank r2

shiftCard (Card r2 s2) c1@(Card r1 s1) =
    if s1 == s2 then Card (shiftRank r2 r1) s1
                else c1

unshiftCard (Card r2 s2) c1@(Card r1 s1) =
    if s1 == s2 then Card (unshiftRank r2 r1) s1
                else c1

shiftCardH (Card r s) (Hand ss) = Hand $ ss // [(s, newSuit)]
    where newSuit = shiftRank r <$> (delete r $ ss ! s)

unshiftCardH (Card r s) (Hand ss) = Hand $ ss // [(s, newSuit)]
    where newSuit = unshiftRank r <$> ss ! s

-- TODO might not need IDeal
class (Show d) => IDeal d where
    -- todo rename
    getHand :: d -> Direction -> Hand
    candidatePlaysD :: d -> Direction -> Suit -> [Int]
    playCardD :: d -> Direction -> Suit -> Int -> d
    unshiftD :: d -> Card -> d

newtype Deal = Deal (Array Direction Hand)
    deriving (Eq)

instance IDeal Deal where
    getHand (Deal hs) d = hs ! d
    playCardD (Deal hs) dir s i = Deal $ hs // [(dir, playCardH (hs ! dir) (Card (Rank i) s))]
    candidatePlaysD d dir s = unrank <$> getSuit (getHand d dir) s
    unshiftD d _ = d

getCardsD :: Deal -> [Card]
getCardsD (Deal arr) = concatMap getCardsH $ elems arr

playCardAndShift (Deal hs) dir s i = Deal $ shiftCardH c <$> hs // [(dir, nh)]
    where c = Card (Rank i) s
          nh = playCardH (hs ! dir) $ c

shiftCardD (Deal hs) card = Deal $ shiftCardH card <$> hs
unshiftCardD (Deal hs) card = Deal $ unshiftCardH card <$> hs

newtype FastDeal = FastDeal { theDeal :: Deal }
    deriving (Eq)

instance Show FastDeal where
    show = show . theDeal

instance IDeal FastDeal where
    getHand = getHand . theDeal
    playCardD d dir s i = FastDeal $ (playCardAndShift . theDeal) d dir s i
    candidatePlaysD = candidatePlaysD . theDeal
    unshiftD d c = FastDeal $ (unshiftCardD . theDeal) d c

getHands d = getHand d <$> [North ..]

instance Show Deal where
    show (Deal hs) =
        let showHand n h = n ++ ": " ++ show h
            in intercalate ", " . zipWith showHand ["north", "east", "south", "west"] $ elems hs

instance Hashable Deal where
    hashWithSalt i (Deal x) = hashWithSalt i $ elems x

instance Hashable FastDeal where
    hashWithSalt i = hashWithSalt i . theDeal

newPartialDeal i d = Deal . listArray (North, West) $ newHand <$> chunksOf i d
newDeal = newPartialDeal 13

randPartialDealM i = newPartialDeal i `liftM` randPartialDeckM i
randDealM :: (RandomGen g) => Rand g Deal
randDealM = randPartialDealM 13
