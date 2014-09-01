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

class Show1 x where
    show1 :: x -> [Char]

data Suit = Club | Diamond | Heart | Spade
    deriving (Show, Enum, Eq, Ord)

instance Show1 Suit where
    show1 Club = "c" -- "♣"
    show1 Diamond = "d" -- "♢"
    show1 Heart = "h" -- "♡"
    show1 Spade = "s" -- "♠"

instance Hashable Suit where
    hashWithSalt i = hashWithSalt i . fromEnum

data Direction = North | East | South | West
    deriving (Show, Enum, Eq, Ord)

instance Show1 Direction where
    show1 x = [head $ show x]

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

instance Show1 Rank where
    show1 (Rank 8) = "T"
    show1 (Rank 9) = "J"
    show1 (Rank 10) = "Q"
    show1 (Rank 11) = "K"
    show1 (Rank 12) = "A"
    show1 (Rank x) = show $ x + 2

instance Hashable Rank where
    hashWithSalt i = hashWithSalt i . unrank

data Strain = Trump Suit | Notrump
    deriving (Show, Eq, Ord)

instance Hashable Strain where
    hashWithSalt i (Trump s) = hashWithSalt i s
    hashWithSalt i _ = hashWithSalt i (4 :: Int)

instance Show1 Strain where
    show1 Notrump = "n"
    show1 (Trump s) = show1 s

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

instance Show Card where
    show (Card r s) = show1 s ++ show1 r

partialDeck i = [Card (Rank v) s | v <- [(13 - i) .. 12], s <- [Club ..]]
fulldeck = partialDeck 13
randPartialDeckM i = do
    d <- shuffleM $ partialDeck i
    return $ take (i * 4) d
randDeckM :: (RandomGen g) => Rand g [Card]
randDeckM = randPartialDeckM 13

newtype Hand = Hand (Array Int [Rank])
    deriving (Eq)

instance Hashable Hand where
    hashWithSalt i (Hand x) = hashWithSalt i $ elems x

handBlocks1 (Hand ss) =
    let showSuit n s = foldl (++) (show1 n) . map show1 . reverse $ sort s
        in reverse . zipWith showSuit [Club ..] $ elems ss

pad 0 p xs = xs
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
    Hand . listArray (0, 3) $ reverse . sort . map rank <$> map (\x -> filter ((==) x . suit) cards) [Club ..]

getSuit :: Hand -> Suit -> [Rank]
getSuit (Hand ss) i = ss ! fromEnum i

handCount :: Hand -> Int
handCount (Hand ss) = sum $ length <$> elems ss

playCardH (Hand ss) (Card r s) = Hand $ ss // [(si, newSuit)]
    where newSuit = delete r $ ss ! si
          si = fromEnum s

-- all cards lower than r2 are shifted up
shiftRank r2 r1 = Rank $ if ri1 < ri2 then ri1 + 1 else ri1
    where ri1 = unrank r1
          ri2 = unrank r2

unshiftRank r2 r1 = Rank $ if ri1 <= ri2 then ri1 - 1 else ri1
    where ri1 = unrank r1
          ri2 = unrank r2

unshiftCard (Card r2 s2) c1@(Card r1 s1) =
    if s1 == s2 then Card (unshiftRank r2 r1) s1
                else c1

shiftCardH (Card r s) (Hand ss) = Hand $ ss // [(si, newSuit)]
    where newSuit = shiftRank r <$> (delete r $ ss ! si)
          si = fromEnum s

unshiftCardH (Card r s) (Hand ss) = Hand $ ss // [(si, newSuit)]
    where newSuit = unshiftRank r <$> ss ! si
          si = fromEnum s

class (Show d) => IDeal d where
    -- todo rename
    getHand :: d -> Direction -> Hand
    candidatePlaysD :: d -> Direction -> Suit -> [Int]
    playCardD :: d -> Direction -> Suit -> Int -> d
    unshiftD :: d -> Card -> d

newtype Deal = Deal (Array Int Hand)
    deriving (Eq)

instance IDeal Deal where
    getHand (Deal hs) d = hs ! fromEnum d
    playCardD (Deal hs) dir s i = Deal $ hs // [(fromEnum dir, playCardH (hs ! fromEnum dir) (Card (Rank i) s))]
    candidatePlaysD d dir s = unrank <$> getSuit (getHand d dir) s
    unshiftD d _ = d

newtype FastDeal = FastDeal (Array Int Hand)
    deriving (Eq)

instance IDeal FastDeal where
    -- TODO: in semi-played FastDeals, getHand will have 'incorrect' results
    getHand (FastDeal hs) d = hs ! fromEnum d
    playCardD (FastDeal hs) dir s i = FastDeal $ shiftCardH c <$> hs // [(dirint, nh)]
        where dirint = fromEnum dir
              c = Card (Rank i) s
              nh = playCardH (hs ! dirint) $ c
    candidatePlaysD d dir s = unrank <$> getSuit (getHand d dir) s
    unshiftD (FastDeal hs) card = FastDeal $ unshiftCardH card <$> hs

getHands d = getHand d <$> [North ..]

instance Show Deal where
    show (Deal hs) =
        let showHand n h = n ++ ": " ++ show h
            in intercalate ", " . zipWith showHand ["north", "east", "south", "west"] $ elems hs

instance Show FastDeal where
    show (FastDeal hs) =
        let showHand n h = n ++ ": " ++ show h
            in intercalate ", " . zipWith showHand ["north", "east", "south", "west"] $ elems hs

instance Hashable Deal where
    hashWithSalt i (Deal x) = hashWithSalt i $ elems x

instance Hashable FastDeal where
    hashWithSalt i (FastDeal x) = hashWithSalt i $ elems x

-- TODO polymorphic construction
-- newPartialDeal i d = Deal . listArray (0, 3) $ newHand <$> chunksOf i d
newPartialDeal i d = FastDeal . listArray (0, 3) $ newHand <$> chunksOf i d
newDeal = newPartialDeal 13

randPartialDealM i = newPartialDeal i `liftM` randPartialDeckM i
randDealM :: (RandomGen g) => Rand g FastDeal
randDealM = randPartialDealM 13
