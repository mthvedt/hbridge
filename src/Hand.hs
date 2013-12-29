module Hand where
import System.Random
import System.Random.Shuffle
import Control.Monad.Random
import Control.Monad.Random.Class
import Control.Monad.State
import Data.List.Split
import Data.Functor
import Data.List
import Data.Array
import Data.Hashable

data Suit = Club | Diamond | Heart | Spade
    deriving (Show, Enum, Eq, Ord)
    
instance Hashable Suit where
    hashWithSalt i = hashWithSalt i . fromEnum

data Direction = North | East | South | West
    deriving (Show, Enum, Eq, Ord)
    
instance Hashable Direction where
    hashWithSalt i = hashWithSalt i . fromEnum

rotate :: Direction -> Direction
rotate x = toEnum $ (fromEnum x + 1) `mod` 4

data Side = NorthSouth | EastWest
    deriving (Show, Enum, Eq, Ord)
    
pair :: Direction -> Side
pair x = toEnum $ fromEnum x `mod` 2

class Show1 x where
    show1 :: x -> [Char]

instance Show1 Suit where 
    show1 Club = "c"
    show1 Diamond = "d"
    show1 Heart = "h"
    show1 Spade = "s"

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
    show1 Notrump = "N"
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
    show (Card r s) = show1 r ++ show1 s

fulldeck = [Card (Rank v) s | v <- [0 .. 12], s <- [Club ..]]
randDeckM :: (RandomGen g) => Rand g [Card]
randDeckM = shuffleM fulldeck

newtype Hand = Hand (Array Int [Rank])
    deriving (Eq)

instance Hashable Hand where
    hashWithSalt i (Hand x) = hashWithSalt i $ elems x

instance Show Hand where
    show (Hand ss) =
        let showSuit n s = foldl (++) (show1 n) . map show1 . reverse $ sort s
            in unwords . zipWith showSuit [Club ..] $ elems ss

newHand cards =
    Hand . listArray (0, 3) $ reverse . sort . map rank <$> map (\x -> filter ((==) x . suit) cards) [Club ..]

getSuit :: Hand -> Suit -> [Rank]
getSuit (Hand ss) i = ss ! fromEnum i

playCardH (Hand ss) (Card r s) = Hand $ ss // [(si, newSuit)]
    where newSuit = delete r $ ss ! si
          si = fromEnum s

newtype Deal = Deal (Array Int Hand)
    deriving (Eq)
    
instance Show Deal where
    show (Deal hs) =
        let showHand n h = n ++ ": " ++ show h
            in intercalate ", " . zipWith showHand ["north", "east", "south", "west"] $ elems hs

instance Hashable Deal where
    hashWithSalt i (Deal x) = hashWithSalt i $ elems x

newDeal d = Deal . listArray (0, 3) $ newHand <$> chunksOf 13 d
getHand (Deal arr) i = arr ! fromEnum i

playCardD :: Deal -> Direction -> Card -> Deal
playCardD (Deal hs) i c = Deal $ hs // [(fromEnum i, playCardH (hs ! fromEnum i) c)]

randDealM :: (RandomGen g) => Rand g Deal
randDealM = liftM newDeal $ randDeckM