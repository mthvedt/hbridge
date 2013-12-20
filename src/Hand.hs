module Hand where
import System.Random
import System.Random.Shuffle
import Control.Monad.Random
import Control.Monad.Random.Class
import Control.Monad.State
import Data.List.Split
import Data.Functor
import Data.List
import qualified Data.Foldable as F

data Suit = Club | Diamond | Heart | Spade
    deriving (Show, Enum, Eq, Ord)

data Direction = North | East | South | West
    deriving (Show, Enum, Eq, Ord)

class Show1 x where
    show1 :: x -> [Char]

instance Show1 Suit where 
    show1 Club = "C"
    show1 Diamond = "D"
    show1 Heart = "H"
    show1 Spade = "S"

showRank 8 = "T"
showRank 9 = "J"
showRank 10 = "Q"
showRank 11 = "K"
showRank 12 = "A"
showRank x = show $ x + 2

data Strain = Trump Suit | Notrump
    deriving (Show, Eq, Ord)

instance Show1 Strain where
    show1 Notrump = "N"
    show1 (Trump s) = show1 s

data Bid = Bid {level :: Int, strain :: Strain}
    deriving (Eq)

instance Show Bid where
    show (Bid l s) = show l ++ show1 s
    
data Card = Card {rank :: Int, suit :: Suit}
    deriving (Eq)

instance Ord Card where
   compare (Card r1 s1) (Card r2 s2)
        | s1 /= s2 = compare s1 s2
        | r1 /= r2 = compare r1 r2
        | otherwise = EQ

instance Show Card where
    show (Card r s) = showRank r ++ show1 s

fulldeck = [Card v s | v <- [0 .. 12], s <- [Club ..]]
randDeckM :: (RandomGen g) => Rand g [Card]
randDeckM = shuffleM fulldeck

data Hand = Hand {spades :: [Int], hearts :: [Int], diams :: [Int], clubs :: [Int]}
    deriving (Eq)

getSuit (Hand s h d c) su =
    case su of
        Spade -> s
        Heart -> h
        Diamond -> d
        Club -> c

mapHand f (Hand s h d c) = map f [s, h, d, c]
foldrHand f a (Hand s h d c) = foldr f a $ map (foldr f a) [s, h, d, c]
gatherHand f m a (Hand s h d c) = foldr m a $ concat $ map f <$> [s, h, d, c]

instance Show Hand where
    show (Hand s h d c) =
        let showSuit name suit = foldl (++) name $ map showRank $ reverse $ sort suit
            in unwords $ zipWith showSuit ["s", "h", "d", "c"] [s, h, d, c]

-- TODO: the requirement that hands be sorted is poorly encapsulated
splitHand cards =
    let [s, h, d, c] = reverse <$> sort <$> map rank <$> map (\x -> filter (\y -> x == suit y) cards) [Club ..]
        in Hand s h d c

data Deal = Deal {north :: Hand, east :: Hand, south :: Hand, west :: Hand}
    deriving (Eq, Show)

getHand (Deal n e s w) d =
    case d of
        North -> n
        East -> e
        South -> s
        West -> w

mapDeal f (Deal n e s w) = f <$> [n, e, s, w]

dealHand d =
    let [north, east, south, west] = splitHand <$> chunksOf 13 d
        in Deal north east south west

randDealM :: (RandomGen g) => Rand g Deal
randDealM = liftM dealHand $ randDeckM

data Board = Board {deal :: Deal, dealer :: Direction, contract :: Bid}
    deriving (Eq, Show)