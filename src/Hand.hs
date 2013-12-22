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

data Suit = Club | Diamond | Heart | Spade
    deriving (Show, Enum, Eq, Ord)

data Direction = North | East | South | West
    deriving (Show, Enum, Eq, Ord)

class Show1 x where
    show1 :: x -> [Char]

instance Show1 Suit where 
    show1 Club = "c"
    show1 Diamond = "d"
    show1 Heart = "h"
    show1 Spade = "s"

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

newtype Hand = Hand (Array Int [Int])
    deriving (Eq)

instance Show Hand where
    show (Hand ss) =
        let showSuit n s = foldl (++) n $ map showRank $ reverse $ sort s
            in unwords $ zipWith showSuit (map show1 [Club ..])  $ elems ss

newHand cards =
    Hand $ listArray (0, 3) $ reverse <$> sort <$> map rank <$> map (\x -> filter (\y -> x == suit y) cards) [Club ..]

newtype Deal = Deal (Array Int Hand)
    deriving (Eq)
    
instance Show Deal where
    show (Deal hs) =
        let showHand n h = n ++ ": " ++ show h
            in intercalate ", " $ zipWith showHand ["north", "east", "south", "west"] $ elems hs

newDeal d = Deal $ listArray (0, 3) $ newHand <$> chunksOf 13 d
getHand (Deal arr) i = arr ! i

randDealM :: (RandomGen g) => Rand g Deal
randDealM = liftM newDeal $ randDeckM

data Board = Board {deal :: Deal, dealer :: Direction, contract :: Bid}
    deriving (Eq, Show)