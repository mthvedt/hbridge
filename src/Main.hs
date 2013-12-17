-- | Main entry point to the application.
module Main where
import Hand
import Bidding
import Control.Monad.Random
import System.Random

-- | The main entry point.
main :: IO ()
main = do
    let b = evalRand randBoardM $ mkStdGen 0
    putStrLn $ show $ b
    putStrLn $ show $ mapDeal (hcpCount gorenHCP) $ deal b