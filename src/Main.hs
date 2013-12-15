-- | Main entry point to the application.
module Main where
import Hand
import Control.Monad.Random
import System.Random

-- | The main entry point.
main :: IO ()
main = do
    putStrLn $ show $ evalRand randBoardM $ mkStdGen 0