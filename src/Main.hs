-- | Main entry point to the application.
module Main where
import qualified Hand as H
import qualified Bidding as B
import qualified Solver as S
import qualified Control.Monad.Random as Rand
import qualified System.Random as Rand
import Data.Array
import Solver.Generic

-- | The main entry point.
main :: IO ()
main = do
    let d = Rand.evalRand H.randDealM $ Rand.mkStdGen 0
        (p:ps) = S.candidatePlays (H.getHand d H.North) H.Heart
        d2 = H.playCardD d H.North p
    putStrLn $ show $ d
    putStrLn $ show $ d2
    -- putStrLn $ show $ B.hcpCountDeal B.gorenHCP d
    -- putStrLn $ show $ S.candidatePlays (H.getHand d 1) H.Heart
    -- putStrLn $ show $ S.initDDLine d (H.Trump H.Heart) H.North
    -- let n = Nim True 101
    --putStrLn $ show $ rank n
    --putStrLn $ show $ moves n
    -- playGame n