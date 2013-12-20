-- | Main entry point to the application.
module Main where
import qualified Hand as H
import qualified Bidding as B
import qualified Solver as S
import qualified Control.Monad.Random as Rand
import qualified System.Random as Rand

-- | The main entry point.
main :: IO ()
main = do
    let d = Rand.evalRand H.randDealM $ Rand.mkStdGen 0
    putStrLn $ show $ d
    putStrLn $ show $ B.hcpCountDeal B.gorenHCP d
    putStrLn $ show $ S.candidatePlays (H.north d) H.Heart