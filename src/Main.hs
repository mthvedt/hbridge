-- | Main entry point to the application.
module Main where
import qualified Hand as H
import qualified Bidding as B
import qualified Solver as S
import qualified Control.Monad.Random as Rand
import qualified Solver.Generic

-- | The main entry point.
main :: IO ()
main = do
    let d = Rand.evalRand (H.randPartialDealM 9) $ Rand.mkStdGen 0
        dds = S.initDDState d (H.Trump H.Heart) H.West
    Solver.Generic.printLine dds
    -- print dds
    putStrLn . S.showLine dds . fst $ Solver.Generic.minimaxLine dds
    -- putStrLn $ show $ B.hcpCountDeal B.gorenHCP d
    -- putStrLn $ show $ S.candidatePlays (H.getHand d 1) H.Heart
    -- putStrLn $ show $ S.initDDLine d (H.Trump H.Heart) H.North
    -- let n = Solver.Generic.Nim True 69
    -- Solver.Generic.printLine n
    --putStrLn $ show $ rank n
    --putStrLn $ show $ moves n
    -- playGame n
