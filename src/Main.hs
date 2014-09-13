-- | Main entry point to the application.
module Main where
import qualified Control.Category as C
import qualified Control.Monad.Random as Rand
import Data.List
import qualified Bidding as B
import qualified Hand as H
import qualified Solver as S
import qualified Solver.Generic
import qualified Solver.Interactive

-- | The main entry point.
main :: IO ()
main = do
    let d = Rand.evalRand (H.randPartialDealM 10) $ Rand.mkStdGen 0
        dds = S.initDDState d (H.Trump H.Heart) H.West
        l = Solver.Generic.newline dds
        -- (ms, sc) = Solver.Generic.minimaxLine dds
        (ms, sc) = Solver.Generic.alphaBetaLine dds
    --Solver.Generic.printLine dds
    -- print dds
    putStrLn $ S.showLine dds ms
    -- Solver.Interactive.doInteractive l Solver.Generic.lineView
    --Solver.Interactive.doInteractive l $ Solver.Generic.lineView >>> S.fastHandView 
    -- putStrLn $ show $ B.hcpCountDeal B.gorenHCP d
    -- putStrLn $ show $ S.candidatePlays (H.getHand d 1) H.Heart
    -- putStrLn $ show $ S.initDDLine d (H.Trump H.Heart) H.North
    -- let n = Solver.Generic.Nim True 69
    -- Solver.Generic.printLine n
    --putStrLn $ show $ rank n
    --putStrLn $ show $ moves n
    -- playGame n
