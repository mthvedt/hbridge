-- | Main entry point to the application.
module Main where
import Control.Category
import Data.List
import qualified Hand as H
import qualified Bidding as B
import qualified Solver as S
import qualified Control.Monad.Random as Rand
import qualified Solver.Generic
import qualified Solver.Interactive

-- | The main entry point.
main :: IO ()
main = do
    let d = Rand.evalRand (H.randPartialDealM 8) $ Rand.mkStdGen 0
        dds = S.initDDState d (H.Trump H.Heart) H.West
        l = Solver.Generic.newline dds
    --Solver.Generic.printLine dds
    -- print dds
    --putStrLn . S.showLine dds . fst $ Solver.Generic.minimaxLine dds
    -- Solver.Interactive.doInteractive l Solver.Generic.lineView
    Solver.Interactive.doInteractive l $ Solver.Generic.lineView >>> S.fastHandView 
    -- putStrLn $ show $ B.hcpCountDeal B.gorenHCP d
    -- putStrLn $ show $ S.candidatePlays (H.getHand d 1) H.Heart
    -- putStrLn $ show $ S.initDDLine d (H.Trump H.Heart) H.North
    -- let n = Solver.Generic.Nim True 69
    -- Solver.Generic.printLine n
    --putStrLn $ show $ rank n
    --putStrLn $ show $ moves n
    -- playGame n
