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
    let d = Rand.evalRand H.randDealM $ Rand.mkStdGen 0
        dds = S.initDDState d (H.Trump H.Heart) H.North
    print $ dds
    iterateM 52 playACard dds
    return ()
    where playACard x = do
            let c = head $ S.candidatePlays x
                r = S.playCardS x c
            print c
            print r
            return r
          iterateM 0 f x = return x
          iterateM n f x = f x >>= iterateM (n - 1) f
    -- putStrLn $ show $ B.hcpCountDeal B.gorenHCP d
    -- putStrLn $ show $ S.candidatePlays (H.getHand d 1) H.Heart
    -- putStrLn $ show $ S.initDDLine d (H.Trump H.Heart) H.North
    -- let n = Nim True 101
    --putStrLn $ show $ rank n
    --putStrLn $ show $ moves n
    -- playGame n