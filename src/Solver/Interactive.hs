{-# LANGUAGE FlexibleContexts #-}
module Solver.Interactive where
import qualified Data.Maybe
import Solver.Generic

class (Game p, Show (Move p), Show (Score p)) => InteractiveGame p where
    parseMove :: p -> String -> Maybe (Move p)
    showgame :: p -> String

acceptMove p i = parseMove p i >>= move p

printScore game =
    do putStr "final score: "
       putStrLn . show $ score game

-- TODO computer moves and such
doInteractive :: (Solvable p, InteractiveGame p) => p -> IO ()
doInteractive p
    | isFinal p = printScore p
-- Hard code player == east-west for now
    | player p = do
          putStrLn $ showgame p
          let m = head . fst $ minimaxLine p
              p2 = fmove p m
          putStr "My move: "
          putStrLn $ show m
          doInteractive p2
    | otherwise = do
        putStrLn $ showgame p
        i <- getLine
        case acceptMove p i of
            Just p2 -> doInteractive p2
            Nothing -> do
                putStrLn "Invalid move!"
                doInteractive p
