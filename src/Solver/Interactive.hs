{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Solver.Interactive where
import Solver.Generic

class (GameTree p m s k, Show p, Show s) => InteractiveGame p m s k where
    acceptMove :: p -> String -> Maybe p

printScore game =
    do putStr "final score: "
       putStrLn . show $ score game

-- TODO computer moves and such
doInteractive :: (Show p, Show m, InteractiveGame p m s k) => p -> IO ()
doInteractive p
    | isFinal p = printScore p
-- Hard code player == east-west for now
    | player p = do
          putStrLn $ show p
          let (p2, m) = head . fst $ minimaxLine p
          putStr "My move: "
          putStrLn $ show m
          doInteractive p2
    | otherwise = do
        putStrLn $ show p
        i <- getLine
        case acceptMove p i of
            Just p2 -> doInteractive p2
            Nothing -> do
                putStrLn "Invalid move!"
                doInteractive p
