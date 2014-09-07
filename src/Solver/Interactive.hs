{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
module Solver.Interactive where
import Solver.Generic

class (Game p, Show (Score p)) => InteractiveGame p where
    parseMove :: p -> String -> Maybe (Move p)
    -- Using Show for Move (Target p) can yield undecidable types
    showmove :: Move p -> String
    showgame :: p -> String

instance (Tracable g, Show (Move (Target g)), Show (Score g),
                 InteractiveGame (Target g)) =>
             InteractiveGame (Line g) where
    parseMove (Line _ tp _) i = parseMove tp i >>= return . LMove
    showmove (LMove m) = show m
    showgame = showgame . detraceLine

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
          putStrLn $ showmove m
          doInteractive p2
    | otherwise = do
        putStrLn $ showgame p
        i <- getLine
        case acceptMove p i of
            Just p2 -> doInteractive p2
            Nothing -> do
                putStrLn "Invalid move!"
                doInteractive p
