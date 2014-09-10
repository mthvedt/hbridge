{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
module Solver.Interactive where
import Solver.Generic
-- TODO remove
import Hand

-- TODO
class (Game p, Show (Score p)) => InteractiveGame p where
    parseMove :: p -> String -> Maybe (Move p)
    -- Using Show for Move (Target p) can yield undecidable types
    -- TODO showmove is no longer needed after removing (Target p)
    showmove :: Move p -> String
    showgame :: p -> String

instance (InteractiveGame g) => InteractiveGame (Line g) where
    parseMove (Line p _) s = parseMove p s >>= return . LMove
    showmove (LMove m) = showmove m
    showgame = showgame . lpos

acceptMove p i = parseMove p i >>= move p

printScore game =
    do putStr "final score: "
       putStrLn . show $ score game

-- TODO computer moves and such
doInteractive :: (Solvable p1, InteractiveGame p1, InteractiveGame p) => p -> View p p1 -> IO ()
--doInteractive :: (Solvable p1, InteractiveGame p) => p -> View p p1 -> IO ()
doInteractive p v@(View vf)
    | isFinal p = printScore p
-- Hard code player == east-west for now
    | player p = do
          putStrLn $ showgame p
          putStrLn "Thinking..."
          let (p1, ctx, rf) = vf p
              m = head . fst $ minimaxLine p1
              m0 = rf m ctx
              p2 = fmove p m0
          -- putStrLn $ showgame p1
          -- putStrLn $ show ctx
          putStr "My move: "
          -- putStrLn $ showmove m
          putStrLn $ showmove m0
          doInteractive p2 v
    | otherwise = do
        putStrLn $ showgame p
        -- TODO putStr "Your move: "
        i <- getLine
        case acceptMove p i of
            Just p2 -> doInteractive p2 v
            Nothing -> do
                putStrLn "Invalid move!"
                doInteractive p v
