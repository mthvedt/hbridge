{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Solver.Interactive where
import Solver.Generic

class (GameTree p m s k, Show p, Show s) => InteractiveGame p m s k where
    acceptMove :: p -> String -> Maybe p

printScore game =
    do putStr "final score: "
       putStrLn . show $ score game

-- TODO computer moves and such
doInteractive :: (InteractiveGame p m s k) => p -> IO ()
doInteractive p = if isFinal p
   then printScore p
   else do
       putStrLn $ show p
       i <- getLine
       case acceptMove p i of
           Just p2 -> doInteractive p2
           Nothing -> do
                   putStrLn "Invalid move!"
                   doInteractive p
