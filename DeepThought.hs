-- file: DeepThought.hs
-- author: alemedeiros <alexandre.n.medeiros _at_ gmail.com>
--
-- Name:  Alexandre Novais de Medeiros
-- RA:    115966
--
-- Program for the MC346 robot fight competition.

import Control.Monad
import DataAcquisitor
import InfiniteImprobabilityDrive
import System.IO

--
-- Main and IO related functions
--
main :: IO ()
main = do
        hSetBuffering stdout LineBuffering
        -- Get game informations
        pStr <- getLine
        szStr <- getLine
        let
            p  = whoAmI pStr
            sz = readSize $ words szStr
        brdStr <- replicateM (fst sz) getLine
        let
            board   = readBoard brdStr
            team    = findRobots p board
            enemies =  findRobots (opponent p) board
            game    = Game p sz board team enemies
        -- Start playing :-D
        startGame game

-- Starting game move:
--  if I am A, make my first move;
--  if I am B, wait for A's move (entering main loop).
startGame :: Game -> IO ()
startGame g@(Game p _ _ _ _)
        | p /= A    = mainLoop g -- Just wait for opponent's move
        | otherwise = do
                let
                    mv :: Move
                    mv = makeMove g
                    ng :: Game -- New game state
                    ng = updateGame mv g
                --print g
                putStrLn $ prettyMove mv
                mainLoop ng

-- Main game loop:
--  get enemy move >> update board >> make move >> updade board
mainLoop :: Game -> IO ()
mainLoop g = do
        mvStr <- getLine
        let
            cg :: Game -- Current game state
            cg = updateGame (readMove mvStr) g
            mv :: Move
            mv = makeMove cg
            ng :: Game -- New game state
            ng = updateGame mv cg
        --print cg
        putStrLn $ prettyMove mv -- Print move in defined format
        mainLoop ng
