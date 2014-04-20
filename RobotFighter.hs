-- file: RobotFighter.hs
-- author: alemedeiros <alexandre.n.medeiros _at_ gmail.com>
--
-- Name:  Alexandre Novais de Medeiros
-- RA:    115966
--
-- Program for the MC346 robot fight competition.

import Control.Monad
import Data.List.Split
import System.IO

--
-- Data types
--

type Level  = Int
type Board  = [[Square]]
type Size   = (Int,Int)

data Player = A | B deriving (Eq, Read, Show)

data Robot = Robot { robotPlayer :: Player
                   , robotLevel  :: Level
           }
           deriving (Show, Eq)

data PowerUp = PowerUp Level deriving (Show, Eq)

data Square = Empty               -- ".."
            | Wall                -- "XX"
            | P PowerUp           -- PowerUp / Resource
            | R Robot             -- Robot
            deriving (Eq, Show)

data Game = Game { player    :: Player
                 , boardSize :: Size
                 , board     :: Board
         }
         deriving (Show)

data Move = Move { who :: Player
                 , originX :: Int
                 , originY :: Int
                 , destinationX :: Int
                 , destinationY :: Int
         }
         deriving (Read, Show)

--
-- Main function
--
main :: IO ()
main = do
        hSetBuffering stdout LineBuffering
        -- Get game informations
        pStr <- getLine
        szStr <- getLine
        let p = whoAmI pStr
        let sz = readSize $ words szStr
        brdStr <- replicateM (fst sz) getLine
        let brd = map (map readSquare) $ map (chunksOf 2) brdStr
        let game = Game p sz brd
        -- Start playing
        startGame game

-- Starting game move:
--  if I am A, I play,
--  if I am B, I enter mainLoop
startGame :: Game -> IO a
startGame g
        | player g /= A = mainLoop g
        | otherwise = do
                putStrLn $ prettyMove mv
                mainLoop ng
                        where
                                mv = makeMove g
                                ng = updateGame mv g

-- Main game loop:
--  get enemy move >> update board >> make move >> updade board
mainLoop :: Game -> IO a
mainLoop g = do
        mStr <- getLine
        let currGame = updateGame (readMove mStr) g
        let mv = makeMove currGame
        putStrLn $ prettyMove mv
        let newGame = updateGame mv currGame
        mainLoop newGame

--
-- Playing functions
--

-- Update Game board acording to the specified Move
updateGame :: Move -> Game -> Game
updateGame _ g = g

-- Make a move based on the game available.
makeMove :: Game -> Move
makeMove g = Move (player g) 0 0 1 0

--
-- Parsing functions
--

-- Determine which player I am!
whoAmI :: String -> Player
whoAmI str
        | any (str==) ["A", "B"] = read str
        | otherwise = error "I can't determine who I am! HALP! :-O"

-- Determine the size of the board, receive a 2 String list.
--  The first String is the height and the second is the width of the board.
readSize :: [String] -> Size
readSize [h, w]  = (read h, read w)
readSize _ = error "I can't understand the size of the board! :-("

-- Determine which square a 2 character String represents.
readSquare :: String -> Square
readSquare "XX" = Wall
readSquare ".." = Empty
readSquare (x:xs)
        | x == 'R' = P . PowerUp $ read xs
        | any (x==) ['A', 'B'] = R $ Robot (read [x]) (read xs)
        | otherwise = error $ "I can't understand this board square:" ++ x:xs ++ " :-("

-- Pretty print move to a String
prettyMove :: Move -> String
prettyMove m = unwords [show $ who m, show $ originX m, show $ originY m, show $ destinationX m, show $ destinationY m]

-- Parse a move from a String
readMove :: String -> Move
readMove str = Move (read $ w!!0) (read $ w!!1) (read $ w!!2) (read $ w!!3) (read $ w!!4)
        where
                w = words str
