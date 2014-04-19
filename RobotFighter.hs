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

-- main function
main :: IO ()
main = do
        hSetBuffering stdout LineBuffering
        pStr <- getLine
        szStr <- getLine
        boardStr <- replicateM (fst $ parseSize szStr) getLine
        print $ parsePlayer pStr
        print $ parseSize szStr
        print $ parseBoard boardStr
                where
                        -- Parse strings to data structures
                        parsePlayer :: String -> Player
                        parsePlayer str = whoAmI str
                        parseSize :: String -> Size
                        parseSize str = readSize $ words str
                        parseBoard :: [String] -> Board
                        parseBoard str = map (map readSquare) $ map (chunksOf 2) str

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
