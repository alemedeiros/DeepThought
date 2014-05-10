-- file: DataAcquisitor.hs
-- author: alemedeiros <alexandre.n.medeiros _at_ gmail.com>
--
-- Name:  Alexandre Novais de Medeiros
-- RA:    115966
--
-- Program for the MC346 robot fight competition.
--
-- Parsing and data-related part of DeepThought.

module DataAcquisitor where

import Data.List.Split
import qualified Data.Sequence as S

--
-- Data types
--

type Level  = Int
type Board  = S.Seq (S.Seq Square)
type Size   = (Int,Int)
type Coord  = (Int,Int)

data Player = A | B deriving (Eq, Read, Show)

data Robot = Robot Player Level
           deriving (Show, Eq)

data PowerUp = PowerUp Level deriving (Show, Eq)

data Square = Empty               -- ".."
            | Wall                -- "XX"
            | P PowerUp           -- PowerUp / Resource
            | R Robot             -- Robot
            deriving (Eq, Show)

data Game = Game Player Size Board
         deriving (Show)

data Move = Move Player Int Int Int Int
         deriving (Read, Show)

--
-- Parsing functions
--

-- Build board from list of string (each String is a line for the Board)
readBoard :: [String] -> Board
readBoard str = S.fromList . map S.fromList . boardToList $ map (chunksOf 2) str
        where
                boardToList :: [[String]] -> [[Square]]
                boardToList = map (map readSquare)

-- Determine which player I am!
whoAmI :: String -> Player
whoAmI str
        | str `elem` ["A", "B"] = read str
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
        | x `elem` "AB" = R $ Robot (read [x]) (read xs)
        | otherwise = error $ "I can't understand this board square:" ++ x:xs ++ " :-("

-- Parse a move from a String
readMove :: String -> Move
readMove str = Move (read $ head w) (read $ w!!1) (read $ w!!2) (read $ w!!3) (read $ w!!4)
        where
                w = words str

--
-- Pretty priting functions
--

-- Pretty print move to a String.
prettyMove :: Move -> String
prettyMove (Move p si sj di dj) = unwords [show p, show si, show sj, show di, show dj]
