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

import qualified Data.Foldable as F
import Data.List.Split
import Data.Sequence as S

--
-- Data types
--

type Level   = Int
type Board   = Seq (Seq Square)
type Size    = (Int,Int)
type Coord   = (Int,Int)
type Team    = Seq Coord

data Player  = A | B deriving (Eq, Read, Show)

data Robot   = Robot Player Level deriving (Show, Eq)
data PowerUp = PowerUp Level deriving (Show, Eq)
data Square  = Empty               -- ".."
             | Wall                -- "XX"
             | P PowerUp           -- PowerUp / Resource
             | R Robot             -- Robot
             deriving (Eq, Show)

data Game    = Game Player Size Board Team Team
         deriving (Show)
data Move    = Move Player Int Int Int Int
         deriving (Read, Show)

--
-- Parsing functions
--

-- Build board from list of string (each String is a line for the Board)
readBoard :: [String] -> Board
readBoard = squaresToBoard . stringToSquares . map (chunksOf 2)
        where
                stringToSquares = map (map readSquare)
                squaresToBoard  = fromList . map fromList

-- Determine which player I am!
whoAmI :: String -> Player
whoAmI str
        | str `elem` ["A", "B"] = read str
        | otherwise             = error "can't determine which player I am"

-- Return opponent of player
opponent :: Player -> Player
opponent A = B
opponent B = A

-- Determine the size of the board, receive a 2 String list.
--  The first String is the height and the second is the width of the board.
readSize :: [String] -> Size
readSize [h, w]  = (read h, read w)
readSize _       = error "can't understand the size of the board"

-- Determine which square a 2 character String represents.
readSquare :: String -> Square
readSquare ['X','X']    = Wall
readSquare ['.','.']    = Empty
readSquare []           = error "empty string"
readSquare (x:xs)
        | x == 'R'      = P . PowerUp $ read xs
        | x `elem` "AB" = R $ Robot (read [x]) (read xs)
        | otherwise     = error $ "invalid square: " ++ x:xs

-- Parse a move from a String
readMove :: String -> Move
readMove str = Move (read p) (read si) (read sj) (read di) (read dj)
        where
                [p, si, sj, di, dj] = words str

--
-- Search functions
--

-- Find Robots
-- O(n x m) search for robots. Use only if necessary.
findRobots :: Player -> Board -> Seq Coord
findRobots p b = fmap (\(i,j,_) -> (i,j)) . F.foldl1 (><) $ fmap onlyRobots indexedBoard
        where
                indexedBoard = mapWithIndex (\i l -> mapWithIndex (ind i) l) b

                ind :: Int -> Int -> Square -> (Int,Int,Square)
                ind i j s = (i,j,s)

                onlyRobots = S.filter (\(_,_,sq) -> isRobot p sq)

-- Get the square of the given coordinates.
getSquare :: Board -> Coord -> Square
getSquare b (i,j) = index (index b i) j

--
-- Updating functions
--

-- Update Game board according to the specified Move.
updateGame :: Move -> Game -> Game
updateGame m (Game p sz b t e) = Game p sz nb nt ne
        where
                -- new elements
                nb = updateBoard m b
                nt = updateTeam m nb p t
                ne = updateTeam m nb o e
                o  = opponent p

-- Specifically, update the board with the given Move.
updateBoard :: Move -> Board -> Board
updateBoard (Move _ si sj di dj) b = setSquare (si,sj) Empty $ setSquare (di,dj) res b
        where
                src = getSquare b (si,sj)
                dst = getSquare b (di,dj)
                res = moveSquare src dst

-- Update robots position
updateTeam :: Move -> Board -> Player -> Team -> Team
updateTeam (Move mp si sj di dj) b p t
        | mp == p && isRobot p dest       = (di,dj) <| remove (si,sj)
        | mp /= p && not (isRobot p dest) = remove (di,dj)
        | otherwise                       = remove (si,sj)
        where
                remove c = S.filter (/= c) t
                dest = getSquare b (di,dj)

-- Set a specific Square to the given coordinates.
setSquare :: Coord -> Square -> Board -> Board
setSquare (i,j) s b = update i updatedLine b
        where
                line = index b i
                updatedLine = update j s line

-- Determine the resulting square of moving the first square towards the second.
moveSquare :: Square -> Square -> Square
moveSquare (R r) (P p) = R $ powerUp r p
moveSquare (R r) Empty = R r
moveSquare (R r1) (R r2) = battle r1 r2
moveSquare _ _ = error "invalid move"

-- Determine the resulting Robot when it gets a PowerUp
powerUp :: Robot -> PowerUp -> Robot
powerUp (Robot p lr) (PowerUp lp) = Robot p (lr+lp)

-- Determine the result of a battle between two robots.
battle :: Robot -> Robot -> Square
battle (Robot p1 l1) (Robot p2 l2)
        | l1 > l2  = R $ Robot p1 (l1-l2)
        | l2 > l1  = R $ Robot p2 (l2-l1)
        | otherwise = Empty

--
-- Type check functions
--

-- Check if Square is Empty.
isEmpty :: Square -> Bool
isEmpty Empty = True
isEmpty _ = False

-- Check if Square has an enemy robot.
isEnemy :: Player -> Square -> Bool
isEnemy m (R (Robot p _))
        | m /= p    = True
        | otherwise = False
isEnemy _ _         = False

-- Check if Square has player's robot.
isRobot :: Player -> Square -> Bool
isRobot m (R (Robot p _))
        | m == p    = True
        | otherwise = False
isRobot _ _         = False

-- Check if Square has resources.
isResource :: Square -> Bool
isResource (P _) = True
isResource _ = False

--
-- Pretty priting functions
--

-- Pretty print move to a String.
prettyMove :: Move -> String
prettyMove (Move p si sj di dj) = unwords [show p, show si, show sj, show di, show dj]
