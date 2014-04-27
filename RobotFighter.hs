-- file: RobotFighter.hs
-- author: alemedeiros <alexandre.n.medeiros _at_ gmail.com>
--
-- Name:  Alexandre Novais de Medeiros
-- RA:    115966
--
-- Program for the MC346 robot fight competition.

import Control.Monad
import Data.List.Split
import Data.List
import System.IO

--
-- Data types
--

type Level  = Int
type Board  = [(Int,Int,Square)]
type Size   = (Int,Int)

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
-- Main function
--
main :: IO ()
main = do
        hSetBuffering stdout LineBuffering
        -- Get game informations
        pStr <- getLine
        szStr <- getLine
        let
            p = whoAmI pStr
            sz = readSize $ words szStr
        brdStr <- replicateM (fst sz) getLine
        let
            game = Game p sz $ readBoard brdStr
        -- Start playing :-D
        startGame game

-- Starting game move:
--  if I am A, make my first move;
--  if I am B, wait for A's move (entering main loop).
startGame :: Game -> IO ()
startGame g@(Game p _ _)
        | p /= A = mainLoop g -- Just wait for oponent's move
        | p == A = do
                let
                    mv :: Move
                    mv = makeMove g
                    ng :: Game -- New game state
                    ng = updateGame mv g
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
        putStrLn $ prettyMove mv
        mainLoop ng

--
-- Playing functions
--

-- Update Game board acording to the specified Move
updateGame :: Move -> Game -> Game
updateGame m (Game p sz b) = Game p sz $ updateBoard m b

updateBoard :: Move -> Board -> Board
updateBoard (Move _ si sj di dj) b = map updateSquare b
        where
                src = getSquare (si,sj) b
                dst = getSquare (di,dj) b
                res = moveSquare src dst
                updateSquare :: (Int,Int,Square) -> (Int,Int,Square)
                updateSquare e@(i,j,_)
                        | (i,j) == (si,sj) = (i,j,Empty)
                        | (i,j) == (di,dj) = (i,j,res)
                        | otherwise = e

moveSquare :: Square -> Square -> Square
moveSquare (R r) (P p) = R $ powerUp r p
moveSquare (R r) Empty = R r
moveSquare (R r1) (R r2) = battle r1 r2
moveSquare _ _ = error "wtf you trying to do?!"

powerUp :: Robot -> PowerUp -> Robot
powerUp (Robot p lr) (PowerUp lp) = Robot p (lr+lp)

battle :: Robot -> Robot -> Square
battle (Robot p1 l1) (Robot p2 l2)
        | l1 > l2 = R $ Robot p1 (l1-l2)
        | l2 > l1 = R $ Robot p2 (l2-l1)
        | otherwise = Empty

-- Make a move based on the game available.
makeMove :: Game -> Move
makeMove g = validMove g . head $ myRobots g

-- Make any valid move for a given Robot
validMove :: Game -> (Int,Int,Robot) -> Move
validMove (Game p (h,w) b) (i,j,_) = Move p i j di dj
        where
                di = fst dest
                dj = snd dest
                dest
                        | i < h-1 && isValid (i+1,j) = (i+1,j)
                        | j < w-1 && isValid (i,j+1) = (i,j+1)
                        | i > 1   && isValid (i-1,j) = (i-1,j)
                        | j > 1   && isValid (i,j-1) = (i,j-1)
                -- Check if the specified coordinates are valid for a move
                isValid :: (Int,Int) -> Bool
                isValid c = or [isEmpty sq, isEnemy p sq, isResource sq]
                        where
                                sq = getSquare c b

-- Check if Square is Empty
isEmpty :: Square -> Bool
isEmpty Empty = True
isEmpty _ = False

-- Check if Square has an enemy robot
isEnemy :: Player -> Square -> Bool
isEnemy me (R (Robot p _))
        | me /= p = True
        | otherwise = False
isEnemy _ _ = False

isResource :: Square -> Bool
isResource (P _) = True
isResource _ = False


-- Find my Robots
myRobots :: Game -> [(Int,Int,Robot)]
myRobots (Game me _ b) = map (\(i,j,R r) -> (i,j,r)) $ filter (isMyRobot) b
        where
                isMyRobot :: (Int,Int,Square) -> Bool
                isMyRobot (_,_,R (Robot p _))
                        | me == p = True
                        | otherwise = False
                isMyRobot _ = False

-- Get the square of the given coordinates.
getSquare :: (Int,Int) -> Board -> Square
getSquare src ((i,j,sq):brd)
        | src == (i,j) = sq
        | otherwise = getSquare src brd

--
-- Parsing functions
--

-- Build board from list of string (each String is a line for the Board)
readBoard :: [String] -> Board
readBoard str = foldl1 (++) $ zipWith readBoardLine [0..] str
        where
                -- i is the index of the line on the board
                readBoardLine :: Int -> String -> [(Int,Int,Square)]
                readBoardLine i line = zipWith (\j sq -> (i,j,readSquare sq)) [0..] $ chunksOf 2 line

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

-- Parse a move from a String
readMove :: String -> Move
readMove str = Move (read $ w!!0) (read $ w!!1) (read $ w!!2) (read $ w!!3) (read $ w!!4)
        where
                w = words str

--
-- Pretty priting functions
--

-- Pretty print move to a String.
prettyMove :: Move -> String
prettyMove (Move p si sj di dj) = unwords [show p, show si, show sj, show di, show dj]
