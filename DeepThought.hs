-- file: DeepThought.hs
-- author: alemedeiros <alexandre.n.medeiros _at_ gmail.com>
--
-- Name:  Alexandre Novais de Medeiros
-- RA:    115966
--
-- Program for the MC346 robot fight competition.

import Control.Monad
import qualified Data.Foldable as F
import Data.List.Split
import qualified Data.Sequence as S
import System.IO

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
            board = readBoard brdStr
            game = Game p sz board -- $ findTeam board
        -- Start playing :-D
        startGame game

-- Starting game move:
--  if I am A, make my first move;
--  if I am B, wait for A's move (entering main loop).
startGame :: Game -> IO ()
startGame g@(Game p _ _)
        | p /= A = mainLoop g -- Just wait for opponent's move
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
updateBoard (Move _ si sj di dj) b = setSquare (si,sj) Empty $ setSquare (di,dj) res b
        where
                src = getSquare (si,sj) b
                dst = getSquare (di,dj) b
                res = moveSquare src dst

setSquare :: (Int,Int) -> Square -> Board -> Board
setSquare (i,j) s b = S.update i updatedLine b
        where
                line = S.index b i
                updatedLine = S.update j s line


moveSquare :: Square -> Square -> Square
moveSquare (R r) (P p) = R $ powerUp r p
moveSquare (R r) Empty = R r
moveSquare (R r1) (R r2) = battle r1 r2
moveSquare _ _ = error "invalid move"

powerUp :: Robot -> PowerUp -> Robot
powerUp (Robot p lr) (PowerUp lp) = Robot p (lr+lp)

battle :: Robot -> Robot -> Square
battle (Robot p1 l1) (Robot p2 l2)
        | l1 > l2 = R $ Robot p1 (l1-l2)
        | l2 > l1 = R $ Robot p2 (l2-l1)
        | otherwise = Empty

-- Make a move based on the game available.
makeMove :: Game -> Move
makeMove g = validMove g $ S.index (S.filter (anyValid g) (myRobots g)) 0

-- Make any valid move for a given Robot
validMove :: Game -> (Int,Int) -> Move
validMove g@(Game p (h,w) _) (i,j) = Move p i j di dj
        where
                di = fst dest
                dj = snd dest
                dest
                        | i < h-1 && isValid g (i+1,j) = (i+1,j)
                        | j < w-1 && isValid g (i,j+1) = (i,j+1)
                        | i > 1   && isValid g (i-1,j) = (i-1,j)
                        | j > 1   && isValid g (i,j-1) = (i,j-1)


-- This Robot has any valid movement?
anyValid :: Game -> Coord -> Bool
anyValid g@(Game _ (h,w) _) (i,j)
        | i < h-1 && isValid g (i+1,j) = True
        | j < w-1 && isValid g (i,j+1) = True
        | i > 1   && isValid g (i-1,j) = True
        | j > 1   && isValid g (i,j-1) = True
        | otherwise = False

-- Check if the specified coordinates are valid for a move
isValid :: Game -> Coord -> Bool
isValid (Game p _ b) c = isEmpty sq || isEnemy p sq || isResource sq
        where
                sq = getSquare c b

-- Check if Square is Empty
isEmpty :: Square -> Bool
isEmpty Empty = True
isEmpty _ = False

-- Check if Square has an enemy robot
isEnemy :: Player -> Square -> Bool
isEnemy m (R (Robot p _))
        | m /= p = True
        | otherwise = False
isEnemy _ _ = False

isResource :: Square -> Bool
isResource (P _) = True
isResource _ = False


-- Find my Robots
myRobots :: Game -> S.Seq Coord --S.Seq Square --[(Int,Int,Robot)]
myRobots (Game me _ b) = S.mapWithIndex (\_ (i,j,_) -> (i,j)) . F.foldl1 (S.><) $ S.mapWithIndex (\_ l -> onlyRobots l) indexedBoard
        where
                indexedBoard = S.mapWithIndex (\i l -> S.mapWithIndex (ind i) l) b

                ind :: Int -> Int -> Square -> (Int,Int,Square)
                ind i j s = (i,j,s)

                onlyRobots = S.filter isMyRobot

                isMyRobot :: (Int,Int,Square) -> Bool
                isMyRobot (_,_,R (Robot p _))
                        | me == p = True
                        | otherwise = False
                isMyRobot _ = False


-- Get the square of the given coordinates.
getSquare :: Coord -> Board -> Square
getSquare (i,j) b = S.index (S.index b i) j

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
