-- file: InfiniteImprobabilityDrive.hs
-- author: alemedeiros <alexandre.n.medeiros _at_ gmail.com>
--
-- Name:  Alexandre Novais de Medeiros
-- RA:    115966
--
-- Program for the MC346 robot fight competition.
--
-- Move decision making part of DeepThought.

module InfiniteImprobabilityDrive where

import DataAcquisitor
import qualified Data.Foldable as F
import qualified Data.Sequence as S

--
-- Playing functions
--

-- Update Game board acording to the specified Move.
updateGame :: Move -> Game -> Game
updateGame m (Game p sz b) = Game p sz $ updateBoard m b

-- Specifically, update the board with the given Move.
updateBoard :: Move -> Board -> Board
updateBoard (Move _ si sj di dj) b = setSquare (si,sj) Empty $ setSquare (di,dj) res b
        where
                src = getSquare (si,sj) b
                dst = getSquare (di,dj) b
                res = moveSquare src dst

-- Set a specific Square to the given coordinates.
setSquare :: Coord -> Square -> Board -> Board
setSquare (i,j) s b = S.update i updatedLine b
        where
                line = S.index b i
                updatedLine = S.update j s line

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
        | l1 > l2 = R $ Robot p1 (l1-l2)
        | l2 > l1 = R $ Robot p2 (l2-l1)
        | otherwise = Empty

-- Make a move based on the game available.
makeMove :: Game -> Move
makeMove g = validMove g $ S.index (S.filter (anyValid g) (myRobots g)) 0

-- Make any valid move for a robot on the given coordinates
validMove :: Game -> Coord -> Move
validMove g@(Game p (h,w) _) (i,j) = Move p i j di dj
        where
                di = fst dest
                dj = snd dest
                -- destination coordinates
                dest :: Coord
                dest | i < h-1 && isValid g (i+1,j) = (i+1,j)
                     | j < w-1 && isValid g (i,j+1) = (i,j+1)
                     | i > 1   && isValid g (i-1,j) = (i-1,j)
                     | j > 1   && isValid g (i,j-1) = (i,j-1)


-- The Robot in these coordinates has any valid movement?
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

-- Check if Square is Empty.
isEmpty :: Square -> Bool
isEmpty Empty = True
isEmpty _ = False

-- Check if Square has an enemy robot.
isEnemy :: Player -> Square -> Bool
isEnemy m (R (Robot p _))
        | m /= p = True
        | otherwise = False
isEnemy _ _ = False

-- Check if Square has resources.
isResource :: Square -> Bool
isResource (P _) = True
isResource _ = False


-- Find my Robots
-- TODO: keep my robots in a Seq inside the Game data type.
myRobots :: Game -> S.Seq Coord
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
