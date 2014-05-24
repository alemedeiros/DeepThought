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
import Data.List
import qualified Data.Sequence as S

--
-- Playing functions
--

-- Make a move based on the game available.
makeMove :: Game -> Move
makeMove g@(Game p _ _ _ _) = snd $ alphaBetaPruning g p 5

-- Get best next move, considering output funtion as a measure.
bestMove :: Game -> Player -> Move
bestMove g p = snd . maximumBy (\(g0,_) (g1,_) -> ordGame g0 g1) . map (\m -> (updateGame m g, m)) $ validMoves g p

-- Returna list of all the valid moves for player mp on game g.
validMoves :: Game -> Player -> [Move]
validMoves g@(Game p _ _ t e) mp
        | p == mp   = allMoves g p t
        | otherwise = allMoves g p e

-- Generates all moves for a player, based on the given team.
allMoves :: Game -> Player -> Team -> [Move]
allMoves g p = F.foldl (++) [] . fmap (allRobotMoves g p)

-- generate all moves for the robot on the specified coordinates.
allRobotMoves :: Game -> Player -> Coord -> [Move]
allRobotMoves (Game _ sz b _ _) p (i,j) = map (uncurry (Move p i j)) valid
        where
                valid = filter (isValid b sz p) moves
                moves = [(i+1,j), (i,j+1), (i-1,j), (i,j-1)]

-- Check if the specified coordinates are valid for a move destination for a
-- player
isValid :: Board -> Size -> Player -> Coord -> Bool
isValid b (h,w) p c@(i,j)
        | i < 0 || i > h-1 = False
        | j < 0 || j > w-1 = False
        | otherwise        = isEmpty sq || isEnemy p sq || isResource sq
        where
                sq = getSquare b c

--
-- Alpha Beta Pruning functions
--

-- Main alpha beta pruning function.
alphaBetaPruning :: Game -> Player -> Int -> (Int,Move)
alphaBetaPruning g p 1 = let res = output g
                         in (res, bestMove g p)
alphaBetaPruning g@(Game gp _ _ t e) p d
        | gp == p   = resMax . foldl (maxFunction g d) (-99999,99999,head mt) $ mt -- Max
        | otherwise = resMin . foldl (minFunction g d) (-99999,99999,head me) $ me -- Min
       where
               resMax (a,_,mv) = (a,mv)
               resMin (_,b,mv) = (b,mv)
               mt = allMoves g p t
               me = allMoves g p e

-- Maximizing step of alpha beta pruning algorithm.
maxFunction :: Game -> Int -> (Int,Int,Move) -> Move -> (Int,Int,Move)
maxFunction g@(Game p _ _ _ _) d (a,b,m) nm
        | a >= b    = (a,     b,  m)  -- Pruned
        | nmRes > a = (nmRes, b, nm)
        | otherwise = (a,     b,  m)
       where
               op    = opponent p
               nmRes = fst . alphaBetaPruning (updateGame nm g) op $ d-1

-- Minimizing step of alpha beta pruning algorithm.
minFunction :: Game -> Int -> (Int,Int,Move) -> Move -> (Int,Int,Move)
minFunction g@(Game p _ _ _ _) d (a,b,m) nm
        | a >= b    = (a,     b,  m)  -- Pruned
        | nmRes < b = (a, nmRes, nm)
        | otherwise = (a,     b,  m)
       where
               nmRes = fst . alphaBetaPruning (updateGame nm g) p $ d-1

--
-- Game state analysis functions
--

-- Ordering function for Moves, based on the output function applied on the
-- updated Game.
ordMove :: Game -> Move -> Move -> Ordering
ordMove g m0 m1 = ordGame (updateGame m0 g) (updateGame m1 g)

-- Ordering function for Games, based on the output function.
ordGame :: Game -> Game -> Ordering
ordGame g0 g1 = compare o0 o1
        where
                o0 = output g0
                o1 = output g1

-- Determine the level of a robot inside a specific square.
robotLevel :: Square -> Int
robotLevel (R (Robot _ l)) = l
robotLevel _               = 0

-- Determine the output of a game: sum of my level - sum of opponent level.
output :: Game -> Int
output (Game _ _ b t e)
        | e == S.empty = 10000
        | t == S.empty = -10000
        | otherwise    = 20 * myTeam - 20 * opTeam + myLevel - 2 * opLevel
        where
                robots  = fmap (robotLevel . getSquare b)
                myLevel = F.foldl (+) 0 $ robots t
                opLevel = F.foldl (+) 0 $ robots e
                myTeam  = S.length t
                opTeam  = S.length e
