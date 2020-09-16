{-# LANGUAGE EmptyCase #-}

import Data.Maybe
import Data.List
--import Debug.Trace
import Control.Monad

data Player = X | O deriving (Eq, Show)
data EndState = Winner Player | Draw | NotOver deriving (Eq, Show)
type Board = [[Maybe Player]]
type Coord = (Int, Int)
--type Board = [Maybe Player]
--type Coord = Int


n :: Int
n = 1

main :: IO ()
main = forM_ (map printBoard $ take (10-n) $ playGameAuto partialBoard) putStrLn

playGameAuto :: Board -> [Board]
playGameAuto init = iterate helper init
	where
		helper h = move h $ chooseMove h

playGame :: [Coord] -> [Board]
playGame coords = scanl move emptyBoard coords

partialBoard :: Board
partialBoard = last $ playGame $ take n moveList

moveList :: [Coord]
moveList = [(0, 0), (0, 1), (1, 0), (0, 2), (1, 1), (1, 2), (2, 0)]
--moveList = [0, 1, 3, 2, 4, 5, 6]


printBoard :: Board -> String
printBoard b = printGameState b ++ printBoard' b ++ "\n\n"
	where
		printSpace :: Maybe Player -> String
		printSpace (Just p) = show p
		printSpace (Nothing) = "_"
		
		printRow :: [Maybe Player] -> String
		printRow spaces = concat $ map printSpace spaces
		
		printBoard' :: [[Maybe Player]] -> String
		printBoard' rows = intercalate "\n" $ map printRow rows
		
		printGameState :: Board -> String
		printGameState board = case getEndState board of
			NotOver  -> "Game is not yet over; current turn: " ++ show (getCurrentTurn board) ++ "\n"
			Draw     -> "Game ended in a draw.\n"
			Winner w -> "Player " ++ show w ++ " won!\n"


emptyBoard :: Board
emptyBoard = [emptyLine, emptyLine, emptyLine]
	where emptyLine = [Nothing, Nothing, Nothing]

isBoardRightSize :: Board -> Bool
isBoardRightSize board = (length board == 3) && and (map (== 3) $ map length board)

getEmpties :: Board -> [Coord]
getEmpties board = filter isSpaceEmpty allCoords
	where
		--allCoords = [0..8]
		allCoords = [(x, y) | x <- [0..2], y <- [0..2]]
		isSpaceEmpty coord = isNothing $ getCoord board coord

countEmpties :: Board -> Int
countEmpties = length . getEmpties

getCoord :: Board -> Coord -> Maybe Player
getCoord b (x, y) = (b !! x) !! y

getEndState :: Board -> EndState
getEndState board =
	case possibleWinner of
		Just winner -> Winner winner -- chicken dinner
		Nothing -> if (0 == countEmpties board) then Draw else NotOver
	where
		possibleWinner :: Maybe Player
		possibleWinner = foldl maybeOnly Nothing $ map testLine extractedLines

		extractedLines :: [[Maybe Player]]
		extractedLines = map (map (getCoord board)) allLines

		allLines :: [[Coord]]
		--allLines = [[0, 1, 2], [3, 4, 5], [6, 7, 8],
		--			[0, 3, 6], [1, 4, 7], [2, 5, 8],
		--			[0, 4, 8], [2, 4, 6]]
		allLines = [[(x, y) | x <- [0..2]] | y <- [0..2]] ++
				   [[(y, x) | x <- [0..2]] | y <- [0..2]] ++
				   [[(0,0),(1,1),(2,2)], [(2,0),(1,1),(0,2)]]

		maybeOnly :: (Eq a) => Maybe a -> Maybe a -> Maybe a
		maybeOnly Nothing Nothing = Nothing
		maybeOnly (Just x) Nothing = Just x
		maybeOnly Nothing (Just x) = Just x
		maybeOnly (Just x) (Just y) = if (x == y) then (Just x) else error "should never have two different values"
		
		-- ugly but works
		testLine :: [Maybe Player] -> Maybe Player
		testLine line = if (all (== Just X) line) then (Just X) else
						if (all (== Just O) line) then (Just O) else
						(Nothing)

--isGameOver :: Board -> Bool
--isGameOver board = if (getEndState board == NotOver) then False else True

getCurrentTurn :: Board -> Player
getCurrentTurn board = if ((countEmpties board) `mod` 2 == 0) then O else X

move :: Board -> Coord -> Board
move board coord@(x, y) 
	| isJust $ getCoord board coord = undefined
	| otherwise = modifyAtIndex board x rowModifier
	--modifyAtIndex board coord $ const $ Just $ getCurrentTurn board  
	where
		--thing1 = modifyAtIndex board x rowModifier
		rowModifier row = modifyAtIndex row y $ const $ Just $ getCurrentTurn board







moveBest :: Board -> Board
moveBest board = move board (chooseMove board)

score :: Board -> Player -> Double
score board player =
	let
		endState = getEndState board
		recursiveScore = 0.9 * score (moveBest board) player
	in case endState of
		(Winner x)  -> if (x == player) then 1 else -1
		Draw -> 0
		NotOver -> recursiveScore

chooseMove :: Board -> Coord
chooseMove board = argmax moveScore $ getEmpties board
	where moveScore coord = score (move board coord) (getCurrentTurn board)









-- | isGameOver board = error "I was asked to choose the best move, but the game is over"
--    | otherwise | isGameOver board = error "I was asked to choose the best move, but the game is over"
--        | otherwise 


-- Utility funtions        
modifyAtIndex :: [a] -> Int -> (a -> a) -> [a]
modifyAtIndex list index modifier 
	| (index >= length list) || (index < 0) = list -- index out of bounds; could throw error instead?
	| otherwise = (take index list) ++ [modifier $ list !! index] ++ (drop (index + 1) list)

argmax :: (Ord b) => (a -> b) -> [a] -> a
argmax _ [] = undefined
argmax f (y:ys) = argmaxHelper y (f y) f ys
	where
		argmaxHelper :: (Ord b) => a -> b -> (a -> b) -> [a] -> a
		argmaxHelper best _ _ [] = best
		argmaxHelper best bestTransformed transformer (x:xs) = 
			let
				xTransformed = transformer x
				(newBest, newBestTransformed) =
					if (bestTransformed >= xTransformed)
						then (best, bestTransformed)
						else (x, xTransformed)
			in
				argmaxHelper newBest newBestTransformed transformer xs

-- Haskell didn't allow me to try the complex, difficult work that Idris allowed.
-- I had an error where I had [Bool] but needed Boo
-- I'm a little scared of messing up, this program feels hard to read
-- It feels fuzzy in some sense
-- I confused checkWinner() with a game over check at first, didn't notice until
-- AI started throwing errors which were difficult to debug











