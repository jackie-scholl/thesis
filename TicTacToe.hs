{-# LANGUAGE EmptyCase #-}

import Data.Foldable (asum)
import Data.Maybe
import Data.List
import Control.Monad

data Player = X | O deriving (Eq, Show)
data EndState = Winner Player | Draw | NotOver deriving (Eq, Show)
type Board = [Maybe Player]
type Coord = Int

-- ----------------------------------------------------------------------------|---------|---------|


main :: IO ()
main = forM_ thing1 putStrLn
	where
		n :: Int
		n = 1

		thing1 :: [String]
		thing1 = map printBoard $ take (10-n) $ playGameAuto partialBoard
		
		playGameAuto :: Board -> [Board]
		playGameAuto init = iterate moveBest init

		playGame :: [Coord] -> [Board]
		playGame coords = scanl move emptyBoard coords

		partialBoard :: Board
		partialBoard = last $ playGame $ take n moveList

		moveList :: [Coord]
		moveList = [0, 1, 3, 2, 4, 5, 6]



emptyBoard :: Board
emptyBoard = replicate 9 Nothing

getEmpties :: Board -> [Coord]
getEmpties board = filter isSpaceEmpty [0..8]
	where
		isSpaceEmpty coord = isNothing $ board !! coord

countEmpties :: Board -> Int
countEmpties = length . getEmpties

getCoord :: Board -> Coord -> Maybe Player
getCoord = (!!)

getCurrentTurn :: Board -> Player
getCurrentTurn board = if ((countEmpties board) `mod` 2 == 0) then O else X

printBoard :: Board -> String
printBoard b = printGameState b ++ printBoard' b ++ "\n\n"
	where
		getRows :: Board -> [[Maybe Player]]
		getRows = chunksOf 3
		
		printSpace :: Maybe Player -> String
		printSpace (Just p) = show p
		printSpace (Nothing) = "_"
		
		printRow :: [Maybe Player] -> String
		printRow spaces = concat $ map printSpace spaces

		printBoard' :: Board -> String
		printBoard' board = intercalate "\n" $ map printRow $ getRows board
		
		printGameState :: Board -> String
		printGameState board = case getEndState board of
			NotOver  -> "Game is not yet over; current turn: "
				++ show (getCurrentTurn board) ++ "\n"
			Draw     -> "Game ended in a draw.\n"
			Winner w -> "Player " ++ show w ++ " won!\n"

getEndState :: Board -> EndState
getEndState board = result
	where
		allLines :: [[Coord]]
		allLines = [[0, 1, 2], [3, 4, 5], [6, 7, 8], -- horizontals
					[0, 3, 6], [1, 4, 7], [2, 5, 8], --verticals
					[0, 4, 8], [2, 4, 6]] -- diagonals

		-- ugly but works
		testLine :: [Maybe Player] -> Maybe Player
		testLine line = if (all (== Just X) line) then (Just X) else
						if (all (== Just O) line) then (Just O) else
						(Nothing)

		extractedLines :: [[Maybe Player]]
		extractedLines = map (map (getCoord board)) allLines

		firstJusts :: [Maybe a] -> Maybe a
		firstJusts = Data.Foldable.asum

		possibleWinner :: Maybe Player
		possibleWinner = firstJusts $ map testLine extractedLines

		result = case possibleWinner of
			Just winner -> Winner winner -- chicken dinner
			Nothing -> if (0 == countEmpties board) then Draw else NotOver


move :: Board -> Coord -> Board
move board coord
	| isJust $ getCoord board coord = undefined
	| otherwise = modifyAtIndex board coord $ const $ Just $ getCurrentTurn board  

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






-- Utility funtions        

chunksOf :: Int -> [x] -> [[x]]
chunksOf n xs
	| length xs <= n = [xs] 
	| otherwise = thing1 $ splitAt n xs
	where
		thing1 :: ([x], [x]) -> [[x]]
		thing1 (a, b) = cons a $ chunksOf n b

cons :: a -> [a] -> [a]
cons x xs = x : xs

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
