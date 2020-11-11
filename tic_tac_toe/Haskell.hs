import Data.Foldable (asum)
import Data.Maybe
import Data.List

data Player = X | O deriving (Eq, Show)
data EndState = Winner Player | Draw | NotOver deriving (Eq, Show)
type Board = [Maybe Player]
type Coord = Int


emptyBoard :: Board
emptyBoard = replicate 9 Nothing

getEmpties board =
	let isSpaceEmpty coord = isNothing $ board !! coord
	in  filter isSpaceEmpty [0..8]

countEmpties = getEmpties

getCurrentTurn :: Board -> Player
getCurrentTurn board = if ((countEmpties board) `mod` 2 == 0) then O else X

getCoord :: Board -> Coord -> Maybe Player
getCoord = (!!)

printBoard :: Board -> String
printBoard b =
	let
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
	in
		printGameState b ++ printBoard' b ++ "\n\n"

getEndState :: Board -> EndState
getEndState board =
	let
		allLines :: [[Coord]]
		allLines = [[0, 1, 2], [3, 4, 5], [6, 7, 8], -- horizontals
					[0, 3, 6], [1, 4, 7], [2, 5, 8], --verticals
					[0, 4, 8], [2, 4, 6]] -- diagonals

		-- ugly but works
		testLine :: [Maybe Player] -> Maybe Player
		testLine line = case line of
			(Just X : Just X : Just X : []) -> Just X
			(Just O : Just O : Just O : []) -> Just O
			_ -> Nothing
			--if (all (== Just X) line) then (Just X) else
			--if (all (== Just O) line) then (Just O) else
			--(Nothing)

		extractedLines :: [[Maybe Player]]
		extractedLines = map (map (getCoord board)) allLines





		firstJusts :: [Maybe a] -> Maybe a
		firstJusts = Data.Foldable.asum


		possibleWinner :: Maybe Player
		possibleWinner = firstJusts $ map testLine extractedLines
		
	in case possibleWinner of
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
		Draw -> 0
		NotOver -> recursiveScore
		(Winner x)  -> if (x == player) then 1 else -1

chooseMove :: Board -> Coord
chooseMove board =
	let
		moveScore coord = score (move board coord) (getCurrentTurn board)
	in
		argmax moveScore $ getEmpties board



main :: IO ()
main = 
	let
		initialBoard :: Board
		initialBoard = move (move emptyBoard 0) 4

		gameSequence :: [Board]
		gameSequence = take 8 $ iterate moveBest $ initialBoard

		printGame :: IO ()
		printGame = mapM_ putStrLn $ map printBoard $ gameSequence
	in
		printGame

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
