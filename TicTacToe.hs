module TicTacToe where

import Data.Char
import Data.Maybe
import Data.List
import Text.Read

-------------------------------------------------------------------
data Player = O | X
            deriving (Eq)

instance Show Player where
  show X = "Player X"
  show O = "Player O"

data Cell = Empty | Taken Player
          deriving (Eq)

instance Show Cell where
  show Empty = "-"
  show (Taken O) = "O"
  show (Taken X) = "X"

type Board = ([Cell], Int)

type Position = (Int, Int)

-------------------------------------------------------------------

--
-- Some useful functions from, or based on, the unassessed problem sheets...
--

-- Preserves Just x iff x satisfies the given predicate. In all other cases
-- (including Nothing) it returns Nothing.
filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe p m@(Just x)
  | p x = m
filterMaybe p _
  = Nothing

-- Replace nth element of a list with a given item.
replace :: Int -> a -> [a] -> [a]
replace 0 p (c : cs)
  = p : cs
replace _ p []
  = []
replace n p (c : cs)
  = c : replace (n - 1) p cs

-- Returns the rows of a given board.
rows :: Board -> [[Cell]]
rows (cs , n) = rows' cs
  where
    rows' [] = []
    rows' cs = r : rows' rs
      where
        (r, rs) = splitAt n cs

-- Returns the columns of a given board.
cols :: Board -> [[Cell]]
cols = transpose . rows

-- Returns the diagonals of a given board.
diags :: Board -> [[Cell]]
diags (cs, n)
  = map (map (cs !!)) [[k * (n + 1) | k <- [0 .. n - 1]],
                      [k * (n - 1) | k <- [1 .. n]]]

-------------------------------------------------------------------

-- Cell = Empty | Taken Player 
-- type Board = ([Cell], Int)
-- for all rows/cols/diags in the board, check for instances of O and X by nub
-- then, check for the existence of singleton in such list 
gameOver :: Board -> Bool
gameOver b = or [x `elem` check | x <- [[Taken O], [Taken X]]]
  where 
    check = map nub (concat [rows b, cols b, diags b])

-- Checks whether the game has tied or not
-- the game has tied if gameOver is False and Empty is not in Cell
gameTie :: Board -> Bool 
gameTie b@(c, _) = not(gameOver b) && Empty `notElem` c 

-------------------------------------------------------------------

--
-- Moves must be of the form "row col" where row and col are integers
-- separated by whitespace. Bounds checking happens in tryMove, not here.
--

-- 1. parse them by white spaces from the given string
-- 2. apply readMaybe to the parsed string
-- 3. if not Int, return Nothing
-- 4. otherwise, extract Int from the Maybe Int and return Maybe Position
parsePosition :: String -> Maybe Position
parsePosition xs 
  | isNothing c || isNothing d = Nothing
  | otherwise = Just (fromJust c, fromJust d)
  where
    (a, b) = break isSpace xs
    (c, d) = (readMaybe a :: Maybe Int, readMaybe b :: Maybe Int)  

-- type Board = ([Cell], Int)
-- the corresponding index depends on the pos and board size, i = p1 * n * p2
-- if i is negative, it is not in the board
-- if i is greater than the cell size, it is not in the board
-- is i is already taken, output Nothing
-- otherwise, Just replace it in the cell 
tryMove :: Player -> Position -> Board -> Maybe Board
tryMove p (p1, p2) (c, n)
  | i < 0 = Nothing 
  | i > length c = Nothing
  | c !! i /= Empty = Nothing 
  | otherwise = Just (replace i (Taken p) c, n)
  where 
    i = p1 * n + p2
-------------------------------------------------------------------
-- I/O Functions

-- show the board row by row on the terminal screen
prettyPrint :: Board -> IO ()
prettyPrint b = do 
  let r = rows b
  let o = concatMap ((++"\n") . show) r 
  putStrLn ("\n" ++ o)


-- Manage user movement by repeatedly: 
-- 1. take user input for target board position
--    in the form of "Int Int"
-- 2. if the input is valid, return the modified new board,
--    otherwise, continue executing takeTurn until the move
--    is successful 
takeTurn :: Board -> Player -> IO Board
takeTurn b p = do 
  putStrLn ("Where do you want to place " ++ show p ++ "?:")
  x <- getLine
  case parsePosition x of 
    Just x -> do 
      case tryMove p x b of 
        Just newBoard -> do 
          return newBoard 
        Nothing -> do 
          putStrLn "Invalid move. Please try again."
          takeTurn b p 
    Nothing -> do 
      putStrLn "Invalid position. Please try again."
      takeTurn b p

-- Manage a game by repeatedly: 
-- 1. print the current board
-- 2. use takeTurn to modify the board
-- 3. check if the game is over, or tied;
-- 4. print suitable message if the game is done,
--    otherwise continue to execute the function with new board.
--    Regognize the next user by selecting element in the players 
--    list that is not the current player. 
playGame :: Board -> Player -> IO ()
playGame b p = do 
  let ps = [O, X]
  prettyPrint b 
  newBoard <- takeTurn b p 
  if gameOver newBoard
    then do 
      prettyPrint newBoard
      putStrLn ("The winner was : " ++ show p)
      putStrLn "Thank you for playing TicTacToe."
      
    else do 
      if gameTie newBoard
        then do 
          prettyPrint newBoard
          putStrLn "Your game has tied; no one has won."
          putStrLn "Thank you for playing TicTacToe."
        else do 
          putStrLn "\nNext Player takes turn."
          playGame newBoard (head (filter (/=p) ps))

-- Manage game replay by repeatedly: 
-- 1. take user input for replay
-- 2. if Y, then re-execute main
-- 3. if N, then terminate main
-- 4. otherwise, re-execute replay
replay :: IO()
replay = do 
  putStrLn "\nPlease input Y/N for replay!"
  a <- getLetter
  case a of 
    'Y' -> do
      putStrLn "\nReloading game..."
      putStrLn "--------------------------------"
      main 
    'N' -> do 
      putStrLn "\nHope you have a great day."
    _  -> do
      putStrLn "Invalid input. Please try again."
      replay

-- handy helper function that takes a character 
-- from the getLine prelude
getLetter :: IO Char
getLetter = do
    -- getLine :: IO String
    cs <- getLine
    case cs of
        [c] -> return c 
        _   -> do 
          putStrLn "Please input a letter, not string."
          getLetter

-- Define turn by repeatedly: 
-- 1. take user input for turn1
-- 2. if the input is valid, check whether it is >=1.
--    otherwise, re-execute defineTurn
-- 3. if the input >=1, signal the start of the game, 
--    and return the corresponding Player 
--    otherwise, re-execute defineTurn 
defineTurn :: IO Player
defineTurn = do 
  putStrLn "Please decide who to play first."
  t <- getLetter
  let valid = ['O', 'X']
  if t `elem` valid
    then do 
      let n = filter (==t) valid
      putStrLn ("Please take your turn, Player " ++ n ++ ".")
      putStrLn "-------------------------------------------"
      let p = [("O", O), ("X", X)]
      return (fromJust(lookup n p))
    else do 
      putStrLn "Invalid input. Please input either \'O\' or \'X\'."
      defineTurn

-- Define board by repeatedly: 
-- 1. tkae user input for board size 
-- 2. if the input is valid, read the input as Int
--    and create the empty board accordingly.
--    otherwise, re-execute defineBoard
defineBoard :: IO Board 
defineBoard = do 
  putStrLn "Please input the size of the board: "
  s <- getLine
  case readMaybe s :: Maybe Int of 
    Just s -> do 
      if s >= 1 
        then do 
          putStrLn ("The game will be played on board size " ++ show s ++ ".")
          putStrLn "---------------------------------------------------------"
          let board = (replicate (s^2) Empty, s)
          return board
        else do 
          putStrLn "Invalid board size. Please input integer >= 1."
          defineBoard
    Nothing -> do 
      putStrLn "Invalid input. Please input an integer."
      defineBoard


-- Print a welcome message, decide first turn, read the board dimension, 
-- invoke playGame and exit with a suitable message.

main :: IO ()
main = do
  putStrLn "Welcome to TicTacToe! \n"

  p1 <- defineTurn
  b <- defineBoard
  playGame b p1 
  replay

  return ()






  
-------------------------------------------------------------------

-- extra testBoards added for test cases 
testBoard1, testBoard2, testBoard3, testBoard4, testBoard5 :: Board

testBoard1
  = ([Taken O,Taken X,Empty,Taken O,
      Taken O,Empty,Taken X,Taken X,
      Taken O,Empty,Empty,Taken X,
      Taken O,Taken X,Empty,Empty],
      4)

testBoard2
  = ([Taken X,Empty,
      Empty,Empty],
      2)

testBoard3
  = ([Taken O,Taken X,Empty,Taken O,Taken X,
      Taken O,Empty,Taken X,Taken X,Empty,
      Empty,Empty,Taken X,Taken O,Taken O,
      Taken O,Taken X,Empty,Empty,Taken X,
      Taken X,Empty,Taken O,Empty,Empty],
      5)

testBoard4 
  = ([Taken O, Taken X, Taken O,
      Taken X, Taken O, Taken X, 
      Taken X, Taken O, Taken X], 
      3)

testBoard5 
  = ([Empty, Empty, Empty,
      Empty, Empty, Empty,
      Empty, Empty, Empty],
      3)
