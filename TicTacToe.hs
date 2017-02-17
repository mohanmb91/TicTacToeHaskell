module TicTacToe (main, main', minimax, newGame, play, TTT) where

-- Based on a program by Peter Drake. 

import Control.Monad
import Data.Char
-- import Data.Foldable
import Data.Function
-- import History
import Data.List
import Data.Maybe
-- import Data.Ord

-- Constants and Types 

-- Use 1-based indexing at the user level but (of course)
-- 0-based indexing for Haskell lists.
rowColRange :: [Int] -- How users will refer to cells.
rowColRange = [1 .. 3] 

-- ?? What does deriving (Eq, Show) mean?
data Player = X | O deriving (Eq, Show)

-- ?? What are some example Outcome objects?
data Outcome = Winner Player | Draw 
instance Show Outcome where
  show (Winner p) = show p ++ " wins."
  show Draw       = "Drawn game."

-- ?? What are some example Mark objects?
data Mark = Mark Player | Empty deriving Eq
instance Show Mark where
  show (Mark p) = show p
  show Empty    = " "

-- A Row is three Marks.
type Row   = [Mark] --  [Mark, Mark, Mark]

-- The Board is three Rows.
-- ?? What do some example Boards with X's O'x and 
--    empty cells actually look like?
type Board = [Row]  -- [[Mark, Mark, Mark], 
                    --  [Mark, Mark, Mark], 
                    --  [Mark, Mark, Mark]]

-- The primary data structure. Keeps track of the game state.
data TTT = TTT { board        :: Board, 
                 -- The minimaxValue is the value of the game 
                 -- computed by minimax.
                 --  1 means X can win.
                 --  0 means each side can force a draw.
                 -- -1 means O can win.
                 minimaxValue :: Int,  
                 -- The max number of moves that can be forced
                 -- without worsening the minimaxValue for that player.
                 maxDepth     :: Int, 
                 -- Will be no more than 1 message, but it's
                 -- easier to treat it as a list of Strings.
                 message      :: [String] } deriving (Eq)

-- The initial TTT state.
newGame :: TTT
newGame = TTT { board        = replicate 3 $ replicate 3 Empty, 
                -- We know the initial minimaxValue is 0. 
                -- Would take too long to compute it.
                minimaxValue = 0, 
                maxDepth     = 9, 
                message      = ["New game."]                     } 

-- Define an ordering on TTT based on 
-- a) the minimaxValue and
-- b) how long the game can be stretched out--the longer the better.
-- Will be used in minimax when we are comparing two successors of
-- a game. Since we are comparing successors, we want to know whose 
-- move produced these two successors, i.e., whoJustMoved. Whether it 
-- was 'X's or 'O's move, we want to maximize maxDepth. BUT if 'O' 
-- produced these successors, we will be looking for a minimum. In
-- that case we want to negate maxDepth to maximize (abs maxDepth).
-- (Seems awkward and ad hoc. Is there a better way?)

-- ?? What are some example TTT objects that illustrate this ordering? 
-- How does the comparison function operate on them?
instance Ord TTT where
  compare = compare `on` measureOfMerit 

    where
    measureOfMerit :: TTT -> (Int, Int)
    measureOfMerit g = 
          (minimaxValue g, 
           maxDepth g * selectByPlayer whoJustMoved 1 (-1))

      where       
      whoJustMoved :: Player
      whoJustMoved = selectByPlayer (whoseMove g) O X
        
-- Define the show function for a TTT.
instance Show TTT where
  -- First generate a list of Strings, one for each line of output. 
  -- Then add "\n     " between Strings and at the start and end.
  -- When concatenated this will both indent and add "\n" to each line. 
  -- Finally concat the Strings into a single String.

  show = ("\n" ++) . unlines . fmap ("     " ++) . makeOutputStrings 
    where 

    -- Construct the output as a List of Strings by combining the
    -- error messages, the board, and the game status String.
    makeOutputStrings :: TTT -> [String]
    makeOutputStrings game = message game 
                             ++ makeBoardStrings (board game)
                             ++ [gameStatus, ""]
      where      
      -- Return a String indicating the status of the game.
      -- ?? What is the significance of having a case statement
      -- after an append (++) operator?
      gameStatus :: String                   
      gameStatus = case currentStatus game of
          -- currentStatus will return Just Outcome if the game is over.                
          Just outcome -> show outcome
          Nothing      -> case minimaxValue game of
                            1    -> show X ++ " can win." 
                            0    -> "Both sides can force a draw."
                            (-1) -> show O ++ " can win." 
                            _    -> "Error: minimax = " ++ show (minimaxValue game)

      -- Format the board for printing, but keep as a list of Strings.
      makeBoardStrings :: Board -> [String] 
      makeBoardStrings = intersperse "---+---+---" . fmap makeRowString 
        where
        -- Convert a row (a list of 3 Marks) into a String with dividers.
        -- E.g., [Mark X, Empty, Mark O] => "| X |   | O | "
        -- ?? What does the last show do? How does it work in this case?
        makeRowString :: Row -> String 
        makeRowString = (' ' :) . intercalate " | " . fmap show
--- End Constants and Types 

cellAt :: Int -> Int -> TTT -> Mark
cellAt r c game = board game !! (r-1) !! (c-1)

-- ?? What is Empty in the code below?
cellIsEmpty :: Int -> Int -> TTT -> Bool
cellIsEmpty r c game = cellAt r c game == Empty
                                    
-- Count the number of marks of a particular kind in the board. 
count :: Mark -> TTT -> Int
count mark game = length [c | row <- board game, c <- row, c == mark]

-- Return Maybe Outcome based on the current game state. 
--   Just (Winner Player) : Player has 3 in a row
--   Just Draw            : The board is full but no 3-in-a-row. 
--   Nothing              : neither of the above
-- ?? What are some illustrative input/output examples for this function?
currentStatus :: TTT -> Maybe Outcome 
currentStatus game = case findWinner (board game) of
    -- ?? Which function produces the following two possibilities?
    -- If (Mark Player) appears 3-in-a-row, return Just (Winner Player)
    Just p  -> Just (Winner p)
    -- If there is no winner, return Just Draw or Nothing depending
    -- on whether the board is full, i.e., the game is over.
    Nothing -> if boardIsFull game then Just Draw else Nothing
               where boardIsFull = (0 ==) . count Empty
      
    -- Can we find 3 identical (Mark Player) in a row?
    -- ?? What are some illustrative input/output examples for this function?
    where 
    findWinner :: Board -> Maybe Player     
    findWinner brd =
      -- searchSpace is all the 3-in-a-row possibilities.
      let searchSpace :: [Row] -- really [ [Mark, Mark, Mark], ... ]
          searchSpace = 
            diag brd : revDiag brd : brd ++ myTranspose brd
            
            where 
            -- ?? How does this function work?
            diag :: Board -> Row -- The major diagonal
            diag xss = [xss !! i !! i | i <- [0 .. (length xss - 1)]]

            -- transpose is a library function. It may be defined as follows.
            -- ?? How does this function work?
            myTranspose :: Board -> Board
            myTranspose xss = [fmap (!!i) xss | i <- [0 .. (length xss - 1)]]

            -- ?? How does this function work?
            revDiag :: Board -> Row -- The minor diagonal
            revDiag = diag . reverse

      in safeHead -- The (:: [Player]) annotation isn't necessary. 
                  -- It's for your information.

                  -- What is the preceding safeHead doing?

                  -- ?? Explain the notation x@(Mark p) and describe
                  -- the function it performs. 
           ([p | [x@(Mark p),y,z] <- searchSpace, x==y && y==z] :: [Player]) 
         where 
         safeHead :: [a] -> Maybe a
         safeHead []    = Nothing
         safeHead (x:_) = Just x

-- Find the minimaxValue and maxDepth of game using minimax. 
-- maxDepth is the longest possible game with this minimax outcome.

-- ?? Provide some input/output examples for this function and explain
-- how the function brings them about.
evaluate :: TTT -> TTT
evaluate game | count Empty game >= 8 = game { maxDepth = count Empty game,
                                               message  = []                 }
evaluate game = case currentStatus game of
    -- If the game is over, set the minimaxValue based on the outcome.
    -- This occurs at the terminal nodes of the game tree.
    -- ?? What does game mean on the following line? Explain how
    -- the expression below works.
    Just outcome -> game { minimaxValue = 
                             case outcome of Winner X -> 1
                                             Draw -> 0
                                             Winner O -> -1,
                           maxDepth = 0,
                           message  = []                        }
    Nothing      -> let minimaxGame = minimax game                             
                    in game { minimaxValue = minimaxValue minimaxGame, 
                              maxDepth     = 1 + maxDepth minimaxGame,
                              message      = []                        }

gameOver :: TTT -> Bool
gameOver = isJust . currentStatus 

ifTE :: Bool -> a -> a -> a
ifTE True  thenPart _ = thenPart
ifTE False _ elsePart = elsePart

makeMove :: TTT -> [String] -> TTT
makeMove game [w] 
  | w `elem` ["m", "minimax"]                                 = minimax game
  | w `elem` ["n", "new", "newGame", "start", "r", "restart", "y"] = newGame
makeMove game [[r], [c]] | isDigit r && isDigit c = play (read [r]) (read [c]) game
makeMove game moveWords = game { message = ["Unrecognized move: " ++ unwords moveWords] }

-- Find and return the best successor of game for the current player. 
-- Best means highest/lowest minimaxValue for 'X'/'O' and most remaining
-- moves among those with the same minimaxValue. 
-- Note: to compare games we compare (minimaxValue, maxDepth) pairs.

-- ?? Provide some illustrative input/output examples. Explain how
-- the function works, especially the lambda expression. What do the
-- various parts of that expression do?
minimax :: TTT -> TTT
minimax game = -- First line selects maximum or minimum
               -- Second line is all the successors evaluated by minimax.
               -- ?? Explain the following two lines.
               selectByPlayer (whoseMove game) maximum minimum
                  $ fmap evaluate successors
    where                                  
    -- Return a list of successor TTTs with all possible next moves. 
    successors :: [TTT]
    successors = [placeMark r c game | r <- rowColRange, 
                                       c <- rowColRange, 
                                       cellIsEmpty r c game]
    -- Here's an alternative definition using do notation.
    successors' = do r <- rowColRange
                     c <- rowColRange 
                     guard $ cellIsEmpty r c game
                     return $ placeMark r c game

-- Place the mark of the current player at position r, c. No evaluation.
placeMark :: Int -> Int -> TTT -> TTT
placeMark r c game = -- Replace an existing row with a new row.
    game { board = replace (r-1) -- The next line is the new row r 
                   (replace (c-1) (Mark (whoseMove game)) (board game !! (r-1)))
                   (board game)  }
    where 
    -- Replace the element at position i in items with item   

    -- ?? Explain how this function works. 
    replace :: Int -> a -> [a] -> [a]
    replace i item items = let (xs, _:ys) = splitAt i items 
                           in xs ++ item:ys

-- Place mark at position r, c and evaluate the result. 
play :: Int -> Int -> TTT -> TTT
play r c game
  | r `notElem` rowColRange    = game {message = ["Row out of range."]}
  | c `notElem` rowColRange    = game {message = ["Column out of range."]}
  | not $ cellIsEmpty r c game = game {message = ["That cell is taken."]}  
  | otherwise                  = evaluate $ placeMark r c game

-- Select among two choices depending on a Player. 
selectByPlayer :: Player -> a -> a -> a
selectByPlayer X theXChoice _          = theXChoice
selectByPlayer O _          theOChoice = theOChoice

-- ?? What are X and O in the code below?
whoseMove :: TTT -> Player
whoseMove game 
  | count (Mark X) game <= count (Mark O) game = X
  | otherwise                                  = O 


------------------------------------ I/O ---------------------------------------------------
cleanChar :: Char -> Char
cleanChar c | c `elem` ",()" = ' ' 
cleanChar c                  = toLower c

prompt :: TTT -> String              -- The $ in the next line depends on currying to make sense.
prompt game = ifTE (gameOver game) "Another game? (y or n) > " $ show (whoseMove game) ++ " to play (row col) > "

main' :: IO ()
main' = do                     
    moveList <- fmap (takeWhile (\ws -> ws /= [] && ws /= ["n"]) . map words . lines . map cleanChar) getContents 
    let states = scanl makeMove newGame moveList
    -- forM_ states (putStr . show)
    foldM (\_ state -> putStr $ show state ++ prompt state) () states 

main :: IO ()
main = playGame newGame
  where

  playGame :: TTT -> IO ()
  playGame game =  
    do putStr $ show game ++ prompt game
      -- See fmap at https://goo.gl/vtM9O
       moveWords <- fmap (words . fmap cleanChar ) getLine
       unless (stopGame moveWords) $
           playGame $ ifTE (gameOver game) newGame (makeMove game moveWords)
    where
    stopGame :: [String] -> Bool
    stopGame []    = True
    -- Answering: Another game? (y or n).   "n" means stop.
    stopGame (w:_) | gameOver game = w `elem` ["n", "no"] 
    stopGame (w:_) = w `elem` ["e", "end", "q", "quit", "s", "stop"]
------------------------------------ I/O ---------------------------------------------------
