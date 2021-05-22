import System.IO hiding(getLine,putStr,(!!))
import Prelude hiding (getLine,putStr,(!!))
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fol
import qualified Data.List as List
import Data.Maybe
getLine :: IO String
getLine = do x <- getChar
             if x == '\n' then
                return []
             else
                do xs <- getLine
                   return (x:xs)

putStr :: String -> IO()
putStr []     = return ()
putStr (x:xs) =  do putChar x 
                    putStr xs 

---------------------------------------------------------
--- Implementation---of---Nim----------------------------

data Player = One | Two deriving (Show, Eq)

change :: Player -> Player
change One = Two
change Two = One


type Board = Seq.Seq Int


initBoard :: Board
initBoard = Seq.fromList [5, 4, 3, 2, 1]


move :: Board -> (Int, Int) -> Maybe Board
move board (row, stars)
  | and [(Seq.index board row) >= stars,
          row < 5] = Just (Seq.adjust (\x -> x - stars) row board)
  | otherwise = Nothing


display :: Board -> String
display board = List.intercalate "\n" (zipWith (++) numbers (stars board))
                where numbers = ["1. ", "2. ", "3. ", "4. ", "5. "]
                      stars board = [(concat . take n) (repeat "* ")
                                    | n <- Fol.toList board]




nim :: IO ()
nim = do putStrLn "Welcome to nim!"
         putStrLn "Initializing ..."
         putStrLn (display initBoard)
         turn initBoard One


turn :: Board -> Player -> IO ()
turn board player = do putStrLn ("\nPlayer " ++ (show player) ++ " is playing!")
                       putStrLn "Row?"
                       row <- getLine
                       putStrLn "How many stars?"
                       stars <- getLine
                       let newBoard = move board ((read row) - 1, read stars)
                       if newBoard == Nothing
                         then do putStrLn "Not valid movement"
                                 turn board player
                         else result (fromJust newBoard) (change player)


result :: Board -> Player -> IO()
result board player = do if board == Seq.fromList [0, 0, 0, 0, 0]
                           then putStrLn ("Player " ++ (show player)
                                         ++ "wins!")
                           else do putStrLn ""
                                   putStrLn (display board)
                                   turn board player