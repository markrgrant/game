-- TODO : Game should also be an instance of State Monad
-- TODO : ghosts should move on their own schedule, separate from
--        player moves
-- TODO : support multiplayer
import Data.List (intercalate)
import Data.Char (toLower)
import System.Random
import System.IO (hFlush, stdout, hSetBuffering, stdin,
                  BufferMode(NoBuffering))
import System.Exit (exitSuccess)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State (State, StateT, put, get, runStateT,
                                  evalStateT)


data Game = Game StdGen [Piece] Lives
type Lives = Int
data Piece = Piece PieceType Int Int
data PieceType = Ghost | Player
type Pieces = [Piece]
data Move = U | D | L | R | Q


boardWidth = 10
boardHeight = 5


showGame :: Game -> String
showGame (Game _ pieces lives) = showBoard pieces ++ showLives lives


showLives :: Int -> String
showLives x = "lives = " ++ show x


showBoard :: [Piece] -> String
showBoard pieces = 
    [0..boardHeight-1] >>= \y ->
    [0..boardWidth] >>= \x ->
    showPosition pieces x y


showPosition pieces x y
  | x == boardWidth = "\n"
  | null piecesAt = "."
  | otherwise = case head piecesAt of
                    (Piece Ghost _ _) -> "G"
                    _ -> "P"
  where piecesAt = filter (\(Piece _ x' y') -> x==x' && y==y') pieces


updateGame :: Char -> Game -> Game
updateGame cmd game
  | cmd == 'l' = updateLives $ movePieces game R
  | cmd == 'j' = updateLives $ movePieces game L
  | cmd == 'i' = updateLives $ movePieces game U
  | cmd == 'k' = updateLives $ movePieces game D
  | otherwise  = game


-- if a player and a ghost share a position, reduce the lives by 1.
updateLives :: Game -> Game
updateLives game@(Game g pieces lives) = 
  if null ghostsAt
    then Game g pieces lives
    else Game g pieces (lives-1)
  where (Piece Player x y) = head pieces
        ghostsAt = filter (\(Piece Ghost x' y') -> x==x' && y==y') (tail pieces)


movePieces :: Game -> Move -> Game
movePieces (Game g pieces lives) mv = Game g' pieces' lives
  where (pieces', g') = movePieces' (pieces, g) [] mv


movePieces' :: ([Piece], StdGen) -> [Piece] -> Move -> ([Piece], StdGen)
movePieces' ([], g) xs mv = (reverse xs, g)
movePieces' (p:ps, g) xs mv = movePieces' (ps, g') (p':xs) mv
  where (p', g') = movePiece p g mv 


movePiece :: Piece -> StdGen -> Move -> (Piece, StdGen)
movePiece p@(Piece Player _ _) g mv = movePiece' p g mv
movePiece p@(Piece Ghost _ _) g mv = movePiece' p g' mv'
  where (mv', g') = randomDir g


movePiece' (Piece t x y) g L = (Piece t ((x - 1) `mod` boardWidth) y, g)
movePiece' (Piece t x y) g R = (Piece t ((x + 1) `mod` boardWidth) y, g)
movePiece' (Piece t x y) g U = (Piece t x ((y - 1) `mod` boardHeight), g)
movePiece' (Piece t x y) g D = (Piece t x ((y + 1) `mod` boardHeight), g)


randomDir :: StdGen -> (Move, StdGen)
randomDir g = (intToMove mv, g') where
  (mv, g') = randomR (0, 3) g


intToMove :: Int -> Move
intToMove i
  | i == 0 = L
  | i == 1 = R
  | i == 2 = U
  | i == 3 = D


initialState :: Game
initialState = Game  (mkStdGen 0) pieces 3
  where pieces = [Piece Player (boardWidth `div` 2) (boardHeight `div` 2),
                  Piece Ghost 0 0,
                  Piece Ghost (boardWidth-1) (boardHeight-1),
                  Piece Ghost 0 (boardHeight-1),
                  Piece Ghost (boardWidth-1) 0]


getInput :: IO Char
getInput = hSetBuffering stdin NoBuffering >> fmap toLower getChar


printRepl :: Game -> IO Char
printRepl game = putStrLn (showGame game) >>
  hFlush stdout >>
  getInput


checkExit :: Game -> IO ()
checkExit (Game _ _ lives) 
  | lives == 0 = putStrLn "You have no more lives left. Exiting." >> exitSuccess
  | otherwise = return ()


run :: StateT Game IO ()
run = get >>=
      liftIO . printRepl >>= \inp ->
      get >>=
      liftIO . checkExit >> 
      get >>=
      put . updateGame inp >>
      run


main :: IO ()
main = evalStateT run initialState
