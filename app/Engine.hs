module Engine where 

import BoardDefinition
import Data.Map (Map)
import Test.HUnit

insertToList :: Int -> a -> [a] -> [a]
insertToList 0 el tab = el : tab
insertToList n el tab = insertToList (n-1) el tab 

-- insertSquare :: Board -> Piece -> Position -> Board
-- insetSquare table piece (x, y) board =  
-- -- toBoard = 

basicSum x y = x + y

type TrueBoard = Map Position Piece
type Move = (Position, Position)
type Situation = (Board, Color)

-- pieceMoves  :: Square -> Position -> Board -> [Move]
-- pieceMoves Just(color, Pawn) (x,y) board = 

-- legalMoves :: Situation -> [Move]



