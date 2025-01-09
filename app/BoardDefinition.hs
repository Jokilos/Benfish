module BoardDefinition where

type Position = (Int, Int)
data Piece = King | Queen | Rook | Bishop | Knight | Pawn | Empty deriving (Show, Eq)
data Color = White | Black deriving (Show, Eq)
type Square = Maybe (Color, Piece)
type Board = [[Square]]

initialRow :: Color -> [Square]
initialRow color = [   
        Just (color, Rook),
        Just (color, Knight),
        Just (color, Bishop),
        Just (color, Queen),
        Just (color, King),
        Just (color, Bishop),
        Just (color, Knight),
        Just (color, Rook)  ]

initialBoard :: Board
initialBoard = [ 
    initialRow White,
    replicate 8 (Just (White, Pawn)),
    replicate 8 Nothing,
    replicate 8 Nothing,
    replicate 8 Nothing,
    replicate 8 Nothing,
    replicate 8 (Just (Black, Pawn)),
    initialRow Black    ]
    
emptyBoard :: Board
emptyBoard = replicate 8 (replicate 8 Nothing) 