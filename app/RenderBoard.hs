module RenderBoard where

import Graphics.Gloss

-- Dimensions
squareSize :: Float
squareSize = 60

boardSize :: Int
boardSize = 8

-- Colors
lightSquare :: Color
lightSquare = makeColorI 240 217 181 255

darkSquare :: Color
darkSquare = makeColorI 181 136 99 255

-- Render a square
renderSquare :: Position -> Bool -> Picture
renderSquare (x, y) isDark = 
    let color = if isDark then darkSquare else lightSquare
    in color $ rectangleSolid squareSize squareSize

-- Render the board
-- renderBoard :: Integer -> Integer ->
renderBoard :: Picture
renderBoard = 
    pictures [ translate (fromIntegral x * squareSize - offset) 
                        (fromIntegral y * squareSize - offset)
                $ renderSquare (x, y) ((x + y) `mod` 2 == 1)
            | x <- [0..boardSize - 1], y <- [0..boardSize - 1]
            ]
    where offset = fromIntegral boardSize * squareSize / 2 - squareSize / 2

display_board:: IO ()
display_board = 
    display (InWindow "Chess Board" (600, 600) (10, 10)) white renderBoard
