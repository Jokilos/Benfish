module RenderBoard where

import BoardDefinition(Position)
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
renderSquare :: Position -> Picture
renderSquare (x, y) = 
    let 
        isDark = (x + y) `mod` 2 == 1
        color = if isDark then darkSquare else lightSquare
    in 
    -- color $  
    rectangleSolid squareSize squareSize

-- Render the board
renderBoardHelper :: Int -> Int-> Picture
renderBoardHelper x y =
    let
        offset = fromIntegral boardSize * squareSize / 2 - squareSize / 2
        xf = (fromIntegral x * squareSize - offset) 
        yf = (fromIntegral y * squareSize - offset) 
    in
    translate xf yf $ renderSquare (x, y) 


renderBoard :: Picture
renderBoard = 
    pictures [renderBoardHelper x y | x <- [0..boardSize - 1], y <- [0..boardSize - 1]]

displayBoard :: IO ()
displayBoard = 
    display (InWindow "Chess Board" (600, 600) (10, 10)) white renderBoard
