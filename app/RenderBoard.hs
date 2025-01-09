module RenderBoard where

import BoardDefinition(Position, Piece, Board)
import qualified BoardDefinition(Color(White, Black))
import Graphics.Gloss

-- Dimensions
squareSize :: Float
squareSize = 100

boardSize :: Int
boardSize = 8

windowSize :: Int
windowSize = round squareSize * boardSize

offset :: Float 
offset = fromIntegral boardSize * squareSize / 2 - squareSize / 2

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
        squareColor = if isDark then darkSquare else lightSquare
    in 
    color squareColor $ rectangleSolid squareSize squareSize

-- Render the board
renderBoardHelper :: Int -> Int -> Picture -> Picture
renderBoardHelper x y =
    let
        xf = (fromIntegral x * squareSize - offset) 
        yf = (fromIntegral y * squareSize - offset) 
    in
    translate xf yf $ 

renderBoard :: Picture
renderBoard =  
    pictures [renderBoardHelper x y | x <- [0..boardSize - 1], y <- [0..boardSize - 1]]

-- Render a piece (load images from disk)
renderPiece :: (BoardDefinition.Color, Piece) -> Position -> IO Picture
renderPiece (pieceColor, piece) (x, y) = do
    let filename = case pieceColor of
            BoardDefinition.White -> "white_" ++ show piece ++ ".bmp"
            BoardDefinition.Black -> "black_" ++ show piece ++ ".bmp"

    pieceImage <- loadBMP filename
    return $ translate (fromIntegral x * squareSize - offset) 
                     (fromIntegral y * squareSize - offset) pieceImage
    where offset = fromIntegral boardSize * squareSize / 2 - squareSize / 2

-- Render all pieces on the board
renderPieces :: Board -> IO Picture
renderPieces board = do
  let positions = [(x, y) | x <- [0..7], y <- [0..7]]
      squaresWithPieces = [(board !! y !! x, (x, y)) | (x, y) <- positions]
  pictures <$> mapM (\(square, pos) -> case square of
                          Nothing       -> return Blank
                          Just piece    -> renderPiece piece pos) squaresWithPieces


displayBoard :: IO ()
displayBoard = 
    display (InWindow "Chess Board" (windowSize, windowSize) (10, 10)) white renderBoard
