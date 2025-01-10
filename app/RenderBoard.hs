module RenderBoard where

import BoardDefinition(Position, Piece, Board, initialBoard)
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
renderBoardHelper :: Float -> Int -> Int -> Picture -> Picture
renderBoardHelper scl x y pic =
    let
        xf = (fromIntegral x * squareSize - offset) 
        yf = (fromIntegral y * squareSize - offset)
        scaledPicture = scale scl scl pic
    in
    translate xf yf scaledPicture 

renderBoard :: Picture
renderBoard = let
        rs x y = renderSquare (x, y)
    in
    pictures [renderBoardHelper 1.0 x y (rs x y) | x <- [0..boardSize - 1], y <- [0..boardSize - 1]]

-- Render a piece (load images from disk)
renderPiece :: (BoardDefinition.Color, Piece) -> Position -> IO Picture
renderPiece (pieceColor, piece) (x, y) = do
    let filename = case pieceColor of
            BoardDefinition.White -> "assets/white_" ++ show piece ++ ".bmp"
            BoardDefinition.Black -> "assets/black_" ++ show piece ++ ".bmp"

    pieceImage <- loadBMP filename
    return $ renderBoardHelper 2.0 x y pieceImage

-- Render all pieces on the board
renderPieces :: Board -> IO Picture
renderPieces board = do
    let positions = [(x, y) | x <- [0..7], y <- [0..7]]
        squaresWithPieces = [(board !! y !! x, (x, y)) | (x, y) <- positions]

    pictures <$> mapM (\(square, pos) -> case square of
        Nothing -> return Blank
        Just piece -> renderPiece piece pos) squaresWithPieces

displayBoard :: Board -> IO ()
displayBoard board = do
    pieces <- renderPieces board 
    display (InWindow "Chess Board" (windowSize, windowSize) (10, 10)) white (pictures [renderBoard, pieces])



