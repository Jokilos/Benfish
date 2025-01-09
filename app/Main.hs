module Main where
import BoardDefinition
import RenderBoard 

main :: IO ()
main = do 
    putStrLn "Hello, Haskell!"
    displayBoard
