import Test.HUnit 
import Engine
test1 :: Test
test1 :: TestCase (assertEqual "Insert Element" [Int 1,Int 2] (insertToList (Int 1) (Int 2) [Int 1]))