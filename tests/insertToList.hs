import Test.HUnit 
import Engine

test1 :: Test
test1 = TestCase (assertEqual "Insert Element" [1, 2] (insertToList 1 2 [1]))