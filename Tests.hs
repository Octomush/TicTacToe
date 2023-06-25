module Tests (Tests.main) where
import IC.TestSuite
import TicTacToe hiding (main)

--testBoard4 reflects the case where the game has tied 
--testBoard5 reflects the initial board
gameOverTestCases
   = [ testBoard1 ==> True
     , testBoard2 ==> False
     , testBoard3 ==> True
     , testBoard4 ==> False
     , testBoard5 ==> False
     ]

-- test cases for gameTie
gameTieTestCases 
   = [ testBoard1 ==> False
     , testBoard2 ==> False
     , testBoard3 ==> False
     , testBoard4 ==> True
     , testBoard5 ==> False
    ]

-- edge case where the first number is quoted
parsePositionTestCases
   = [
       ("0 2") ==> (Just (0,2))
     , ("0 -8") ==> (Just (0,-8))
     , ("-4 1") ==> (Just (-4,1))
     , ("0 %1") ==> (Nothing)
     , ("") ==> (Nothing)
     , ("1 2 3") ==> (Nothing)
     , ("\'2\' 4") ==> (Nothing)
     ]

--edge case where position exceeded the board size
tryMoveTestCases
  = [
      (X,(0,0),testBoard2) ==> (Nothing)
    , (O,(-1,2),testBoard2) ==> (Nothing)
    , (O,(0,-1),testBoard2) ==> (Nothing)
    , (O,(1,1),testBoard2) ==> (Just ([Taken X,Empty,Empty,Taken O],2))
    , (O,(3,3),testBoard1) ==> (Just ([Taken O,Taken X,Empty,Taken O,Taken O,
                                Empty,Taken X,Taken X,Taken O,Empty,Empty,Taken
                                X,Taken O,Taken X,Empty,Taken O],4))
    , (X, (4, 4), testBoard5) ==> (Nothing)
    ]

-- You can add your own test cases above

allTestCases
  = [
      TestCase "gameOver" (gameOver)
               gameOverTestCases
    , TestCase "gameTie" (gameTie)
               gameTieTestCases
    , TestCase "parsePosition" (parsePosition)
               parsePositionTestCases
    , TestCase "tryMove" (uncurry3 tryMove)
               tryMoveTestCases
    ]


runTests = mapM_ goTest allTestCases

main = runTests
