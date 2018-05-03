{-# LANGUAGE TemplateHaskell #-}
module Sudoku where
{-| Haskell Sudoku
    Author: Jim
    Modifier: Chris Tran|-}
import Data.Char (digitToInt, isDigit)
import Data.Maybe (fromJust, isJust, isNothing, listToMaybe)
import Data.List (transpose, group, sort, elemIndex, nub, findIndex)
import Data.List.Split (chunksOf)
import Control.Monad (liftM, replicateM_)
import Test.QuickCheck
import Data.Ix (inRange)

-------------------------------------------------------------------------

{-| A Sudoku puzzle is a list of lists, where each value is a Maybe Int. That is,
each value is either `Nothing' or `Just n', for some Int value `n'. |-}
data Puzzle = Puzzle [[Maybe Int]]
 deriving (Show, Eq)

{-| A Block is a list of 9 Maybe Int values. Each Block represents a row, a column,
or a square. |-}
type Block = [Maybe Int]

{-| A Pos is a zero-based (row, column) position within the puzzle. |-}
data Pos = Pos (Int, Int) deriving (Show, Eq)

{-| A getter for the rows in a Sudoku puzzle. |-}
rows :: Puzzle -> [[Maybe Int]]
rows (Puzzle rs) = rs

example :: Puzzle
example =
  Puzzle
    [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
    , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
    , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
    , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
    , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
    , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
    , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
    , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
    , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
    ]

{-| Ex 1.1

    A sudoku with just blanks. |-}
allBlankPuzzle :: Puzzle
allBlankPuzzle = Puzzle $ replicate 9 $ replicate 9 Nothing

{-| Ex 1.2

    Checks if sud is really a valid representation of a sudoku puzzle. |-}
isPuzzle :: Puzzle -> Bool
isPuzzle puzzle = let rs = rows puzzle
                  in
                   length rs == 9
                   && all ((== 9) . length) rs
                   && and (concatMap
                             (map (\ x -> 
                                    maybe (isNothing x) (inRange (1, 9)) x))
                             rs)

{-| Ex 1.3

    Checks if the puzzle is already solved, i.e. there are no blanks. |-}
isSolved :: Puzzle -> Bool
isSolved = and . concatMap (map isJust) . rows

{-| Ex 2.1

    `printPuzzle s' prints a representation of `s'. |-}
printPuzzle :: Puzzle -> IO ()
printPuzzle = putStrLn . unlines . map printRow . rows

--Converting a row into a String
printRow :: Show a => [Maybe a] -> String
printRow [] = "";
printRow (x:xs) | isNothing x = "." ++ printRow xs
                | otherwise = (show . fromJust) x ++ printRow xs

{-| Ex 2.2

    `readPuzzle f' reads from the FilePath `f', and either delivers it, or stops
    if `f' did not contain a puzzle. |-}
readPuzzle :: FilePath -> IO Puzzle
readPuzzle path = readFile path >>=
                    return .
                      (\ x ->
                        if (not . isPuzzle) x then error "Not a Sudoku puzzle!" else x)
                        . Puzzle . map strToList . lines

--Converting a String into list
strToList :: String -> [Maybe Int]
strToList [] = []
strToList (x:xs) | x == '.' = Nothing : strToList xs
                 | otherwise = Just (digitToInt x) : strToList xs

{-| Ex 3.1

    Check that a block contains no duplicate values. |-}
isValidBlock :: Block -> Bool
isValidBlock block = let block' = filter isJust block
                     in (length . nub) block' == length block'

{-| Ex 3.2

    Collect all blocks on a board - the rows, the columns and the squares. |-}
blocks :: Puzzle -> [Block]
blocks puzzle = rows puzzle ++ (transpose . rows) puzzle ++ squares puzzle

--Extracting all unique 3x3 squares in the Sudoku
squares :: Puzzle -> [Block]
squares = map concat .
           concatMap transpose . chunksOf 3 . map (chunksOf 3) . rows

{-| Ex 3.3

    Check that all blocks in a puzzle are legal. |-}
isValidPuzzle :: Puzzle -> Bool
isValidPuzzle = all isValidBlock . blocks

{-| Ex 4.1

    Given a Puzzle that has not yet been solved, returns a position in
    the Puzzle that is still blank. If there are more than one blank
    position, you may decide yourself which one to return. |-}
blank :: Puzzle -> Pos
blank puzzle = let xs = map (findIndex isNothing) (rows puzzle)
                   y = findIndex isJust xs
               in case y of
                    Just n -> Pos (n, fromJust ((!!) xs n))

{-| Ex 4.2

    Given a list, and a tuple containing an index in the list and a
    new value, updates the given list with the new value at the given
    index. |-}
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) [] _ = []
(!!=) arr (index, newValue) | (index >= length arr) || (index < 0) = arr
                            | otherwise = take index arr ++ [newValue] ++ drop (index + 1) arr

{-| Ex 4.3

    `update s p v' returns a puzzle which is a copy of `s' except that
    the position `p' is updated with the value `v'. |-}
update :: Puzzle -> Pos -> Maybe Int -> Puzzle
update puzzle (Pos (y, x)) newValue = let rs = rows puzzle
                                          newRow = (rs !! y) !!= (x, newValue)
                                      in Puzzle $ take y rs ++ [newRow] ++ drop (y + 1) rs

{-| Ex 5.1

    Solve the puzzle. |-}
solve :: Puzzle -> Maybe Puzzle
solve puzzle | (not . isValidPuzzle) puzzle = Nothing
             | isSolved puzzle = Just puzzle
             | otherwise = pickASolution possibleSolutions
  where
    nineUpdatedSuds = [update puzzle (blank puzzle) (Just newValue) | newValue <- [1..9]] :: [Puzzle]
    possibleSolutions = [sol | puzzle' <- nineUpdatedSuds, let sol = solve puzzle', isJust sol]

pickASolution :: [Maybe Puzzle] -> Maybe Puzzle
pickASolution [] = Nothing
pickASolution solutions = head solutions

{-| Ex 5.2

    Read a puzzle and solve it. |-}
readAndSolve :: FilePath -> IO ()
readAndSolve path = readPuzzle path >>= maybe (print "(no solution)") printPuzzle . solve

{-| Ex 5.3

    Checks if s1 is a solution of s2. |-}
--All blocks are OK, no blank
--Digit in puzzle remain the same as puzzle
isSolutionOf :: Puzzle -> Puzzle -> Bool
isSolutionOf sol pzl = isValidPuzzle sol
                       && isSolved sol
                       && all (\(x, y) -> isNothing y || (x == y))
                            (zip ((concat . rows) sol) ((concat . rows) pzl))

-------------------------------------------------------------------------
-- QuickCheck tests:
--
-- Run these in ghci, as
--
-- Sudoku> quickCheck prop_myProp
--
-- or
--
-- Sudoku> runTests
--
-- But note that some tests, prop_solve in particular, may take a long time to run.
-- You can run a test with fewer cases by running
--
-- > fewerCheck prop_solve
--
-- or optimise your solver so that it runs faster!
--
-- Feel free to add your own tests.
-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Puzzle
cell :: Gen (Maybe Int)
cell = frequency [ (9, return Nothing)
                 , (1, do n <- choose(1,9) ; return (Just n))]

-- an instance for generating Arbitrary Puzzles
instance Arbitrary Puzzle where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Puzzle rows)

instance Arbitrary Pos where
  arbitrary = do r <- choose (0,8)
                 c <- choose (0,8)
                 return $ Pos (r,c)

prop_allBlank :: Bool
prop_allBlank = let rs = rows allBlankPuzzle
                in
                 length rs == 9
                 && and (map ((==9) . length) rs)
                 && and ((concatMap (map isNothing)) rs)

prop_isPuzzle :: Puzzle -> Bool
prop_isPuzzle s = isPuzzle s

prop_isNotPuzzle :: Bool
prop_isNotPuzzle = not $ isPuzzle (Puzzle [[]])

prop_blocks :: Puzzle -> Bool
prop_blocks s = ((length bl) == 3*9) &&
                and [(length b) == 9 | b <- bl]
  where bl = blocks s

prop_isValidPuzzle :: Puzzle -> Bool
prop_isValidPuzzle s = isValidPuzzle s || not (null bads)
  where bads = filter (not . isValidBlock) (blocks s)

prop_blank :: Puzzle -> Bool
prop_blank s = let rs        = rows s
                   Pos (x,y) = blank s
               in isNothing ((rs !! x) !! y)
                
prop_listReplaceOp :: [a] -> (Int, a) -> Bool
prop_listReplaceOp s (i,x) = length s == length (s !!= (i, x))

prop_update :: Puzzle -> Pos -> Maybe Int -> Bool
prop_update s p m = let Pos (r,c) = p
                        s' = update s p m
                        rs = rows s'
                    in
                     (rs !! r) !! c == m

-- run with fewerCheck if you
-- do not like to wait...
prop_solve :: Puzzle -> Bool
prop_solve s 
    | solution == Nothing = True
    | otherwise           = isSolutionOf (fromJust solution) s
  where solution = solve s

fewerCheck prop = quickCheckWith (stdArgs{ maxSuccess = 30 })  prop


{-- Template Haskell Magic, ignore this for now! --}
return []
runTests = $quickCheckAll
