import System.IO
import Data.List.Split
import Data.List
import Control.Monad
import System.Random

-- move zero in position x if possible in list (row version)
moveRow:: (Eq a , Num a, Ord a) => Int -> [a] -> [a]
moveRow x m
 | 0 `elem` m   = take x (filter (>0) m) ++ [0] ++ drop x (filter (>0) m)
 | otherwise    = m

-- take x,y :: Int and return new matrix with move if possible 
moveXY:: (Eq a , Num a, Ord a) => Int -> Int -> [[a]] -> [[a]]
moveXY x y m 
 | x > (length m) - 1                   = m
 | y > (length (m!!0)) - 1              = m
 | elem 0 (m!!x) == True                = map (moveRow y) m
 | elem 0 ((transpose m)!!y) == True    = transpose $ map ( moveRow x) (transpose m)
 | otherwise =  m

-- Generator of random number between (lo,hi)
genRandomNumbersBetween :: Int -> Int -> (Int, Int) -> [Int]
genRandomNumbersBetween n seed (a, b) = take n $ (randomRs (a, b) myGenerator) where
    myGenerator = mkStdGen seed

-- Check if the game is resolved (solution exercise 3)
resolved :: (Num a, Eq a, Ord a ) => [a]-> (Bool)
resolved a = snd $ foldl (\ (i,res) x -> if i == x then (i+1,True && res) else (i+1,False && res) ) (1,True) (init a)

-- Check if all numbers have casual position (c.t. original board 1..15) 
allCasual::(Num a, Eq a, Ord a) => [a]-> (Bool)
allCasual a = snd $ foldl (\ (i,res) x -> if i /= x then (i+1,True && res) else (i+1,False && res) ) (1,True) (filter (>0) a)

-- Recursive shuffle until allCasual (solution exercise 5)
randUntilShuffled :: (Num a, Ord a) => [[a]] -> Int -> [[a]]
randUntilShuffled m x
 | allCasual (concat m) == True     = m
 | otherwise                        = randUntilShuffled (moveXY (head $ genRandomNumbersBetween 1 (x+3) (0, max ((length $ m!!0) -1) (length m - 1) )) (head $ genRandomNumbersBetween 1 (x+5) (0, max ((length $ m!!0) -1) (length m - 1) )) m) (x+2)



main = do
    let seed = 0
    print "Do you want to load gameBoard? (press y for yes else no)"
    l <- getLine
    if l == "y" then do
        print "--PLAYING GAME--"
        content <- readFile "prova.txt"
        let mat = map (map (read :: String -> Int)) (map words (lines content))
        mapM_ print mat
        looping mat
    else do 
        print "How many rows do you want in the table?"
        in1 <- getLine
        print "How many cols do you want in the table?"
        in2 <- getLine
        let w = read in1
        let h = read in2
        let initMat = chunksOf h ([1..(w*h-1)]++[0])
        mapM_ print initMat
        
        --Section ex4 ex5
        print "Do you want random move or shuffle? (r :random, otherwise shuffle)"
        c <- getLine
        when ((head c) == 'r' ) $ do 
            print "How many random move do you want ? "
            in6 <- getLine
            let n = read in6
            let randMoves = chunksOf 2 (genRandomNumbersBetween (n*2) seed (0, max w h) :: [Int])
            print randMoves
            -- Solution exercise 4
            let newM = foldl(\acc x -> moveXY (head x) (last x) acc) initMat randMoves
            mapM_ print newM
            print "--PLAYING GAME--"
            looping newM
        -- Shuffle game    
        print "Wait a moment... I'm shuffling the game!!" 
        -- Solution exercise 4
        let mat1 = randUntilShuffled initMat 1  --shuffle
        mapM_ print mat1
        print "--PLAYING GAME--"
        looping mat1 -- play the game with shuffled matrix
    


        -- setup game board (Solution exercise 5)
        --let mat1 = randUntilShuffled initMat 1  --shuffle
        --mapM_ print mat1
        --looping mat1 -- play the game with shuffled matrix
        




looping mat = do 
    print "Choose row:"
    in3 <- getLine
    print "Choose column:"
    in4 <- getLine
    let x = read in3
    let y = read in4
    let newM = moveXY x y mat
    mapM_ print newM
    if ((resolved $ (concat $ newM)) == True) then do
        print $ "You WIN!!!"
    else do
         -- writing matrix on file
        let a = map (map show ) newM
        let b = map unwords a
        let c = unlines b
        writeFile "prova.txt" c   
        print "Saving your status in _ prova.txt _"
    looping newM



