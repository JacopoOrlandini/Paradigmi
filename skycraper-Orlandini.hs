import System.IO
import Data.List
import Control.Monad

-- Get number of roofTop
getSkr::(Num a,Ord a) => [a] -> a
getSkr skyscraper = fst $ foldl(\ (acc,maxScr) skr -> if skr > maxScr then (acc+1,skr)  else (acc,maxScr)) (0,0) skyscraper

-- Unique value on list
unique :: (Eq a) => [a] -> Bool
unique []     = True
unique (x:xs) = x `notElem` xs && unique xs

-- Check unique value and range
checkRow :: [Int] -> Bool
checkRow l = if sum l == sum [1..(length l)] then True && (unique l) else False

--MAIN FUNCTION
main = do
  content <- readFile "game4x4.txt"
  let mat = map (map (read :: String -> Int)) (map words (lines content)) -- read full matrix from file
  let gameMat = tail $ init $ mat   -- matrix without first and last row

  -- Row process
  rowRes <- forM [0 .. (length gameMat -1 )](\ a -> do
    return ((compare (getSkr $ tail $ init $ gameMat!!a) (head $ gameMat!!a)), (compare (getSkr (reverse $ tail $ init $ gameMat!!a)) (last $ gameMat!!a)))
    )

  -- Column process (transposed row)
  colRes <- forM [0 .. (length gameMat -1 )](\a -> do
    return ((compare (getSkr $ tail $ init $ gameMat!!a) (head $ gameMat!!a)), (compare (getSkr (reverse $ tail $ init $ gameMat!!a)) (last $ gameMat!!a)))
    )
  mapM_ print mat
  print $ "(constrain row left, constrain row rigth) "
  print rowRes
  print $ "(constrain col left, constrain col rigth) "
  print colRes
  print $ "Check unique and range on list "
  print $ map checkRow gameMat
