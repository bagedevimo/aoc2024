import Data.List(transpose)
import Data.Maybe(mapMaybe)
import Debug.Trace

main :: IO ()
main = do
  input <- (readFile "input/day4.txt")
  putStrLn $ show (solvePart1 (lines input))

diagonals s = (transpose $ map reverse as) ++ (transpose bs)
  where
    (as,bs) = unzip $ zipWith (flip $ splitAt) s [0..]

window :: [Char] -> [[Char]]
window start
  | length start < 3 = []
  | otherwise         = take 4 start : window (drop 1 start)

solvePart1 :: [[Char]] -> Int
solvePart1 grid = sum [leftRight, topBottom, topLeftBottomRight, topRightBottomLeft] where
  leftRight          = sum $ map (\line -> length $ filter isXmas (window (line))) grid
  topBottom          = sum $ map (\line -> length $ filter isXmas (window ((line)))) (transpose grid)
  topLeftBottomRight = sum $ map (\line -> length $ filter isXmas (window (line))) (diagonals grid)
  topRightBottomLeft = sum $ map (\line -> length $ filter isXmas (window ((line)))) (diagonals (reverse grid))

isXmas :: String -> Bool
isXmas s =
  case (s) of
    'X' : 'M' : 'A' : 'S' : _ -> True
    'S' : 'A' : 'M' : 'X' : _ -> True
    _ -> False
