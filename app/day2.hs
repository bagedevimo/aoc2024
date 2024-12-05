import Data.List(sort)
import Data.Tuple
import Debug.Trace

main :: IO ()
main = do
  input <- readFile "input/day2.txt"
  putStrLn $ "Part 1: " ++ solvePart1 input
  putStrLn $ "Part 2: " ++ solvePart2 input

parse :: String -> [[Int]]
parse input = 
  map (map read) $ map words (lines input)

testLine :: [Int] -> Bool
testLine = all check . map calc . window

check :: (Ord a, Num a) => a -> Bool
check x = x >= 1 && x <= 3

calc :: Num a => (a, a) -> a
calc (a, b) = b - a

window :: [b] -> [(b, b)]
window list = (zip list (drop 1 list))

deleteAt idx list =
  case (splitAt idx list) of
    (left, (_: right)) -> left ++ right
    (left, [])        -> left

solvePart1 :: String -> String
solvePart1 input =
  show $ increasing + decreasing
    where
      increasing = test $ parse input
      decreasing = test $ map reverse $ parse input
      test = length . filter testLine

solvePart2 :: String -> String
solvePart2 input =
  show $ increasing + decreasing
    where
      increasing = length $ filter (==True) (alllines lines)
      decreasing = length $ filter (==True) (alllines (map reverse lines))
      alllines l = map (\line -> any testLine (line : (morelists line))) l
      lines = parse input
      morelists list = map (\idx -> deleteAt idx list) [0..(length list)]
