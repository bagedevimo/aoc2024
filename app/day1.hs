import Data.List(sort)

main :: IO ()
main = do
  input <- readFile "input/day1.txt"
  putStrLn $ "Part 1: " ++ solvePart1 input
  putStrLn $ "Part 2: " ++ solvePart2 input

reduceLine :: String -> ([Int], [Int]) -> ([Int], [Int])
reduceLine curr (left, right) = 
  case (words curr) of
    (a:b:_) -> (left ++ [read a :: Int], right ++ [read b :: Int])
    _     -> (left, right)

buildLists :: String -> ([Int], [Int])
buildLists input =
  foldr reduceLine ([], []) $ lines input

solvePart1 :: String -> String
solvePart1 input =
  let
    (left, right) = buildLists input
    sortL = sort left :: [Int]
    sortR = sort right :: [Int]
  in
    show $ foldl (+) 0 $ zipWith (\a b -> abs (a - b)) sortL sortR

solvePart2 :: String -> String
solvePart2 input =
  let
    (left, right) = buildLists input
  in
    show $ foldr (\k accl -> accl + k * (length (filter (== k) right))) 0 left
