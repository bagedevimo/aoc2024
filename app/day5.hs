import Data.List.Split
import Debug.Trace

main :: IO ()
main = do
  input <- (readFile "input/day5.txt")
  putStrLn $ show $ solvePart1 $ lines input

data Rule = (Int, Int)
data Update = [Int]

parse :: String -> ([Rule], [Update])
parse lines =

    (rules : (updates: _)) = splitOn [""] lines

solvePart1 :: [String] -> Int
solvePart1 lines =
