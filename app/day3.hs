import Data.List(subsequences)
import Data.Maybe(mapMaybe)
import Debug.Trace

main :: IO ()
main = do
  input <- readFile "input/day3.txt"
  putStrLn $ "Part 1: " ++ (show $ solvePart1 input)
  putStrLn $ "Part 2: " ++ (show $ solvePart2 input)

-- xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))

data Instruction = Mul Int | Enable | Disable deriving Show


window :: String -> [String]
window start
  | length start < 8 = []
  | otherwise         = take 12 start : window (drop 1 start)


solvePart1 :: String -> Int
solvePart1 str = foldr execute 0 (instructions str) where
  execute instr acc =
    acc + (unwrap instr)
  unwrap instr =
    case instr of
      Mul a -> a
      _     -> 0

solvePart2 :: String -> Int
solvePart2 str = snd (foldr execute (True, 0) (traceShow (instructions str) (reverse $ instructions str))) where
  execute instr (enabled, acc) =
    case (traceShow (enabled, instr) (enabled, instr)) of
      (True, Mul a) -> (True, acc + a)
      (_, Enable)   -> (True, acc)
      (_, Disable)  -> (False, acc)
      (False, _ )   -> (False, acc)

instructions :: String -> [Instruction]
instructions str = mapMaybe parseInstruction (window str)

parseInstruction :: String -> Maybe Instruction
parseInstruction str =
  case str of
    'd' : 'o' : '(' : ')' : _ -> Just Enable
    'd' : 'o' : 'n' : '\'' : 't' : '(' : ')' : _ -> Just Disable
    'm' : 'u' : 'l' : '(' : a1 : ',' : b1 : ')' : _ -> Just $ Mul $ (read [a1] :: Int) * (read [b1] :: Int)
    'm' : 'u' : 'l' : '(' : a1 : ',' : b1 : b2 : ')' : _ -> Just $ Mul $ (read [a1] :: Int) * (read [b1, b2] :: Int)
    'm' : 'u' : 'l' : '(' : a1 : ',' : b1 : b2 : b3 : ')' : _ -> Just $ Mul $ (read [a1] :: Int) * (read [b1, b2, b3] :: Int)
    'm' : 'u' : 'l' : '(' : a1 : a2 : ',' : b1 : ')' : _ -> Just $ Mul $ (read [a1, a2] :: Int) * (read [b1] :: Int)
    'm' : 'u' : 'l' : '(' : a1 : a2 : ',' : b1 : b2 : ')' : _ -> Just $ Mul $ (read [a1, a2] :: Int) * (read [b1, b2] :: Int)
    'm' : 'u' : 'l' : '(' : a1 : a2 : ',' : b1 : b2 : b3 : ')' : _ -> Just $ Mul $ (read [a1, a2] :: Int) * (read [b1, b2, b3] :: Int)
    'm' : 'u' : 'l' : '(' : a1 : a2 : a3 : ',' : b1 : ')' : _ -> Just $ Mul $ (read [a1, a2, a3] :: Int) * (read [b1] :: Int)
    'm' : 'u' : 'l' : '(' : a1 : a2 : a3 : ',' : b1 : b2 : ')' : _ -> Just $ Mul $ (read [a1, a2, a3] :: Int) * (read [b1, b2] :: Int)
    'm' : 'u' : 'l' : '(' : a1 : a2 : a3 : ',' : b1 : b2 : b3 : ')' : _ -> Just $ Mul $ (read [a1, a2, a3] :: Int) * (read [b1, b2, b3] :: Int)
    _     -> Nothing
