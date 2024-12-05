import Data.List
import System.Environment

type Report = [Int]
type Input = [Report]

main = do
  args <- getArgs
  if null args
    then
      putStrLn "Please provide an input filename."
    else do
      input <- fmap parseInput . readFile . head $ args
      part1 input
      part2 input

part1 :: Input -> IO ()
part1 = print . length . filter isSafe

isSafe :: Report -> Bool
isSafe report =
  let pairs = zip report . drop 1 $ report
  in (all (uncurry (<)) pairs || all (uncurry (>)) pairs)
       && all ((<= 3) . abs . uncurry subtract) pairs

part2 :: Input -> IO ()
part2 = print . length . filter isSafe2

isSafe2 report =
  isSafe report
    || (any isSafe . zipWith (++) (inits report) . fmap (drop 1) tails $ report)

parseInput :: String -> Input
parseInput = fmap (fmap read . words) . lines
