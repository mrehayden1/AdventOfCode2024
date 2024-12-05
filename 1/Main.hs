import Data.Char
import Data.List
import Data.Maybe
import qualified Data.IntMap as M
import System.Environment

type Input = ([Int], [Int])

main :: IO ()
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
part1 (ls, rs) =
  print . sum . zipWith ((abs .) . subtract) (sort ls) . sort $ rs

part2 :: Input -> IO ()
part2 (ls, rs) = do
  let occurences = foldl' (\m k -> M.insertWith (+) k 1 m) M.empty rs
  print . sum . fmap (\n -> n * (fromMaybe 0 . (occurences M.!?) $ n)) $ ls

parseInput :: String -> Input
parseInput = unzip . fmap split . lines
 where
  split s =
    let l = read . takeWhile (not . isSpace) $ s
        r = read . dropWhile isSpace . dropWhile (not . isSpace) $ s
    in (l, r)
