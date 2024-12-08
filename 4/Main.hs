import Data.List (isPrefixOf, tails, transpose)
import Data.Map (Map)
import qualified Data.Map as M
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  if null args
    then
      putStrLn "Usage: ./day4 [input-file]"
    else do
      input <- readFile . head $ args
      part1 input
      part2 input

part1 :: String -> IO ()
part1 input = do
  let rows = lines input
      cols = transpose rows
      diags = diagonals rows

  print . length
    . concatMap (concatMap (filter matchesXmas . tails))
    $ [rows, cols, diags]

 where
  matchesXmas :: String -> Bool
  matchesXmas = liftA2 (||) (isPrefixOf "XMAS") (isPrefixOf "SAMX")

diagonals :: [[a]] -> [[a]]
diagonals = (++) <$> diagonals' . fmap reverse <*> diagonals'
 where
  diagonals' :: [[a]] -> [[a]]
  diagonals' = (++)
    <$> transpose . zipWith drop [0..]
    <*> tail . transpose . zipWith drop [0..] . rotate

   where
    rotate :: [[a]] -> [[a]]
    rotate = reverse . fmap reverse

part2 :: String -> IO ()
part2 input = do
  let rows = lines input
      n = length rows
      cells = M.fromList
        . concat . zipWith (\j -> zipWith ((,) . (, j)) [0..]) [0..]
        $ rows :: Map (Int, Int) Char
  print . length
    . filter (xMasAt cells)
    $ [ (i, j) | j <- [0..(n-3)], i <- [0..(n-3)] ]
  return ()

xMasAt :: Map (Int, Int) Char -> (Int, Int) -> Bool
xMasAt cells (i, j) =
  let c  = cells M.! (i+1, j+1)
      nw = cells M.! (i, j)
      se = cells M.! (i+2, j+2)
      ne = cells M.! (i+2, j)
      sw = cells M.! (i, j+2)
  in c == 'A' && elem [nw, se] ["MS", "SM"] && elem [ne, sw] ["MS", "SM"]
