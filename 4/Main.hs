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
    . filter (liftA2 (||) (isPrefixOf "XMAS") (isPrefixOf "SAMX"))
    . concatMap (concatMap tails)
    $ [rows, cols, diags]

diagonals :: [[a]] -> [[a]]
diagonals rows = (diagonals' . fmap reverse $ rows) ++ diagonals' rows
 where
  diagonals' :: [[a]] -> [[a]]
  diagonals' xs =
    let xs' = rotate xs
    in gen xs ++ (tail . gen $ xs')
   where
    gen = transpose . zipWith drop [0..]

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
  cells M.! (i+1, j+1) == 'A'
    && ((cells M.! (i, j) == 'M' && cells M.! (i+2, j+2) == 'S')
         || (cells M.! (i, j) == 'S' && cells M.! (i+2, j+2) == 'M'))
    && ((cells M.! (i+2, j) == 'M' && cells M.! (i, j+2) == 'S')
         || (cells M.! (i+2, j) == 'S' && cells M.! (i, j+2) == 'M'))
