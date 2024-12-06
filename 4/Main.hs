import Data.List (isPrefixOf, tails, transpose)
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

part1 :: String -> IO ()
part1 input = do
  let rows = lines input
      cols = transpose rows
      diags = (diagonals . fmap reverse $ rows) ++ diagonals rows

  print . length
    . filter (liftA2 (||) (isPrefixOf "XMAS") (isPrefixOf "SAMX"))
    . concatMap (concatMap tails)
    $ [rows, cols, diags]

diagonals :: [[a]] -> [[a]]
diagonals xs =
  let xs' = flip' xs
  in gen xs ++ (tail . gen $ xs')
 where
  gen = transpose . zipWith drop [0..]

flip' :: [[a]] -> [[a]]
flip' = reverse . fmap reverse
