import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Maybe
import System.Environment

type Input = String

newtype Parser s a = Parser {
  unParser :: MaybeT (State (s, Input)) a
} deriving (Applicative, Alternative, Functor, Monad, MonadFail)

type InstructionParser = Parser Bool

instance MonadState s (Parser s) where
  get   = Parser . lift . gets $ fst
  put s = Parser . lift . modify $ \(_, i) -> (s, i)

setInput :: Input -> Parser s ()
setInput i = Parser . lift $ modify $ \(s, _) -> (s, i)

getInput :: Parser s Input
getInput = Parser . lift . gets $ snd

main :: IO ()
main = do
  args <- getArgs
  if null args
    then
      putStrLn "Please provide an input filename."
    else do
      input <- readFile . head $ args
      print . flip evalState (True, input) . runMaybeT . unParser
        $ readInstructions

readInstructions :: InstructionParser Int
readInstructions =
  0 <$ eof <|> do
    try readDo <|> return ()
    readDont <|> return ()
    n <- readMul <|> 0 <$ anyChar
    ns <- readInstructions
    return $ n + ns

readDo :: InstructionParser ()
readDo = string "do()" >> put True

readDont :: InstructionParser ()
readDont = string "don't()" >> put False

readMul :: InstructionParser Int
readMul = do
  string_ "mul("
  (x :: Int) <- fmap read . many . oneOf $ ['0'..'9']
  char_ ','
  (y :: Int) <- fmap read . many . oneOf $ ['0'..'9']
  char_ ')'
  s <- get
  return $ if s then x * y else 0

{-
 - Parser combinators
 -}

try :: Parser s a -> Parser s a
try p = do
  -- Save the unconsumed input for if `p` fails.
  input <- getInput
  -- TODO Repeat any failure messages from `p`. It doesn't matter currently as
  -- we're using `Maybe` to handle failure.
  p <|> (setInput input >> fail "")

eof :: Parser s ()
eof = do
  input <- getInput
  if null input
    then return ()
    else fail "Unmatched end of file."

anyChar :: Parser s ()
anyChar = do
  input <- getInput
  case input of
    []       -> fail "Expected any char got end of file."
    (_ : is) -> setInput is

char :: Char -> Parser s Char
char c = do
  (i : is) <- getInput
  if i == c
    then do
      setInput is
      return c
    else
      fail $ "Unmatched character '" ++ (c : "") ++ "'."

char_ :: Char -> Parser s ()
char_ = void . char

oneOf :: [Char] -> Parser s Char
oneOf chars = oneOf' chars
 where
  oneOf' :: [Char] -> Parser s Char
  oneOf' []       = fail $ "Failed to match characters in \"" ++ chars ++ "\""
  oneOf' (c : cs) = char c <|> oneOf' cs

string :: String -> Parser s String
string ""       = return ""
string (c : cs) = do
  a <- char c
  as <- string cs
  return $ a : as

string_ :: String -> Parser s ()
string_ = void . string
