import Prelude hiding (lookup)
import System.IO
import System.Environment
import Control.Applicative
import Trie
import Data.List hiding (insert)

flush :: IO ()
flush = hFlush stdout

loadFromFile :: String -> IO (Trie Char Bool)
loadFromFile filepath = foldl' (\trie word -> insert word True trie) emptyTrie . lines <$> readFile filepath

testComplete :: Trie Char Bool -> IO ()
testComplete trie = do
  putStr ">>= "
  flush
  word <- getLine
  putStr $ unlines . take 5 . map (word++) $ (completions word trie)
  flush
  testComplete trie

main = do
  [filename] <- getArgs
  trie <- loadFromFile filename
  testComplete trie
