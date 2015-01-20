import Data.List
import Data.Char

main :: IO ()
main = do
	a <- getLine
	putStrLn (f a)

f :: String -> String
f x = concat (replicate 3 x) 