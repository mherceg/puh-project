module Main where

import Prelude
import Data.List
import System.Environment
import System.Directory
import Hash
main :: IO ()
main = do
	a <- getArgs
	exists <- doesFileExist (a !! 0)
	if exists then if check (a!!0) then Hash.runScript (a!!0) else
		putStrLn "Invalid file, should be a .hash script"
		else
			putStrLn ("File " ++ (a !! 0) ++ " doesn't exist")


	{-if null a then Hash.runInteractive else 
		if (check (a!!0)) then Hash.runScript (a!!0) else do
			if exists then 
				putStrLn "Invalid file, should be a .hash script"
				else
					putStrLn ("File " ++ (a !! 0) ++ " doesn't exist")
					-}
	where
		check x = ".hash" `isSuffixOf` x