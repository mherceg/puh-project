module Hash where

import Language.Commands
import Language.Exec
import Language.Expressions
import System.IO
import Data.List
import Control.Monad
-- The top-level module. Connects parsing to execution and adds interaction
-- with the user / reading from file.
-- Runs a .hash script
runScript :: FilePath -> IO ()
runScript fp = do
	input <- readFile fp
	let coms = map createExpr (lines (input ++ "\nexit") ) 
	state <- createEmptyScriptState
	foldM (runTopLevel commands) state coms
	return ()

-- Communicates with the user and performs hash commands line by line
runInteractive :: IO ()
runInteractive = do
	hSetBuffering stdout NoBuffering
	state <- createEmptyScriptState
	putStr "Hash> "
	line <- getLine
	runOne (createExpr line) state
	where
		runOne com ss = do
			newState <- runTopLevel commands ss com
			putStr "Hash> "
			line <- getLine
			runOne (createExpr line) newState

createExpr :: String -> TLExpr
createExpr input = 
	let ws = words $ takeWhile(\x -> x /= '#') input in
	if null ws then Emptyy else
		TLCmd Cmd { name = head ws
			, args = filter (\x -> head x /= '<' && head x /= '>') (tail ws)
			, inDir = isValid $ filter (\x -> head x == '<') (tail ws)
			, outDir = isValid $ filter (\x -> head x == '>') (tail ws)
			, append = shouldAppend $ filter (\x -> head x == '>') (tail ws)
			} 
	where
		isValid arg
			| null arg = Nothing
			| otherwise = Just $ dropWhile (\x -> x == '<' || x == '>') $ head arg
		shouldAppend arg
			| null arg = False
			| otherwise = isPrefixOf ">>" (head arg)
