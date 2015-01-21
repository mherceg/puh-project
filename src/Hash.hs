module Hash where

import Language.Commands
import Language.Exec
import System.IO
-- The top-level module. Connects parsing to execution and adds interaction
-- with the user / reading from file.
-- Runs a .hash script
runScript :: FilePath -> IO ()
runScript fp = do
	input <- readFile fp
	let ln = lines input
	putStrLn (unlines ln) 
-- Communicates with the user and performs hash commands line by line
runInteractive :: IO ()
runInteractive = do
	hSetBuffering stdout NoBuffering
	state <- createEmptyScriptState
	newState <- chmod ["777","proba"] state
	putStrLn (output newState)
	return ()