module Language.Commands where

import Data.Map as M
import Language.Exec
import System.Directory
-- A map of (command name, command pairs), used to abstract command
-- execution and make adding new commands relatively easy
commands :: M.Map String Command
commands = M.fromList [
    ("welcome", welcome),
    ("exit", exit),
    ("mv", move),
    ("cp", copy),
    ("cpdir", copyDir),
--    ("rm", remove),
--    ("rmdir", removeDir),
    ("create", create)
--    ("mkdir", makeDir),
--    ("pwd", pointWorkingDir),
--    ("ls", list),
--    ("cd", changeDir),
--    ("cat", cat)
    ]

welcome ::Command
welcome _ ss = do
	return ScriptState { output = "Wlecome", wd = wd ss, vartable = M.empty}

exit :: Command
exit = undefined

--Command for moving and renaming files
move :: Command
move [] ss = do
	return (writeError ss "mv: expecting at least two parameters\n")

move [_] ss = move [] ss

move args ss
	| length args == 2 = do
		check <- doesDirectoryExist (path ss (args !! 1))
		if check then 
				renameFile (path ss (args !! 0)) ((path ss (args !! 1)) ++ "/" ++ (args!!0))
			else
				renameFile (path ss (args !! 0)) (path ss (args !! 1))
		return ss
	| otherwise = do
		check <- doesDirectoryExist (path ss (last args))
		if not check then do
			return (writeError ss ("mv: " ++ (last args) ++ " is not a valid target directory\n"))
			else do
				Prelude.mapM ((flip move) ss) [init args]
				return ss


copy :: Command
copy = undefined

copyDir :: Command
copyDir = undefined

--Creates one or more empty files
create :: Command
create args ss = do
	mapM (\x -> createSingle (path ss x)) args
	return ss
	where
		createSingle arg = do
			writeFile arg ""

--create file path with respect to executing directory
path :: ScriptState -> String -> FilePath
path ss name = wd ss ++ "/" ++ name

--add error message to current script state
writeError :: ScriptState -> String -> ScriptState
writeError ss message = 
	ScriptState  {output = output ss ++ message,
					wd = wd ss, vartable = vartable ss}