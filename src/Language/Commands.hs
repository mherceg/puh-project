module Language.Commands where

import Data.Map as M
import Data.Char
import Language.Exec
import System.Directory
import System.Exit
import Data.List (intercalate, unfoldr)
import Numeric (showHex)
import Data.ByteString as BS (readFile, unpack, ByteString)
-- A map of (command name, command pairs), used to abstract command
-- execution and make adding new commands relatively easy
commands :: M.Map String Command
commands = M.fromList [
    ("exit", exit),
    ("mv", move),
    ("cp", copy),
    ("rm", remove),
    ("create", create),
    ("cpdir", cpDir),
    ("rmdir", rmDir),
    ("mkdir", mkDir),
    ("pwd", pwd),
    ("ls", ls),
    ("cd", cd),
    ("cat", cat),
    ("hexdump", hexdump)
    ]

exit :: Command
exit _ _ = do
	putStrLn "Thank you for using this shell!"
	exitSuccess

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

--copy a file or copy one or multiple files in an existing directory
copy :: Command
copy args ss 
	| length args == 2 = do
		copyFile (path ss (args!!0)) (path ss (args!!1))
		return ss
	| otherwise = do
		check <- doesDirectoryExist (path ss (last args))
		if check then do
			Prelude.mapM ((flip copy) ss) [init args]
			return ss
			else do 
				return $ writeError ss ("cp: " ++ (last args) ++ " is not a valid copy target.")

--remove one or more files
remove :: Command
remove args ss 
	| Prelude.null args = do
		return ss
	| otherwise = do
		let curr = head args
		check <- doesFileExist (path ss curr)
		if check then do
			removeFile (path ss curr)
			remove (tail args) ss
			else do
				remove (tail args) (writeError ss ("rm: " ++ curr ++ "is not a valid target for remove."))


--Creates one or more empty files
create :: Command
create args ss = do
	mapM (\x -> createSingle (path ss x)) args
	return ss
	where
		createSingle arg = do
			writeFile arg ""

--copy emptt directories to a target directory
cpDir :: Command
cpDir args ss 
	| length args == 1 = do
		return ss
	| otherwise = do
		let curr = path ss (head args)
		check <- doesDirectoryExist curr
		if not check then do
			cpDir (tail args) (writeError ss ("cpdir: " ++ (head args) ++ "is not a valid cpdir target"))
			else do
				content <- getDirectoryContents curr
				if not (Prelude.null content) then
					 cpDir (tail args) (writeError ss ("cpdir: " ++ (head args) ++ "is not a valid cpdir target"))
					 else do
					 	createDirectory ((path ss (last args)) ++ "/" ++ (head args))
					 	rmDir (tail args) ss

--remove one or more empty directories
rmDir :: Command
rmDir args ss
	| Prelude.null args = do
		return ss
	| otherwise = do
		let curr = path ss (head args)
		check <- doesDirectoryExist curr
		if not check then do
			rmDir (tail args) (writeError ss ("rmdir: " ++ (head args) ++ "is not a valid rmdir target"))
			else do
				content <- getDirectoryContents curr
				if not (Prelude.null content) then
					 rmDir (tail args) (writeError ss ("rmdir: " ++ (head args) ++ "is not a valid rmdir target"))
					 else do
					 	removeDirectory curr
					 	rmDir (tail args) ss

--create one or more new directories
mkDir :: Command 
mkDir args ss
	| Prelude.null args = do
		return ss
	| otherwise = do
		createDirectory (path ss (head args))
		mkDir (tail args) ss

--Print working directory
pwd :: Command
pwd _ ss = do
	pt <- canonicalizePath (wd ss)
	return $ writeError ss (pt)

--list contents of a directory
ls :: Command
ls args ss
	| Prelude.null args = ls' (wd ss)
	| otherwise = ls' (path ss (args !! 0))
	where
		ls' x = do
			check <- doesDirectoryExist x
			if not check then do
				return $ writeError ss ("ls: " ++ x ++ "is not a valid target")
				else do
					files <- getDirectoryContents x
					return $ printAll ss files
					where
						printAll ss' [] = ss'
						printAll ss' (x':xs) = printAll (writeError ss' x') xs

--change directory
cd :: Command
cd args ss 
	| Prelude.null args = do
		return ScriptState {output = output ss, wd = "~", vartable = vartable ss}
	| otherwise = do
		return ScriptState {output = output ss, wd = (path ss (args!!0)), vartable = vartable ss}

--concatenate files and print to standard output
cat :: Command
cat args ss
	| Prelude.null args = do
		return ss
	| otherwise = do
		content <- Prelude.readFile (path ss (head args))
		cat (tail args) (writeError ss content)

--outputs a file in hexadecimal format
hexdump :: Command
hexdump args ss = do
	content <- BS.readFile (path ss (args!!0))
	putStrLn $ simpleHex content
	return $ writeError ss $ concat $ Prelude.map ((flip showHex) "") $ BS.unpack content
	return ss

-- |'simpleHex' converts a 'ByteString' to a 'String' showing the octets
-- grouped in 32-bit words.
--
-- Sample output
--
-- @4b c1 ad 8a  5b 47 d7 57@
simpleHex :: BS.ByteString -> String
simpleHex c = concat
          $ Prelude.map ((flip showHex) "")
          $ BS.unpack c


--create file path with respect to executing directory
path :: ScriptState -> String -> FilePath
path ss name = wd ss ++ "/" ++ name

--add error message to current script state
writeError :: ScriptState -> String -> ScriptState
writeError ss message = 
	ScriptState  {output = output ss ++ message,
					wd = wd ss, vartable = vartable ss}