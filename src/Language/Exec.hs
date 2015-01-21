module Language.Exec where 

import Data.Map as M
import Language.Expressions
import System.Directory
-- A model of a command which is waiting for arguments and a state to run
type Command = [String] -> ScriptState -> IO ScriptState
-- A table of variables, in fact a map of (Name, Value) pairs.
type VarTable = M.Map String String
-- A command table - abstracted command execution, (contains command name,
-- command) pairs. Simplest, but hardly the best way to implement this.
type CommandTable = M.Map String Command
-- A script state containing the last output, current working directory and
-- the current table of variables.
data ScriptState = ScriptState { output :: String
, wd :: FilePath
, vartable :: VarTable
} deriving Show

createEmptyScriptState :: IO ScriptState
createEmptyScriptState = do
	currentDir <- getCurrentDirectory
	return ScriptState { output = "", wd = currentDir, vartable = M.empty}


runTopLevel :: CommandTable -> ScriptState -> TLExpr -> IO ScriptState
runTopLevel table ss expr = do
	if (expr == Emptyy ) then do
		return ss
		else do
			let cmd = unmaybe (M.lookup (name (unwrap expr)) table) 
			newState <- (cmd (args (unwrap expr)) ss)
			case (outDir (unwrap expr)) of
				Nothing -> putStrLn (output newState)
				(Just x) -> if append (unwrap expr) then do
					appendFile (Language.Exec.path newState x) (output newState)
					else do
						writeFile (Language.Exec.path newState x) (output newState)
			return ScriptState { output = "", wd = wd newState, vartable = vartable newState}

	where
		unwrap (TLCmd cm) = cm
		unwrap _ = Cmd { name = "void", args = [], inDir = Nothing, outDir = Nothing, append = False}
		unmaybe :: Maybe Command -> Command
		unmaybe (Just x) = x
		unmaybe Nothing = unmaybe (M.lookup "void" table)


--create file path with respect to executing directory
path :: ScriptState -> String -> FilePath
path ss nmm = wd ss ++ "/" ++ nmm