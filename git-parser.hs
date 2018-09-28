import Data.Either

data Command = CmdBranch | CmdFile deriving (Show, Eq)

cmdParser :: String -> Either String Command
cmdParser str = case str of
  "branch" -> Right CmdBranch
  "file" -> Right CmdFile
  str' -> Left ("Unknown command: " ++ str')

data SubCommand = CmdList | CmdCreate | CmdRemove deriving (Show, Eq)

subCmdParser :: String -> Either String SubCommand
subCmdParser str = case str of
  "list" -> Right CmdList
  "create" -> Right CmdCreate
  "remove" -> Right CmdRemove
  str' -> Left ("Unknown sub command: " ++ str')

mainParser :: String -> Either String (Command, SubCommand)
mainParser str =
  let [cmdStr, subCmdStr] = words str
  in (,) <$> cmdParser cmdStr <*> subCmdParser subCmdStr
