{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

-- | Claude Code / Codex / Gemini–style agent tools.
--
-- Provides real, working tool implementations for code-agent workflows:
--
-- * 'readFileTool'   – read a file (optionally a line range)
-- * 'editFileTool'   – search-and-replace edit within a file
-- * 'todoWriteTool'  – create or overwrite a file with content
-- * 'grepTool'       – recursive pattern search via the system @grep@
-- * 'bashTool'       – execute a shell command and capture output
-- * 'webSearchTool'  – stub for web search (requires an API key to be useful)
-- * 'codeAgentTools' – all of the above bundled as a list
-- * 'codeAgentToolAgent' – a combined 'Agent' that routes by tool name

module AIAgent.Tools.CodeAgent
  ( -- * Individual Tools
    readFileTool
  , editFileTool
  , todoWriteTool
  , grepTool
  , bashTool
  , webSearchTool

    -- * Bundled
  , codeAgentTools
  , codeAgentToolAgent
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value(..), object, (.=))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.Exit (ExitCode(..))
import System.FilePath (takeDirectory)
import System.Process (readProcessWithExitCode)

import AIAgent.Agents.Base (Agent, mkStatelessAgent, AgentCapability(..))
import AIAgent.Tools.Base

-- ---------------------------------------------------------------------------
-- Read
-- ---------------------------------------------------------------------------

-- | Read a file's contents, optionally restricted to a line range.
--
-- Parameters:
--
-- * @path@ (required) – absolute or relative file path
-- * @start_line@ (optional) – first line to include (1-based)
-- * @end_line@ (optional) – last line to include (1-based, -1 for end-of-file)
readFileTool :: Tool
readFileTool = mkAsyncTool "Read" $ \input -> do
  case HM.lookup "path" input of
    Just (String path) -> do
      let fp = Text.unpack path
      exists <- doesFileExist fp
      if not exists
        then return $ Left $ "File not found: " <> path
        else do
          content <- TIO.readFile fp
          let allLines = Text.lines content
              startLine = extractInt "start_line" input
              endLine   = extractInt "end_line" input
              selected  = sliceLines startLine endLine allLines
              numbered  = zipWith (\n l -> Text.pack (show n) <> ". " <> l)
                                  [(maybe 1 id startLine) ..] selected
          return $ Right $ object
            [ "content" .= Text.unlines numbered
            , "path"    .= path
            , "lines"   .= length selected
            ]
    _ -> return $ Left "Missing required parameter: path"

-- ---------------------------------------------------------------------------
-- Edit (search-and-replace)
-- ---------------------------------------------------------------------------

-- | Perform a precise search-and-replace edit in a file.
--
-- Parameters:
--
-- * @path@ (required)    – file path
-- * @old_str@ (required) – exact text to find (must occur exactly once)
-- * @new_str@ (required) – replacement text
editFileTool :: Tool
editFileTool = mkAsyncTool "Edit" $ \input -> do
  case (HM.lookup "path" input, HM.lookup "old_str" input, HM.lookup "new_str" input) of
    (Just (String path), Just (String oldStr), Just (String newStr)) -> do
      let fp = Text.unpack path
      exists <- doesFileExist fp
      if not exists
        then return $ Left $ "File not found: " <> path
        else do
          content <- TIO.readFile fp
          let occurrences = countOccurrences oldStr content
          case occurrences of
            0 -> return $ Left $ "old_str not found in " <> path
            1 -> do
              let newContent = Text.replace oldStr newStr content
              TIO.writeFile fp newContent
              return $ Right $ object
                [ "status"  .= ("ok" :: Text)
                , "path"    .= path
                , "message" .= ("Replaced 1 occurrence" :: Text)
                ]
            n -> return $ Left $ "old_str is not unique in " <> path
                               <> " (found " <> Text.pack (show n) <> " occurrences)"
    _ -> return $ Left "Missing required parameters: path, old_str, new_str"

-- ---------------------------------------------------------------------------
-- TodoWrite (create / overwrite file)
-- ---------------------------------------------------------------------------

-- | Create or overwrite a file with the given content.
-- Parent directories are created automatically.
--
-- Parameters:
--
-- * @path@ (required)    – target file path
-- * @content@ (required) – full file content
todoWriteTool :: Tool
todoWriteTool = mkAsyncTool "TodoWrite" $ \input -> do
  case (HM.lookup "path" input, HM.lookup "content" input) of
    (Just (String path), Just (String content)) -> do
      let fp = Text.unpack path
      createDirectoryIfMissing True (takeDirectory fp)
      result <- try (TIO.writeFile fp content)
      case result of
        Left (ex :: SomeException) ->
          return $ Left $ "Write failed: " <> Text.pack (show ex)
        Right () ->
          return $ Right $ object
            [ "status"  .= ("ok" :: Text)
            , "path"    .= path
            , "bytes"   .= Text.length content
            ]
    _ -> return $ Left "Missing required parameters: path, content"

-- ---------------------------------------------------------------------------
-- Grep (pattern search)
-- ---------------------------------------------------------------------------

-- | Search for a pattern in files using the system @grep@ command.
--
-- Parameters:
--
-- * @pattern@ (required)        – regex or fixed-string pattern
-- * @path@ (optional)           – directory or file to search (default @.@)
-- * @include@ (optional)        – glob for file names, e.g. @\"*.hs\"@
-- * @case_insensitive@ (optional) – @true@ for @-i@ flag
grepTool :: Tool
grepTool = mkAsyncTool "Grep" $ \input -> do
  case HM.lookup "pattern" input of
    Just (String pat) -> do
      let searchPath    = maybe "." Text.unpack (extractText "path" input)
          includeGlob   = extractText "include" input
          caseFlag      = case HM.lookup "case_insensitive" input of
                            Just (Bool True) -> ["-i"]
                            _                -> []
          includeFlag   = maybe [] (\g -> ["--include=" ++ Text.unpack g]) includeGlob
          args          = ["-rn"] ++ caseFlag ++ includeFlag
                          ++ ["--", Text.unpack pat, searchPath]
      (exitCode, stdout, stderr) <- readProcessWithExitCode "grep" args ""
      case exitCode of
        ExitSuccess   -> return $ Right $ object
          [ "matches" .= Text.pack stdout
          , "count"   .= length (lines stdout)
          ]
        ExitFailure 1 -> return $ Right $ object
          [ "matches" .= ("" :: Text)
          , "count"   .= (0 :: Int)
          ]
        ExitFailure n -> return $ Left $
          "grep failed (exit " <> Text.pack (show n) <> "): " <> Text.pack stderr
    _ -> return $ Left "Missing required parameter: pattern"

-- ---------------------------------------------------------------------------
-- Bash (shell execution)
-- ---------------------------------------------------------------------------

-- | Execute a shell command and capture its output.
--
-- Parameters:
--
-- * @command@ (required) – the shell command to run
-- * @timeout@ (optional) – not enforced here; callers can wrap with 'executeToolWithTimeout'
bashTool :: Tool
bashTool = mkAsyncTool "Bash" $ \input -> do
  case HM.lookup "command" input of
    Just (String cmd) -> do
      (exitCode, stdout, stderr) <- readProcessWithExitCode "bash" ["-c", Text.unpack cmd] ""
      let code = case exitCode of
                   ExitSuccess   -> 0 :: Int
                   ExitFailure n -> n
      return $ Right $ object
        [ "exit_code" .= code
        , "stdout"    .= Text.pack stdout
        , "stderr"    .= Text.pack stderr
        ]
    _ -> return $ Left "Missing required parameter: command"

-- ---------------------------------------------------------------------------
-- WebSearch (stub requiring external API)
-- ---------------------------------------------------------------------------

-- | Web search tool. Requires an external search API (e.g. SerpAPI, Brave Search).
-- Without a configured @api_key@ this tool returns an informative error.
--
-- Parameters:
--
-- * @query@ (required)   – search query
-- * @api_key@ (optional) – API key for the search provider
webSearchTool :: Tool
webSearchTool = mkAsyncTool "WebSearch" $ \input -> do
  case HM.lookup "query" input of
    Just (String _query) ->
      case extractText "api_key" input of
        Nothing -> return $ Left
          "WebSearch requires an api_key parameter. Configure a search provider (SerpAPI, Brave Search, etc.)."
        Just _apiKey ->
          -- A real implementation would make an HTTP request here.
          -- Since we have no HTTP dependency, we return an actionable error.
          return $ Left
            "WebSearch HTTP backend not compiled in. Add an http-client dependency and supply a handler."
    _ -> return $ Left "Missing required parameter: query"

-- ---------------------------------------------------------------------------
-- Bundle
-- ---------------------------------------------------------------------------

-- | All code-agent tools collected in a single list.
codeAgentTools :: [Tool]
codeAgentTools =
  [ readFileTool
  , editFileTool
  , todoWriteTool
  , grepTool
  , bashTool
  , webSearchTool
  ]

-- | A combined 'Agent' that routes incoming requests to the appropriate tool
-- based on the @\"tool\"@ key in the input map.
codeAgentToolAgent :: Agent
codeAgentToolAgent = mkStatelessAgent "CodeAgent" [ToolUsage, DataRetrieval, Computation] $ \input ->
  case HM.lookup "tool" input of
    Just (String name) ->
      case find (\t -> toolName t == name) codeAgentTools of
        Just tool -> do
          result <- executeTool tool input
          case result of
            Left err  -> return $ Left (toolErrorMessage err)
            Right res -> return $ Right (toolResultData res)
        Nothing -> return $ Left $ "Unknown tool: " <> name
                     <> ". Available: " <> Text.intercalate ", " (map toolName codeAgentTools)
    _ -> return $ Left "Missing \"tool\" key in input. Available: Read, Edit, TodoWrite, Grep, Bash, WebSearch"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

extractText :: Text -> HashMap Text Value -> Maybe Text
extractText key m = case HM.lookup key m of
  Just (String t) -> Just t
  _               -> Nothing

extractInt :: Text -> HashMap Text Value -> Maybe Int
extractInt key m = case HM.lookup key m of
  Just (Number n) -> Just (round n)
  _               -> Nothing

sliceLines :: Maybe Int -> Maybe Int -> [Text] -> [Text]
sliceLines mStart mEnd ls =
  let start = maybe 0 (\s -> max 0 (s - 1)) mStart
      end_  = case mEnd of
                Nothing  -> length ls
                Just (-1) -> length ls
                Just e   -> min (length ls) e
  in take (end_ - start) (drop start ls)

countOccurrences :: Text -> Text -> Int
countOccurrences needle haystack
  | Text.null needle = 0
  | otherwise        = go 0 haystack
  where
    go !acc txt
      | Text.null txt = acc
      | needle `Text.isPrefixOf` txt = go (acc + 1) (Text.drop (Text.length needle) txt)
      | otherwise = go acc (Text.drop 1 txt)
