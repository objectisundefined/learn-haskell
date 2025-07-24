{-# LANGUAGE OverloadedStrings #-}

module AIAgent.Tools.Function
  ( -- * Function Tool Types
    FunctionTool(..)
  , FunctionSchema(..)
  , ParameterSpec(..)
  
    -- * Function Tool Creation
  , mkFunctionTool
  , mkSimpleFunctionTool
  , mkMathTool
  , mkStringTool
  , mkFileTool
  
    -- * Predefined Tools
  , mathTools
  , stringTools
  , fileTools
  
    -- * Utilities
  , validateParameters
  , extractParameter
  ) where

import Data.Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import System.Directory
import System.FilePath

import AIAgent.Tools.Base

-- | Parameter specification for function tools
data ParameterSpec = ParameterSpec
  { paramName        :: Text
  , paramType        :: Text  -- "string", "number", "boolean", "array"
  , paramDescription :: Text
  , paramRequired    :: Bool
  , paramDefault     :: Maybe Value
  } deriving (Show, Eq)

-- | Schema definition for function tools
data FunctionSchema = FunctionSchema
  { schemaName        :: Text
  , schemaDescription :: Text
  , schemaParameters  :: [ParameterSpec]
  } deriving (Show, Eq)

-- | A function tool with schema validation
data FunctionTool = FunctionTool
  { funcTool   :: Tool
  , funcSchema :: FunctionSchema
  } deriving (Show)

-- | Create a function tool with schema validation
mkFunctionTool :: FunctionSchema 
               -> (HashMap Text Value -> IO (Either Text Value))
               -> FunctionTool
mkFunctionTool schema action = 
  let tool = mkAsyncTool (schemaName schema) $ \input -> do
        case validateParameters schema input of
          Left err -> return $ Left err
          Right validatedInput -> action validatedInput
  in FunctionTool tool schema

-- | Create a simple function tool without schema validation
mkSimpleFunctionTool :: Text -> Text -> (Value -> Value) -> FunctionTool
mkSimpleFunctionTool name description func =
  let schema = FunctionSchema name description []
      action input = do
        case HM.lookup "input" input of
          Just value -> return $ Right (func value)
          Nothing -> return $ Left "No input provided"
  in mkFunctionTool schema action

-- | Create mathematical function tools
mkMathTool :: Text -> (Double -> Double -> Double) -> FunctionTool
mkMathTool name operation =
  let schema = FunctionSchema name ("Mathematical operation: " <> name)
        [ ParameterSpec "a" "number" "First number" True Nothing
        , ParameterSpec "b" "number" "Second number" True Nothing
        ]
      action input = do
        case (extractParameter "a" input, extractParameter "b" input) of
          (Just (Number a), Just (Number b)) -> do
            let result = operation (realToFrac a) (realToFrac b)
            return $ Right $ Number (realToFrac result)
          _ -> return $ Left "Invalid number parameters"
  in mkFunctionTool schema action

-- | Create string manipulation tools
mkStringTool :: Text -> (Text -> Text) -> FunctionTool
mkStringTool name operation =
  let schema = FunctionSchema name ("String operation: " <> name)
        [ ParameterSpec "text" "string" "Input text" True Nothing ]
      action input = do
        case extractParameter "text" input of
          Just (String text) -> return $ Right $ String (operation text)
          _ -> return $ Left "Invalid string parameter"
  in mkFunctionTool schema action

-- | Create file operation tools
mkFileTool :: Text -> (FilePath -> IO Text) -> FunctionTool
mkFileTool name operation =
  let schema = FunctionSchema name ("File operation: " <> name)
        [ ParameterSpec "path" "string" "File path" True Nothing ]
      action input = do
        case extractParameter "path" input of
          Just (String path) -> do
            result <- operation (Text.unpack path)
            return $ Right $ String result
          _ -> return $ Left "Invalid file path parameter"
  in mkFunctionTool schema action

-- | Predefined mathematical tools
mathTools :: [FunctionTool]
mathTools = 
  [ mkMathTool "add" (+)
  , mkMathTool "subtract" (-)
  , mkMathTool "multiply" (*)
  , mkMathTool "divide" safeDiv
  , mkSimpleFunctionTool "sqrt" "Square root" sqrtFunc
  , mkSimpleFunctionTool "abs" "Absolute value" absFunc
  ]
  where
    safeDiv a b = if b == 0 then 0 else a / b
    sqrtFunc (Number n) = Number (realToFrac $ sqrt $ realToFrac n)
    sqrtFunc _ = String "Invalid input for sqrt"
    absFunc (Number n) = Number (abs n)
    absFunc _ = String "Invalid input for abs"

-- | Predefined string manipulation tools
stringTools :: [FunctionTool]
stringTools =
  [ mkStringTool "uppercase" Text.toUpper
  , mkStringTool "lowercase" Text.toLower
  , mkStringTool "reverse" Text.reverse
  , mkStringTool "length" (Text.pack . show . Text.length)
  , mkSimpleFunctionTool "concat" "Concatenate strings" concatFunc
  ]
  where
    concatFunc (Array values) = 
      let strings = [s | String s <- values]
      in String $ Text.concat strings
    concatFunc _ = String "Invalid input for concat"

-- | Predefined file operation tools
fileTools :: [FunctionTool]
fileTools =
  [ mkFileTool "read_file" TIO.readFile
  , mkFileTool "file_exists" (fmap (Text.pack . show) . doesFileExist)
  , mkFileTool "get_file_size" getFileSizeText
  , mkFileTool "get_extension" (return . Text.pack . takeExtension)
  ]
  where
    getFileSizeText path = do
      size <- getFileSize path
      return $ Text.pack $ show size

-- | Validate parameters against schema
validateParameters :: FunctionSchema -> HashMap Text Value -> Either Text (HashMap Text Value)
validateParameters schema input = do
  let required = filter paramRequired (schemaParameters schema)
      missing = filter (\param -> not $ HM.member (paramName param) input) required
  
  if not (null missing)
    then Left $ "Missing required parameters: " <> 
                Text.intercalate ", " (map paramName missing)
    else validateTypes schema input

-- | Validate parameter types
validateTypes :: FunctionSchema -> HashMap Text Value -> Either Text (HashMap Text Value)
validateTypes schema input = do
  let params = schemaParameters schema
  mapM_ (validateParam input) params
  return input
  where
    validateParam :: HashMap Text Value -> ParameterSpec -> Either Text ()
    validateParam inputMap param = 
      case HM.lookup (paramName param) inputMap of
        Nothing -> 
          if paramRequired param
            then Left $ "Missing required parameter: " <> paramName param
            else Right ()
        Just value -> 
          if validateType (paramType param) value
            then Right ()
            else Left $ "Invalid type for parameter " <> paramName param <> 
                       ": expected " <> paramType param

-- | Validate value type
validateType :: Text -> Value -> Bool
validateType "string" (String _) = True
validateType "number" (Number _) = True
validateType "boolean" (Bool _) = True
validateType "array" (Array _) = True
validateType "object" (Object _) = True
validateType _ _ = False

-- | Extract parameter from input
extractParameter :: Text -> HashMap Text Value -> Maybe Value
extractParameter key input = HM.lookup key input