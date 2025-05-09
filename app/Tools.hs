{-
 -  Module containing data structures pertaining to tools in the Claude API
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Tools (tools, ToolDef(..)) where
import Data.Aeson
import Prelude hiding (readFile)
import System.Directory
import Data.Text
import Control.Exception
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import System.IO.Error
import Control.Monad.IO.Class


-- All of the tools we support
tools :: [ToolDef]
tools = 
    [ readFile
    , listFiles
    , editFile
    ]

-- The definition of a tool
-- Name, description, and schema are all presented to the LLM 
-- function is an IO action invoked when the model wants to call the tool
data ToolDef = ToolDef
    { name :: Text 
    , description :: Text 
    , input_schema :: Value -- A JSON Schema that describes what values we expect
    , function :: Value -> IO (Either Text Text)
    } deriving (Generic)

instance Show ToolDef where
    show ToolDef {name, description, input_schema } = 
        "ToolDef { " <> items <> " }"
        where 
            items = unpack $ intercalate ", " [name,description, show' input_schema]
            show' = pack . show

-- Serialize TooLDef's by skipping function
instance ToJSON ToolDef where
 toJSON (ToolDef name description input_schema _) =
        object
            [ "name" .= name
            , "description" .= description
            , "input_schema" .= input_schema
            ]

-- Below are our tool defintions

-- Tool for reading files

readFile :: ToolDef
readFile = ToolDef 
    { name = "read_file"
    , description = "Read the contents of a given relative file path. Use this when you want to see what's inside a file. Do not use this with directory names."
    , input_schema =  readFileSchema
    , function = readFileFunc
    }

newtype SingleFile = SF
    { file_name :: Text }
    deriving (Show, Generic)

instance FromJSON SingleFile

readFileFunc :: Value -> IO (Either Text Text)
readFileFunc input = 
    let parsed = fromJSON input :: Result SingleFile in
    case parsed of
        Success SF { file_name } -> do
            liftIO (readFile' (unpack file_name))
        _ -> return (Left "Invalid argument")


readFile' :: FilePath -> IO (Either Text Text)
readFile' path = do
    catch 
        (Right <$> TIO.readFile path)
        (\e -> if isDoesNotExistError e then return (Left "File does not exist") else ioError e)

readFileSchema :: Value
readFileSchema = 
    object 
        [ "type" .= String "object"
        , "properties" .= object 
            [ "file_name" .= object  
                [ "type" .= String "string"
                , "description" .= String "The file name to read"
                ]
            ]
        ]

-- tool for listing files
listFiles :: ToolDef
listFiles = ToolDef 
    { name = "list_files"
    , description = "List the contents of a given relative file path. Use this when you want to see what files are in a directory. Only use this with directory names."
    , input_schema = listFileSchema
    , function = listFileFunction
    }

listFileSchema :: Value
listFileSchema =
    object 
        [ "type" .= String "object"
        , "properties" .= object
            [ "file_name" .= object 
                [ "type" .= String "string" 
                , "description" .= String "The directory name to list"
                ]
            ]
        ]

listFileFunction :: Value -> IO (Either Text Text)
listFileFunction v = 
    let parsed = fromJSON v :: Result SingleFile in
    case parsed of
        Success SF { file_name }-> do
            files <- listDirectory (unpack file_name)
            let msg = intercalate "\n" [ "- " <> pack file | file <- files ]
            return (Right msg)
        _ -> return (Left "failed to parse input")
        

-- tool for eiditing files

editFile :: ToolDef
editFile = ToolDef 
    { name = "edit_file"
    , description = "Make edits to a text file.\nReplaces 'old_str' with 'new_str' in the given file. 'old_str' and 'new_str' MUST be different from each other.\nIf the file specified with path doesn't exist, it will be created."
    , input_schema = editFileSchema
    , function = editFileFunction
    }

editFileSchema :: Value
editFileSchema =
    object 
        [ "type" .= String "object"
        , "properties" .= object
            [ "file_name" .= object
                [ "type" .= String "string"
                , "description" .= String "The file name to edit"
                ]
            , "old_str" .= object  
                [ "type" .= String "string"
                , "description" .= String "Text to search for - must match exactly and must only have one match exactly"
                ]
            , "new_str" .= object
                [ "type" .= String "string"
                , "description" .= String "Text to replace old_str with"
                ]
            ]
        ]

data EditFileInput = EFI
    { file :: Text
    , old_str :: Text
    , new_str :: Text
    } deriving (Show,Generic)

    
instance FromJSON EditFileInput where
  parseJSON = withObject "EditFileInput" $ \o -> do
    file    <- o .: "file_name"  -- JSON key ➜ record field ‘file’
    old_str <- o .: "old_str"
    new_str <- o .: "new_str"
    pure EFI{ file, old_str, new_str }
    
editFileFunction :: Value -> IO (Either Text Text)
editFileFunction v = 
    let parsed = fromJSON v :: Result EditFileInput in
    case parsed of
        Success EFI { file, old_str, new_str } -> do
            fileExists <- doesFileExist (unpack file)
            if fileExists
                then do
                    contents <- TIO.readFile (unpack file)
                    let updated = replace old_str new_str contents
                    TIO.writeFile (unpack file) updated
                    pure $ Right "file updated"
                else do
                    -- If file doesn't exist, create a new empty file with the new_str
                    TIO.writeFile (unpack file) new_str
                    pure $ Right "new file created"
        _ -> return $ Left "failed to parse input"
