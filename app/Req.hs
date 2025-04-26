{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Req where
import Data.Text (Text, intercalate)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson
import qualified Content as C
import qualified Resp

data ToolDef = ToolDef
    { name :: Text 
    , description :: Text 
    , input_schema :: Value
    , function :: Value -> IO (Either Text Text)
    } deriving (Generic)


instance Show ToolDef where
    show ToolDef {name, description, input_schema } = 
        "ToolDef { " <> items <> " }"
        where 
            items = T.unpack $ intercalate ", " [name,description, show' input_schema]
show' :: Show a => a -> Text 
show' = T.pack . show

instance ToJSON ToolDef where
 toJSON (ToolDef name description input_schema _) =
        object
            [ "name" .= name
            , "description" .= description
            , "input_schema" .= input_schema
            ]

data Req = Req
    { model :: Text
    , max_tokens :: Int
    , messages :: [Message]
    , tools :: [ToolDef]
    } deriving (Show,Generic)

fromMessages :: [Message] -> [ToolDef] -> Req
fromMessages = Req "claude-3-7-sonnet-20250219" 1024 . reverse

instance ToJSON Req

data Message = Message 
    { role :: Text 
    , content :: C.Content
    } deriving (Show,Generic)


user :: Text -> Message
user = Message "user" . C.Text

toolResult :: Bool -> Text -> Text -> Message 
toolResult err toolId msg =  Message "user"  (C.toolResult toolId err msg)

toolUse :: Resp.ToolUse -> Message
toolUse tu = Message "assistant" (C.ToolUse tu)

assistant :: Text -> Message
assistant = Message "assistant" . C.Text

instance ToJSON Message where
    toJSON = genericToJSON defaultOptions { omitNothingFields = True }
