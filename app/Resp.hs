{-
 -  Module containing data structures pertaining to responses from the Claude API
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Resp 
    (Resp(..)
    , RespObject(..)
    , response
    , ToolUse(..)
    )
where
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson

-- A response is a list of objects
newtype Resp = Resp { content :: [RespObject] } 
    deriving (Show, Generic) 

-- Fetches the last object in the response, which is Cladue's most recent message
response :: Resp -> RespObject
response = last . content

instance FromJSON Resp

-- Responses are either text or tool invocations
data RespObject 
    = Text Text
    | Tool ToolUse
    deriving (Show,Generic)

-- Tool invocations have an id, a tool namae, and a JSON object to be evaluated by the tool
data ToolUse = ToolUse 
    { id :: Text 
    , name :: Text 
    , input :: Value
    } deriving (Show,Generic)

instance ToJSON ToolUse

instance FromJSON RespObject where
    parseJSON = withObject "RespObject" $ \obj ->  do 
        typ :: Text <- obj .: "type" 
        case typ of 
            "text" -> Text <$> obj .: "text"
            "tool_use" -> build <$> obj .: "id" <*> obj .: "name" <*> obj .: "input"
            _ -> fail $ "Unexpected type tag: " <> T.unpack typ
        where
            build toolId name input = Tool (ToolUse toolId name input)
