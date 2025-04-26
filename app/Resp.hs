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

newtype Resp = Resp { 
        content :: [RespObject] 
    } deriving (Show, Generic) 


response :: Resp -> RespObject
response = last . content

instance FromJSON Resp

data RespObject 
    = Text Text
    | Tool ToolUse
    deriving (Show,Generic)


data ToolUse = ToolUse 
    { id :: Text 
    , name :: Text 
    , input :: Value
    } deriving (Show,Generic)

instance ToJSON ToolUse

build :: Text -> Text -> Value -> RespObject
build id name input = Tool (ToolUse id name input)

instance FromJSON RespObject where
    parseJSON = withObject "RespObject" $ \obj ->  do 
        typ :: Text <- obj .: "type" 
        case typ of 
            "text" -> Text <$> obj .: "text"
            "tool_use" -> build <$> obj .: "id" <*> obj .: "name" <*> obj .: "input"
            _ -> fail $ "Unexpected type tag: " <> T.unpack typ
        


