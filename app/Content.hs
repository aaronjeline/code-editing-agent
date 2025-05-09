{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Content where
import Data.Text
import Data.Aeson
import GHC.Generics (Generic)
import qualified Data.Vector as V
import qualified Resp

-- "Content" blocks in the Claude API
data Content 
    = Text Text
    | ToolResult ToolResult 
    | ToolUse Resp.ToolUse -- The conversation we present to Claude has to contain the tool invocation
    deriving (Generic)

instance Show Content where
    show (Text t) = unpack t
    show (ToolResult r) = show $ toJSON r
    show (ToolUse use) = show $ toJSON use

-- Builds a content block containing the result of a tool
toolResult :: Text -> Bool -> Text -> Content
toolResult tool_use_id is_error content = 
    ToolResult (TR 
        { tool_use_id
        , is_error
        , content
        })

-- Builds a content block containing a tool invocation
toolUse :: Text -> Text -> Value -> Content
toolUse id name input = ToolUse (Resp.ToolUse { Resp.id, Resp.name, Resp.input })

instance ToJSON Content where
    toJSON (Text t) = String t
    toJSON (ToolResult TR { tool_use_id, is_error, content } ) = Array $ V.fromList [object
        [ "type" .= String "tool_result"
        , "is_error" .= Bool is_error 
        , "tool_use_id" .= String tool_use_id
        , "content" .= content
        ]]
    toJSON (ToolUse (Resp.ToolUse {Resp.id, Resp.name, Resp.input}) )  =
        Array $ V.fromList [object 
        [ "type" .= String "tool_use"
        , "id" .= String id 
        , "input" .= input
        , "name" .= name
        ]
        ]

data ToolResult = TR 
    { tool_use_id :: Text 
    , is_error :: Bool 
    , content :: Text
    } deriving (Show,Generic)


instance ToJSON ToolResult
