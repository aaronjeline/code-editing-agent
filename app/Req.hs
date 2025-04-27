{-
 -  Module containing data structures pertaining to requests to the Claude API
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Req 
    (Req
    , fromMessages
    , Message(..)
    , user
    , assistant 
    , toolUse
    , toolResult
    ) where
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson
import qualified Content as C
import qualified Resp
import Tools (ToolDef)

{- 
 - Requests to the Claude API
 -}
data Req = Req
    { model :: Text
    , max_tokens :: Int
    , messages :: [Message]
    , tools :: [ToolDef]
    } deriving (Show,Generic)

instance ToJSON Req

-- Construct a request from a list of messsages and tools, 
-- selecting default values for the other fields
fromMessages :: [Message] -> [ToolDef] -> Req
fromMessages = Req "claude-3-7-sonnet-20250219" 1024 . reverse


-- A Message in the API. 
data Message = Message 
    { role :: Text 
    , content :: C.Content
    } deriving (Generic)

instance Show Message where
    show (Message { role, content }) = 
        T.unpack role <> ": " <> show content

instance ToJSON Message where
    toJSON = genericToJSON defaultOptions { omitNothingFields = True }

-- Construct a text message from the user
user :: Text -> Message
user = Message "user" . C.Text

-- Construct a message from the assistant attempting to invoke a tool
toolUse :: Resp.ToolUse -> Message
toolUse tu = Message "assistant" (C.ToolUse tu)

-- Construct a message form the user showing the result of a tool
toolResult :: Bool -> Text -> Text -> Message 
toolResult err toolId msg =  Message "user"  (C.toolResult toolId err msg)

-- Construct a text message from the assistant
assistant :: Text -> Message
assistant = Message "assistant" . C.Text

