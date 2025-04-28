{-
 -  Module containing the main interface to the Cluade API
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
module Claude 
    (Agent
    , build 
    , invoke 
    , Claude(..)
    , runClaude
    , MonadClaude
    )
where
import System.Environment (lookupEnv)
import Network.HTTP.Req
import Data.ByteString (ByteString)
import Data.Text (Text, unpack)
import Data.ByteString.Char8 (pack)
import qualified Req
import qualified Resp
import Control.Monad.Reader
import Tools (ToolDef(..))

-- This is our Claude monad, which supports the following effects
-- 1) Reading from the Agent data structure
-- 2) Failing 
-- 3) Arbitrary IO
newtype Claude a = Claude (ReaderT Agent IO a) 
    deriving (Functor, Applicative, Monad, MonadReader Agent, MonadIO, MonadFail)

-- Our agent has access to an API Key and a list of tools
data Agent = Agent 
    { apiKey :: ByteString 
    , tools :: [ToolDef] 
    }

-- Introduce a TypeClass for Claude monad, 
class (MonadFail m, MonadIO m, MonadReader Agent m) => MonadClaude m where

instance MonadClaude Claude where

-- Evaluate CaLaude actions given a set of tool definitions
runClaude :: [ToolDef] -> Claude a -> IO a
runClaude tools (Claude a) = do
    c <- build tools
    runReaderT a c

-- Given tool definitions, read the API key from process environment 
-- and return an agent
build :: [ToolDef] -> IO Agent
build tools = do
    Just apiKey <- (pack <$>) <$> lookupEnv "ANTHROPIC_API_KEY"
    return Agent { apiKey, tools }

-- Core piece of the puzzle: given a list of message,  
-- inovke the Cladue API to get the next message.
-- If we get back a tool use, run the tool, then loop and invoke again 
invoke :: MonadClaude m => [Req.Message] -> m Req.Message
invoke messages = do
    tools <- asks tools
    -- Build up our web request from our conversation so far
    let request = Req.fromMessages messages tools
    body <- makeWebRequest request
    case Resp.response body of
        -- If we got a text response, we're done, return the new message
        Resp.Text t ->  return (Req.assistant t) 
        -- If we got a tool use: invoke the tool and loop
        Resp.Tool toolUse -> do 
            toolAns <- useTool toolUse
            -- Message containing the inovcation 
            let useMsg = Req.toolUse toolUse 
            -- Message containing the answer
            let ansMsg = case toolAns of 
                    Right answer -> Req.toolResult False (Resp.id toolUse) answer
                    Left errMsg -> Req.toolResult True (Resp.id toolUse) errMsg
            -- Loop with new messages appended to the conversation 
            invoke (ansMsg:useMsg:messages)

-- Invoke a tool, and log to the console
useTool :: (MonadClaude m) => Resp.ToolUse -> m (Either Text Text)
useTool Resp.ToolUse { Resp.name, Resp.input } = do
    tool <- findTool name
    let msg = "Tool " <> show name <> "(" <> show input <> ")"
    liftIO $ do 
        putStrLn msg
        function tool input

-- Find a tool by name, failing if we can't find it
findTool :: (MonadClaude m) => Text -> m ToolDef
findTool toolName = do
    t <- asks tools
    find t
    where
        find [] = fail ("No such tool" <> unpack toolName)
        find (tool:tools)
            | name tool == toolName = return tool
            | otherwise = find tools

-- Make a web request to the Claude Messages API
makeWebRequest :: MonadClaude m => Req.Req -> m Resp.Resp
makeWebRequest request = do
    key <- asks apiKey
    runReq defaultHttpConfig $ do
        response <- req 
            POST
            (https "api.anthropic.com" /: "v1" /: "messages")
            (ReqBodyJson request)
            jsonResponse 
            (headers key)
        return (responseBody response)

-- Construct the HTTP headers we need
headers :: ByteString -> Option a
headers key = 
    header "x-api-key" key
    <> header "anthropic-version" "2023-06-01"
    <> header "content-type" "application/json"

