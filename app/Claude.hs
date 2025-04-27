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
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Char8 (pack)
import qualified Req
import qualified Resp
import Control.Monad.Reader
import Tools (ToolDef(..))

-- This is our Claude monad, which can read from the agent dat structure 
-- and do IO
newtype Claude a = Claude (ReaderT Agent IO a) 
    deriving (Functor, Applicative, Monad, MonadReader Agent, MonadIO, MonadFail)

data Agent = Agent 
    { apiKey :: ByteString 
    , tools :: [ToolDef] 
    }

class (MonadFail m, MonadIO m, MonadReader Agent m) => MonadClaude m where

instance MonadClaude Claude where

runClaude :: [ToolDef] -> Claude a -> IO a
runClaude tools (Claude a) = do
    c <- build tools
    runReaderT a c

build :: [ToolDef] -> IO Agent
build tools = do
    Just apiKey <- (pack <$>) <$> lookupEnv "ANTHROPIC_API_KEY"
    return Agent { apiKey, tools }

invoke :: MonadClaude m => [Req.Message] -> m Req.Message
invoke messages = do
    Agent { apiKey, tools } <- ask
    let request = Req.fromMessages messages tools
    let asJson = encode $ toJSON request
    liftIO $ L.writeFile "request.json" asJson
    body <- runReq defaultHttpConfig $ do
        response <- req 
            POST
            (https "api.anthropic.com" /: "v1" /: "messages")
            (ReqBodyJson request)
            jsonResponse 
            (headers apiKey)
        pure (responseBody response :: Resp.Resp)
    case Resp.response body of
        Resp.Text t ->  return (Req.assistant t) -- Claude's turn ends
        Resp.Tool toolUse -> do 
            toolAns <- useTool toolUse
            let useMsg = Req.toolUse toolUse
            let ansMsg = case toolAns of 
                    Right answer -> Req.toolResult False (Resp.id toolUse) answer
                    Left errMsg -> Req.toolResult True (Resp.id toolUse) errMsg
            invoke (ansMsg:useMsg:messages)

useTool :: (MonadClaude m) => Resp.ToolUse -> m (Either Text Text)
useTool Resp.ToolUse { Resp.name, Resp.input } = do
    tool <- findTool name
    let msg = "Tool " <> show name <> "(" <> show input <> ")"
    liftIO $ putStrLn msg
    liftIO $ function tool input

findTool :: (MonadClaude m) => Text -> m ToolDef
findTool toolName = do
    t <- asks tools
    find t
    where
        find [] = fail ""
        find (tool:tools)
            | name tool == toolName = return tool
            | otherwise = find tools

headers :: ByteString -> Option a
headers key = 
    header "x-api-key" key
    <> header "anthropic-version" "2023-06-01"
    <> header "content-type" "application/json"

