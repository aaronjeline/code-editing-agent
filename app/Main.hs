{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Req
import Req (Message(..))
import Claude
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout, stdin)
import System.IO.Error
import Control.Monad.IO.Class
import Control.Exception (catch)
import Tools

main :: IO ()
main =  do
    BS.putStrLn "Chat with claude!"
    runClaude tools $ agentMain []

-- Entrypoint for our agent. Runs in a loop taking turns
agentMain :: MonadClaude m => [Message] -> m ()
agentMain conversation = do
    maybeInput <- getInput
    case maybeInput of
        Nothing -> return () -- If we're out of input, exit the loop
        Just input -> do
            let msg = Req.user input -- Wrap the text input as a user message
            -- Add the message to the conversation and invoke the model
            resp <- invoke (msg : conversation) 
            printMessage resp
            -- Add both the message and the model's response, and loop
            agentMain (resp : msg : conversation) 
            
printMessage :: MonadClaude m => Message -> m ()
printMessage m = liftIO $ do 
    putStrLn $ "Claude: " <> show (content m)
    
-- Read a line of input from stdin, returning `Nothing` if the stream is empty
getInput :: MonadClaude m => m (Maybe T.Text)
getInput = liftIO $ do
    TIO.putStr "> "
    hFlush stdout
    catch 
        (Just <$> TIO.hGetLine stdin) 
        (\e -> if isEOFError e then return Nothing else ioError e)
