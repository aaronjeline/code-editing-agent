{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Req
import Req (Message(..), ToolDef(..))
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

agentMain :: MonadClaude m => [Message] -> m ()
agentMain conversation = do
    input <- getInput
    case input of
        Nothing -> return ()
        Just input -> do
            let msg = Req.user input
            let c = msg : conversation
            resp <- invoke c
            printMessage resp
            agentMain (resp : c)
            
printMessage :: MonadClaude m => Message -> m ()
printMessage m = liftIO $ do 
    putStrLn $ "Claude: " <> show (content m)
    
getInput :: MonadClaude m => m (Maybe T.Text)
getInput = liftIO $ do
    TIO.putStr "> "
    hFlush stdout
    catch 
        (Just <$> TIO.hGetLine stdin) 
        (\e -> if isEOFError e then return Nothing else ioError e)
