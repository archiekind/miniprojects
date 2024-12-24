{-# LANGUAGE OverloadedStrings #-}

import Network.Wai (Application, responseLBS, responseFile)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Paths_Server (getDataFileName)

app :: Application
-- app _ respond = respond $ responseLBS status200 [("Content-Type", "text/plain")] "Hello, World!"
app _ respond = do
    filename <- getDataFileName "static/home.html"
    respond $ responseFile status200 [("Content-Type", "text/html")] filename Nothing

-- Main function to start the server
main :: IO ()
main = do
    putStrLn "Starting server on http://127.0.0.1:8080"
    run 8080 app