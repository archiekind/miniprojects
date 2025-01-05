{-# LANGUAGE OverloadedStrings #-}

import Network.Wai (Application, responseLBS) -- responseFile
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Paths_Review (getDataFileName)

import Database.SQLite.Simple
import Text.Mustache
import Data.Aeson (object, (.=))
import Data.Text.Lazy.Encoding (encodeUtf8)

app :: Application
-- app _ respond = respond $ responseLBS status200 [("Content-Type", "text/plain")] "Hello, World!"
app _ respond = do
    filename <- getDataFileName "static/home.mustache"
    template <- compileMustacheFile filename
    books <- getBooks
    let bookObj = map (\title -> object ["book" .= title]) books
    let templateData = object ["books" .= bookObj]  
    let renderedHTML = renderMustache template templateData
    respond $ responseLBS status200 [("Content-Type", "text/html")] (encodeUtf8 renderedHTML)
    -- respond $ responseFile status200 [("Content-Type", "text/html")] filename Nothing

-- Main function to start the server
main :: IO ()
main = do
    putStrLn "Starting server on http://127.0.0.1:8080"
    run 8080 app

getBooks :: IO [String]
getBooks = do
    let dbName = "data/books.db"
    database <- open dbName
    execute_ database "CREATE TABLE IF NOT EXISTS books (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL, author NOT NULL);"
    strs <- query_ database "SELECT title FROM books" :: IO [Only String]
    close database
    return (map fromOnly strs)