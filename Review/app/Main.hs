{-# LANGUAGE OverloadedStrings #-}

import Network.Wai (Application, responseLBS, responseFile)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Paths_Review (getDataFileName)

import Database.SQLite.Simple
import Text.Mustache (localAutomaticCompile)

data Book = MkBook String

app :: Application
-- app _ respond = respond $ responseLBS status200 [("Content-Type", "text/plain")] "Hello, World!"
app request respond = do
    filename <- getDataFileName "static/home.mustache"
    -- template <- filename
    let searchspace = ['.']
    books <- getBooks
    let bookList = map (\name -> MkBook name)
    let templateData = object ["books" .= bookList]
    let renderedHTML = renderMustache template templateData
    respond $ responseLBS status200 [("Content-Type", "text/html")] renderedHTML
    -- respond $ responseLBS status200 [("Content-Type", "text/plain")] "Hello World"
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