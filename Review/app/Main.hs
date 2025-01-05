{-# LANGUAGE OverloadedStrings #-}

import Network.Wai (Application, responseLBS, requestMethod, rawPathInfo) -- responseFile
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status302)
import Network.Wai.Parse (parseRequestBody, lbsBackEnd)
import Paths_Review (getDataFileName)

import Database.SQLite.Simple
    ( Only(fromOnly), open, execute_, execute, query_, close)
import Text.Mustache
import Data.Aeson (object, (.=))
import qualified Data.Text.Lazy.Encoding as LE
import qualified Data.Text.Encoding as E
import Data.Text (unpack)

app :: Application
-- app _ respond = respond $ responseLBS status200 [("Content-Type", "text/plain")] "Hello, World!"
app request respond = case (requestMethod request, rawPathInfo request) of
    ("POST", _) -> do
        (params, _) <- parseRequestBody lbsBackEnd request

        let title = fmap E.decodeUtf8 (lookup "title" params)
        let author = fmap E.decodeUtf8 (lookup "author" params)
        case (title, author) of
            (Just t, Just a) -> do 
                addBook (unpack t) (unpack a)
            (_, _) -> do 
                pure ()
        
        respond $ responseLBS status302 [("Location", "http:")] ""

    (_, _) -> do
        filename <- getDataFileName "static/home.mustache"
        template <- compileMustacheFile filename
        books <- getBooks
        let bookObj = map (\title -> object ["book" .= title]) books
        let templateData = object ["books" .= bookObj]  
        let renderedHTML = renderMustache template templateData
        respond $ responseLBS status200 [("Content-Type", "text/html")] (LE.encodeUtf8 renderedHTML)


--     -- respond $ responseFile status200 [("Content-Type", "text/html")] filename Nothing

-- Main function to start the server
main :: IO ()
main = do
    putStrLn "Starting server on http://127.0.0.1:8080"
    run 8080 app

getBooks :: IO [String]
getBooks = do
    let dbName = "data/books.db"
    database <- open dbName
    execute_ database "CREATE TABLE IF NOT EXISTS books (id INTEGER PRIMARY KEY AUTOINCREMENT, title TEXT NOT NULL, author TEXT NOT NULL);"
    strs <- query_ database "SELECT title FROM books" :: IO [Only String]
    close database
    return (map fromOnly strs)

addBook :: String -> String -> IO ()
addBook title author = do
    let dbName = "data/books.db"
    database <- open dbName
    execute_ database "CREATE TABLE IF NOT EXISTS books (id INTEGER PRIMARY KEY AUTOINCREMENT, title TEXT NOT NULL, author TEXT NOT NULL);"
    execute database "INSERT INTO books(title, author) values(?, ?);" (title, author)
    close database