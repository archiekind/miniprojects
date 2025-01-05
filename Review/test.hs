{-# LANGUAGE OverloadedStrings #-}

import Database.SQLite.Simple

-- getBooks :: IO [String]
-- getBooks = do
--     let dbName = "data/books.db"
--     database <- open dbName
--     execute_ database "CREATE TABLE IF NOT EXISTS books (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL, author NOT NULL);"
--     strs <- query_ database "SELECT title FROM books" :: IO [Only String]
--     close database
--     return (map fromOnly strs)

addBook :: String -> String -> IO ()
addBook title author = do
    let dbName = "data/books.db"
    database <- open dbName
    execute_ database "CREATE TABLE IF NOT EXISTS books (id INTEGER PRIMARY KEY AUTOINCREMENT, title TEXT NOT NULL, author TEXT NOT NULL);"
    execute database "INSERT INTO books(title, author) values(?, ?);" (title, author)
    close database
    putStrLn "data inserted successfully"