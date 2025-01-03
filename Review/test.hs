{-# LANGUAGE OverloadedStrings #-}

import Database.SQLite.Simple

getBooks :: IO [String]
getBooks = do
    let dbName = "data/books.db"
    database <- open dbName
    execute_ database "CREATE TABLE IF NOT EXISTS book (name VARCHAR(40) PRIMARY KEY)"
    strs <- query_ database "SELECT * FROM book" :: IO [Only String]
    close database
    return (map fromOnly strs)