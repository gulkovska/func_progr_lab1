{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Exception (SomeException, displayException, finally, try)
import Database.MySQL.Simple (Connection, close)
import System.Exit (exitFailure)

import Newspaper.Cli (runCli)
import Newspaper.Db (connectDb, initializeDatabase, loadDbConfig)

main :: IO ()
main = do
    config <- loadDbConfig
    connected <- try (connectDb config) :: IO (Either SomeException Connection)
    case connected of
        Left err -> do
            putStrLn "Не вдалося підключитися до MySQL."
            putStrLn ("Причина: " ++ displayException err)
            putStrLn "Перевірте, чи запущено сервер MySQL, та змінні середовища:"
            putStrLn "  NEWSPAPER_DB_HOST, NEWSPAPER_DB_PORT, NEWSPAPER_DB_USER,"
            putStrLn "  NEWSPAPER_DB_PASSWORD, NEWSPAPER_DB_NAME"
            exitFailure
        Right connection -> do
            (initializeDatabase connection >> runCli connection)
                `finally` close connection
