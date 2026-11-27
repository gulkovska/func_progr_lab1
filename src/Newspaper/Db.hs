{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Підключення до MySQL, створення схеми та початкове наповнення.
module Newspaper.Db
    ( DbConfig(..)
    , loadDbConfig
    , connectDb
    , initializeDatabase
    , ensureSchema
    , seedIfEmpty
    ) where

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Time (fromGregorian, getCurrentTime)
import Database.MySQL.Simple
import qualified Database.MySQL.Base as MySQL
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Newspaper.Entity
import Newspaper.Types

-- | Параметри підключення (можна перевизначити змінними середовища).
data DbConfig = DbConfig
    { dbHost     :: String
    , dbPort     :: Word
    , dbUser     :: String
    , dbPassword :: String
    , dbName     :: String
    } deriving (Show)

loadDbConfig :: IO DbConfig
loadDbConfig = do
    host <- envOr "NEWSPAPER_DB_HOST" "localhost"
    portText <- envOr "NEWSPAPER_DB_PORT" "3306"
    user <- envOr "NEWSPAPER_DB_USER" "root"
    password <- envOr "NEWSPAPER_DB_PASSWORD" ""
    name <- envOr "NEWSPAPER_DB_NAME" "faculty_newspaper"
    pure DbConfig
        { dbHost = host
        , dbPort = fromMaybe 3306 (readMaybe portText)
        , dbUser = user
        , dbPassword = password
        , dbName = name
        }
  where
    envOr key fallback = fromMaybe fallback <$> lookupEnv key

-- | Підключитися до сервера, створити базу (якщо потрібно) і вибрати її.
connectDb :: DbConfig -> IO Connection
connectDb cfg = do
    conn <- connect defaultConnectInfo
        { connectHost = dbHost cfg
        , connectPort = fromIntegral (dbPort cfg)
        , connectUser = dbUser cfg
        , connectPassword = dbPassword cfg
        , connectDatabase = ""
        }
    MySQL.setCharacterSet conn "utf8mb4"
    _ <- execute_ conn (fromString $
        "CREATE DATABASE IF NOT EXISTS `" ++ dbName cfg
        ++ "` CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci")
    MySQL.selectDB conn (dbName cfg)
    pure conn

-- | Створити всі таблиці (idempotent).
initializeDatabase :: Connection -> IO ()
initializeDatabase conn = do
    ensureSchema conn
    seedIfEmpty conn

ensureSchema :: Connection -> IO ()
ensureSchema conn = mapM_ (execute_ conn) schemaStatements

schemaStatements :: [Query]
schemaStatements =
    [ "CREATE TABLE IF NOT EXISTS authors (\
      \ id INT NOT NULL AUTO_INCREMENT,\
      \ name VARCHAR(120) NOT NULL,\
      \ email VARCHAR(160) NOT NULL,\
      \ department VARCHAR(160) NOT NULL,\
      \ PRIMARY KEY (id)\
      \ ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci"

    , "CREATE TABLE IF NOT EXISTS categories (\
      \ id INT NOT NULL AUTO_INCREMENT,\
      \ name VARCHAR(120) NOT NULL,\
      \ description VARCHAR(400) NOT NULL DEFAULT '',\
      \ PRIMARY KEY (id),\
      \ UNIQUE KEY uq_categories_name (name)\
      \ ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci"

    , "CREATE TABLE IF NOT EXISTS articles (\
      \ id INT NOT NULL AUTO_INCREMENT,\
      \ title VARCHAR(255) NOT NULL,\
      \ material_type ENUM('article','graphic','video') NOT NULL DEFAULT 'article',\
      \ abstract_text TEXT NOT NULL,\
      \ author_id INT NOT NULL,\
      \ category_id INT NOT NULL,\
      \ published_at DATE NOT NULL,\
      \ PRIMARY KEY (id),\
      \ KEY idx_articles_author (author_id),\
      \ KEY idx_articles_category (category_id),\
      \ KEY idx_articles_published (published_at),\
      \ CONSTRAINT fk_articles_author FOREIGN KEY (author_id)\
      \   REFERENCES authors (id) ON DELETE RESTRICT ON UPDATE CASCADE,\
      \ CONSTRAINT fk_articles_category FOREIGN KEY (category_id)\
      \   REFERENCES categories (id) ON DELETE RESTRICT ON UPDATE CASCADE\
      \ ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci"

    , "CREATE TABLE IF NOT EXISTS materials (\
      \ id INT NOT NULL AUTO_INCREMENT,\
      \ article_id INT NOT NULL,\
      \ file_path VARCHAR(512) NOT NULL,\
      \ file_type VARCHAR(40) NOT NULL,\
      \ size_kb INT NOT NULL DEFAULT 0,\
      \ PRIMARY KEY (id),\
      \ KEY idx_materials_article (article_id),\
      \ CONSTRAINT fk_materials_article FOREIGN KEY (article_id)\
      \   REFERENCES articles (id) ON DELETE CASCADE ON UPDATE CASCADE\
      \ ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci"

    , "CREATE TABLE IF NOT EXISTS readers (\
      \ id INT NOT NULL AUTO_INCREMENT,\
      \ name VARCHAR(120) NOT NULL,\
      \ email VARCHAR(160) NOT NULL,\
      \ faculty VARCHAR(160) NOT NULL,\
      \ PRIMARY KEY (id)\
      \ ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci"

    , "CREATE TABLE IF NOT EXISTS reviews (\
      \ id INT NOT NULL AUTO_INCREMENT,\
      \ article_id INT NOT NULL,\
      \ reader_id INT NOT NULL,\
      \ rating TINYINT NOT NULL,\
      \ comment TEXT NULL,\
      \ created_at DATETIME NOT NULL,\
      \ PRIMARY KEY (id),\
      \ KEY idx_reviews_article (article_id),\
      \ KEY idx_reviews_reader (reader_id),\
      \ CONSTRAINT fk_reviews_article FOREIGN KEY (article_id)\
      \   REFERENCES articles (id) ON DELETE CASCADE ON UPDATE CASCADE,\
      \ CONSTRAINT fk_reviews_reader FOREIGN KEY (reader_id)\
      \   REFERENCES readers (id) ON DELETE CASCADE ON UPDATE CASCADE\
      \ ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci"

    , "CREATE TABLE IF NOT EXISTS statistics (\
      \ id INT NOT NULL AUTO_INCREMENT,\
      \ article_id INT NOT NULL,\
      \ views INT NOT NULL DEFAULT 0,\
      \ likes INT NOT NULL DEFAULT 0,\
      \ PRIMARY KEY (id),\
      \ UNIQUE KEY uq_statistics_article (article_id),\
      \ CONSTRAINT fk_statistics_article FOREIGN KEY (article_id)\
      \   REFERENCES articles (id) ON DELETE CASCADE ON UPDATE CASCADE\
      \ ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci"
    ]

-- | Додати демонстраційні дані, якщо таблиця авторів порожня.
seedIfEmpty :: Connection -> IO ()
seedIfEmpty conn = do
    [Only count] <- query_ conn "SELECT COUNT(*) FROM authors" :: IO [Only Int]
    when (count == 0) $ do
        -- Автори
        author1 <- create conn (Author Nothing
            "Ірина Ковальчук" "kovalchuk@knu.ua" "Кафедра журналістики")
        author2 <- create conn (Author Nothing
            "Ігор Мельник" "melnyk@knu.ua" "Кафедра комп'ютерних наук")
        author3 <- create conn (Author Nothing
            "Марія Савчук" "savchuk@knu.ua" "Кафедра дизайну")

        -- Категорії
        cat1 <- create conn (Category Nothing
            "Навчання" "Новини навчального процесу та студентського життя")
        cat2 <- create conn (Category Nothing
            "Наука" "Наукові досягнення, олімпіади та конференції")
        cat3 <- create conn (Category Nothing
            "Студентське життя" "Події, ініціативи та творчі проєкти")
        cat4 <- create conn (Category Nothing
            "Культура" "Фестивалі, виставки та мистецькі матеріали")

        -- Публікації
        article1 <- create conn (Article Nothing
            "Як пройшов перший тиждень навчання"
            TextMaterial
            "Огляд подій першого тижня, знайомство з викладачами та студентськими організаціями."
            author1 cat1 (fromGregorian 2026 9 12))
        article2 <- create conn (Article Nothing
            "Перемога на олімпіаді з програмування"
            TextMaterial
            "Команда факультету здобула перше місце на регіональній олімпіаді з алгоритмічного програмування."
            author2 cat2 (fromGregorian 2026 10 3))
        article3 <- create conn (Article Nothing
            "Фестиваль студентської творчості"
            GraphicMaterial
            "Фотозвіт із щорічного фестивалю, де студенти представили музичні, театральні та художні роботи."
            author3 cat3 (fromGregorian 2026 10 20))
        article4 <- create conn (Article Nothing
            "Екскурсія до лабораторії робототехніки"
            VideoMaterial
            "Відеорепортаж про відкриту лабораторію, нові роботизовані проєкти та можливості для студентів."
            author2 cat1 (fromGregorian 2026 11 1))
        article5 <- create conn (Article Nothing
            "Науковий семінар кафедри"
            TextMaterial
            "Анонс і підсумки семінару з сучасних методів аналізу даних та машинного навчання."
            author1 cat2 (fromGregorian 2026 11 15))
        article6 <- create conn (Article Nothing
            "Виставка студентських робіт"
            GraphicMaterial
            "Фотогалерея робіт студентів кафедри дизайну: плакати, ілюстрації та концепт-арт."
            author3 cat4 (fromGregorian 2026 11 22))

        -- Файли публікацій
        _ <- create conn (Material Nothing article1
            "/var/www/newspaper/files/2026/09/classroom.html" "html" 18)
        _ <- create conn (Material Nothing article1
            "/var/www/newspaper/files/2026/09/classroom.jpg" "jpg" 420)
        _ <- create conn (Material Nothing article2
            "/var/www/newspaper/files/2026/10/olympiad.pdf" "pdf" 260)
        _ <- create conn (Material Nothing article3
            "/var/www/newspaper/files/2026/10/festival-01.jpg" "jpg" 890)
        _ <- create conn (Material Nothing article3
            "/var/www/newspaper/files/2026/10/festival-02.jpg" "jpg" 910)
        _ <- create conn (Material Nothing article4
            "/var/www/newspaper/files/2026/11/lab-tour.mp4" "mp4" 15400)
        _ <- create conn (Material Nothing article5
            "/var/www/newspaper/files/2026/11/seminar-slides.pdf" "pdf" 1200)
        _ <- create conn (Material Nothing article6
            "/var/www/newspaper/files/2026/11/exhibition.jpg" "jpg" 760)

        -- Читачі
        reader1 <- create conn (Reader Nothing
            "Олена Бондар" "bondar@student.knu.ua" "Факультет інформаційних технологій")
        reader2 <- create conn (Reader Nothing
            "Андрій Ткаченко" "tkachenko@student.knu.ua" "Факультет комп'ютерних наук")
        reader3 <- create conn (Reader Nothing
            "Софія Лисенко" "lysenko@student.knu.ua" "Факультет комп'ютерних наук")
        reader4 <- create conn (Reader Nothing
            "Дмитро Гнатюк" "gnatyuk@student.knu.ua" "Факультет журналістики")

        -- Відгуки
        now <- getCurrentTime
        _ <- create conn (Review Nothing article1 reader2 9
            (Just "Дуже корисний матеріал, дякую!") now)
        _ <- create conn (Review Nothing article2 reader3 10
            (Just "Неймовірно! Пишаємося нашими студентами.") now)
        _ <- create conn (Review Nothing article3 reader4 8
            (Just "Яскраві фото, атмосфера свята.") now)
        _ <- create conn (Review Nothing article1 reader1 7 Nothing now)
        _ <- create conn (Review Nothing article5 reader2 9
            (Just "Цікаві доповіді та жваве обговорення.") now)

        -- Статистика
        _ <- create conn (Statistic Nothing article1 152 24)
        _ <- create conn (Statistic Nothing article2 231 57)
        _ <- create conn (Statistic Nothing article3 98 31)
        _ <- create conn (Statistic Nothing article4 310 66)
        _ <- create conn (Statistic Nothing article5 45 12)
        _ <- create conn (Statistic Nothing article6 120 19)

        putStrLn "Демонстраційні дані успішно додано."
