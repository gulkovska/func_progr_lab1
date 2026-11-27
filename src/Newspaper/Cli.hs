{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Інтерактивний консольний інтерфейс інформаційної системи.
module Newspaper.Cli
    ( runCli
    ) where

import Control.Exception (SomeException, displayException, fromException,
                          throwIO, try)
import Control.Monad (forM_, when)
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day, UTCTime, defaultTimeLocale, formatTime, getCurrentTime,
                  parseTimeM, utctDay)
import Database.MySQL.Simple (Connection)
import System.Exit (ExitCode, exitSuccess)
import System.IO (hFlush, isEOF, stdout)
import Text.Read (readMaybe)

import Newspaper.Entity
import Newspaper.Queries
import Newspaper.Types

--------------------------------------------------------------------------------
-- Допоміжні функції вводу/виводу
--------------------------------------------------------------------------------

runCli :: Connection -> IO ()
runCli conn = do
    putStrLn banner
    mainMenu conn

banner :: String
banner = unlines
    [ ""
    , "=================================================="
    , "   МЕРЕЖЕВА ГАЗЕТА ФАКУЛЬТЕТУ"
    , "   Інформаційна система на Haskell + MySQL"
    , "=================================================="
    ]

prompt :: String -> IO String
prompt label = do
    putStr (label ++ ": ")
    hFlush stdout
    eof <- isEOF
    if eof
        then do
            putStrLn ""
            exitSuccess
        else getLine

promptDef :: String -> String -> IO String
promptDef label def = do
    value <- prompt (label ++ " [" ++ def ++ "]")
    pure (if null value then def else value)

promptInt :: String -> IO Int
promptInt label = do
    raw <- prompt label
    case readMaybe raw of
        Just number -> pure number
        Nothing -> do
            putStrLn "  Потрібне ціле число."
            promptInt label

promptIntDef :: String -> Int -> IO Int
promptIntDef label def = do
    raw <- prompt (label ++ " [" ++ show def ++ "]")
    if null raw
        then pure def
        else case readMaybe raw of
            Just number -> pure number
            Nothing -> do
                putStrLn "  Потрібне ціле число."
                promptIntDef label def

promptRating :: String -> Int -> IO Int
promptRating label def = do
    number <- promptIntDef label def
    if number >= 1 && number <= 10
        then pure number
        else do
            putStrLn "  Оцінка має бути від 1 до 10."
            promptRating label def

promptDay :: String -> Day -> IO Day
promptDay label def = do
    let defText = formatTime defaultTimeLocale "%Y-%m-%d" def
    raw <- prompt (label ++ " [" ++ defText ++ "]")
    if null raw
        then pure def
        else case parseTimeM True defaultTimeLocale "%Y-%m-%d" raw of
            Just day -> pure day
            Nothing -> do
                putStrLn "  Формат дати: РРРР-ММ-ДД."
                promptDay label def

promptMaybeText :: String -> IO (Maybe Text)
promptMaybeText label = do
    raw <- prompt (label ++ " (порожньо — без коментаря)")
    pure (if null raw then Nothing else Just (T.pack raw))

promptMaterialType :: MaterialType -> IO MaterialType
promptMaterialType def = do
    raw <- prompt ("Тип (1-стаття, 2-графіка, 3-відео) ["
                   ++ T.unpack (materialTypeLabel def) ++ "]")
    if null raw
        then pure def
        else case parseMaterialType raw of
            Just value -> pure value
            Nothing -> do
                putStrLn "  Невірний тип. Введіть 1, 2 або 3."
                promptMaterialType def

confirm :: String -> IO Bool
confirm label = do
    raw <- prompt (label ++ " (y/n)")
    pure (map toLower raw `elem` ["y", "yes", "т", "так"])

safe :: IO () -> IO ()
safe action = do
    result <- try action
    case result of
        Left (err :: SomeException) ->
            case fromException err :: Maybe ExitCode of
                Just exitCode -> throwIO exitCode
                Nothing -> putStrLn ("\n  Помилка: " ++ displayException err)
        Right () -> pure ()

short :: Int -> Text -> String
short width text
    | T.length text <= width = T.unpack text
    | otherwise = T.unpack (T.take (width - 3) text) ++ "..."

showMaybeId :: Maybe Int -> String
showMaybeId = maybe "—" show

showComment :: Maybe Text -> String
showComment = maybe "—" (short 45)

formatDay :: Day -> String
formatDay = formatTime defaultTimeLocale "%Y-%m-%d"

formatDateTime :: UTCTime -> String
formatDateTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M"

printHeader :: [String] -> IO ()
printHeader columns = do
    putStrLn (intercalate " | " columns)
    putStrLn (replicate (length (intercalate " | " columns)) '-')

printRow :: [String] -> IO ()
printRow = putStrLn . intercalate " | "

--------------------------------------------------------------------------------
-- Головне меню
--------------------------------------------------------------------------------

mainMenu :: Connection -> IO ()
mainMenu conn = do
    putStrLn "\n--- ГОЛОВНЕ МЕНЮ ---"
    putStrLn "1. Статті та матеріали"
    putStrLn "2. Автори"
    putStrLn "3. Категорії"
    putStrLn "4. Файли публікацій"
    putStrLn "5. Читачі"
    putStrLn "6. Відгуки"
    putStrLn "7. Статистика"
    putStrLn "8. Звіти"
    putStrLn "0. Вихід"
    choice <- prompt "Ваш вибір"
    case choice of
        "1" -> articlesMenu conn >> mainMenu conn
        "2" -> authorsMenu conn >> mainMenu conn
        "3" -> categoriesMenu conn >> mainMenu conn
        "4" -> materialsMenu conn >> mainMenu conn
        "5" -> readersMenu conn >> mainMenu conn
        "6" -> reviewsMenu conn >> mainMenu conn
        "7" -> statisticsMenu conn >> mainMenu conn
        "8" -> reportsMenu conn >> mainMenu conn
        "0" -> putStrLn "До побачення!"
        _   -> putStrLn "Невірний пункт." >> mainMenu conn

--------------------------------------------------------------------------------
-- Автори
--------------------------------------------------------------------------------

authorsMenu :: Connection -> IO ()
authorsMenu conn = do
    putStrLn "\n--- АВТОРИ ---"
    putStrLn "1. Список авторів"
    putStrLn "2. Додати автора"
    putStrLn "3. Редагувати автора"
    putStrLn "4. Видалити автора"
    putStrLn "0. Назад"
    choice <- prompt "Ваш вибір"
    case choice of
        "1" -> safe (showAuthors conn) >> authorsMenu conn
        "2" -> safe (addAuthor conn) >> authorsMenu conn
        "3" -> safe (editAuthor conn) >> authorsMenu conn
        "4" -> safe (deleteAuthor conn) >> authorsMenu conn
        "0" -> pure ()
        _   -> putStrLn "Невірний пункт." >> authorsMenu conn

showAuthors :: Connection -> IO ()
showAuthors conn = do
    authors <- listAll conn :: IO [Author]
    if null authors
        then putStrLn "Авторів немає."
        else do
            printHeader ["ID", "Ім'я", "Email", "Кафедра"]
            forM_ authors $ \author ->
                printRow [ showMaybeId (authorId author)
                         , T.unpack (authorName author)
                         , T.unpack (authorEmail author)
                         , T.unpack (authorDepartment author)
                         ]

listAuthorsBrief :: Connection -> IO ()
listAuthorsBrief conn = do
    authors <- listAll conn :: IO [Author]
    forM_ authors $ \author ->
        putStrLn ("  " ++ showMaybeId (authorId author) ++ ". "
                  ++ T.unpack (authorName author))

addAuthor :: Connection -> IO ()
addAuthor conn = do
    name <- prompt "Ім'я"
    email <- prompt "Email"
    department <- prompt "Кафедра"
    ident <- create conn (Author Nothing (T.pack name) (T.pack email) (T.pack department))
    putStrLn ("Додано автора з ID " ++ show ident ++ ".")

editAuthor :: Connection -> IO ()
editAuthor conn = do
    ident <- promptInt "ID автора"
    found <- findById conn ident :: IO (Maybe Author)
    case found of
        Nothing -> putStrLn "Автора не знайдено."
        Just author -> do
            name <- promptDef "Ім'я" (T.unpack (authorName author))
            email <- promptDef "Email" (T.unpack (authorEmail author))
            department <- promptDef "Кафедра" (T.unpack (authorDepartment author))
            changed <- updateEntity conn author
                { authorName = T.pack name
                , authorEmail = T.pack email
                , authorDepartment = T.pack department
                }
            putStrLn (if changed then "Автора оновлено." else "Нічого не змінено.")

deleteAuthor :: Connection -> IO ()
deleteAuthor conn = do
    ident <- promptInt "ID автора"
    found <- findById conn ident :: IO (Maybe Author)
    case found of
        Nothing -> putStrLn "Автора не знайдено."
        Just author -> do
            approved <- confirm ("Видалити автора \"" ++ T.unpack (authorName author) ++ "\"?")
            when approved $ do
                deleted <- deleteEntity conn author
                putStrLn (if deleted then "Автора видалено." else "Не вдалося видалити.")

--------------------------------------------------------------------------------
-- Категорії
--------------------------------------------------------------------------------

categoriesMenu :: Connection -> IO ()
categoriesMenu conn = do
    putStrLn "\n--- КАТЕГОРІЇ ---"
    putStrLn "1. Список категорій"
    putStrLn "2. Додати категорію"
    putStrLn "3. Редагувати категорію"
    putStrLn "4. Видалити категорію"
    putStrLn "0. Назад"
    choice <- prompt "Ваш вибір"
    case choice of
        "1" -> safe (showCategories conn) >> categoriesMenu conn
        "2" -> safe (addCategory conn) >> categoriesMenu conn
        "3" -> safe (editCategory conn) >> categoriesMenu conn
        "4" -> safe (deleteCategory conn) >> categoriesMenu conn
        "0" -> pure ()
        _   -> putStrLn "Невірний пункт." >> categoriesMenu conn

showCategories :: Connection -> IO ()
showCategories conn = do
    categories <- listAll conn :: IO [Category]
    if null categories
        then putStrLn "Категорій немає."
        else do
            printHeader ["ID", "Назва", "Опис"]
            forM_ categories $ \category ->
                printRow [ showMaybeId (categoryId category)
                         , T.unpack (categoryName category)
                         , short 60 (categoryDescription category)
                         ]

listCategoriesBrief :: Connection -> IO ()
listCategoriesBrief conn = do
    categories <- listAll conn :: IO [Category]
    forM_ categories $ \category ->
        putStrLn ("  " ++ showMaybeId (categoryId category) ++ ". "
                  ++ T.unpack (categoryName category))

addCategory :: Connection -> IO ()
addCategory conn = do
    name <- prompt "Назва"
    description <- prompt "Опис"
    ident <- create conn (Category Nothing (T.pack name) (T.pack description))
    putStrLn ("Додано категорію з ID " ++ show ident ++ ".")

editCategory :: Connection -> IO ()
editCategory conn = do
    ident <- promptInt "ID категорії"
    found <- findById conn ident :: IO (Maybe Category)
    case found of
        Nothing -> putStrLn "Категорію не знайдено."
        Just category -> do
            name <- promptDef "Назва" (T.unpack (categoryName category))
            description <- promptDef "Опис" (T.unpack (categoryDescription category))
            changed <- updateEntity conn category
                { categoryName = T.pack name
                , categoryDescription = T.pack description
                }
            putStrLn (if changed then "Категорію оновлено." else "Нічого не змінено.")

deleteCategory :: Connection -> IO ()
deleteCategory conn = do
    ident <- promptInt "ID категорії"
    found <- findById conn ident :: IO (Maybe Category)
    case found of
        Nothing -> putStrLn "Категорію не знайдено."
        Just category -> do
            approved <- confirm ("Видалити категорію \"" ++ T.unpack (categoryName category) ++ "\"?")
            when approved $ do
                deleted <- deleteEntity conn category
                putStrLn (if deleted then "Категорію видалено." else "Не вдалося видалити.")

--------------------------------------------------------------------------------
-- Статті та матеріали
--------------------------------------------------------------------------------

articlesMenu :: Connection -> IO ()
articlesMenu conn = do
    putStrLn "\n--- СТАТТІ ТА МАТЕРІАЛИ ---"
    putStrLn "1. Список статей"
    putStrLn "2. Детальна інформація про статтю"
    putStrLn "3. Додати статтю"
    putStrLn "4. Редагувати статтю"
    putStrLn "5. Видалити статтю"
    putStrLn "6. Пошук за назвою/анотацією"
    putStrLn "0. Назад"
    choice <- prompt "Ваш вибір"
    case choice of
        "1" -> safe (showArticles conn) >> articlesMenu conn
        "2" -> safe (showArticleDetailsPrompt conn) >> articlesMenu conn
        "3" -> safe (addArticle conn) >> articlesMenu conn
        "4" -> safe (editArticle conn) >> articlesMenu conn
        "5" -> safe (deleteArticle conn) >> articlesMenu conn
        "6" -> safe (searchArticlesPrompt conn) >> articlesMenu conn
        "0" -> pure ()
        _   -> putStrLn "Невірний пункт." >> articlesMenu conn

printArticleRows :: [ArticleRow] -> IO ()
printArticleRows rows = do
    printHeader ["ID", "Назва", "Тип", "Автор", "Категорія", "Дата", "Файлів", "Перегл.", "Вподоб."]
    forM_ rows $ \(ident, title, mtype, _abstract, author, category, day, files, views, likes) ->
        printRow [ show ident
                 , short 40 title
                 , T.unpack (materialTypeLabel (fromMaybe TextMaterial (materialTypeFromDb mtype)))
                 , short 20 author
                 , short 20 category
                 , formatDay day
                 , show files
                 , show views
                 , show likes
                 ]

showArticles :: Connection -> IO ()
showArticles conn = do
    rows <- listArticleRows conn
    if null rows then putStrLn "Статей немає." else printArticleRows rows

showArticleDetailsPrompt :: Connection -> IO ()
showArticleDetailsPrompt conn = do
    ident <- promptInt "ID статті"
    showArticleDetails conn ident

showArticleDetails :: Connection -> Int -> IO ()
showArticleDetails conn ident = do
    found <- findArticleRow conn ident
    case found of
        Nothing -> putStrLn "Статтю не знайдено."
        Just (articleId', title, mtype, abstractText, author, category, day, files, views, likes) -> do
            putStrLn ""
            putStrLn ("=== " ++ T.unpack title ++ " ===")
            putStrLn ("ID: " ++ show articleId')
            putStrLn ("Тип: " ++ T.unpack (materialTypeLabel (fromMaybe TextMaterial (materialTypeFromDb mtype))))
            putStrLn ("Автор: " ++ T.unpack author)
            putStrLn ("Категорія: " ++ T.unpack category)
            putStrLn ("Дата публікації: " ++ formatDay day)
            putStrLn ("Анотація: " ++ T.unpack abstractText)
            putStrLn ("Файлів: " ++ show files ++ "; переглядів: " ++ show views
                      ++ "; вподобань: " ++ show likes)

            putStrLn "\nФайли публікації:"
            materials <- articleMaterials conn articleId'
            if null materials
                then putStrLn "  немає"
                else forM_ materials $ \(materialId', path, fileType, sizeKb) ->
                    putStrLn ("  #" ++ show materialId' ++ " " ++ T.unpack path
                              ++ " (" ++ T.unpack fileType ++ ", " ++ show sizeKb ++ " КБ)")

            putStrLn "\nВідгуки читачів:"
            reviews <- articleReviews conn articleId'
            if null reviews
                then putStrLn "  немає"
                else forM_ reviews $ \(reviewId', reader, rating, comment, createdAt) ->
                    putStrLn ("  #" ++ show reviewId' ++ " " ++ T.unpack reader
                              ++ " — " ++ show rating ++ "/10, "
                              ++ formatDateTime createdAt
                              ++ maybe "" (\text -> ", \"" ++ T.unpack text ++ "\"") comment)

searchArticlesPrompt :: Connection -> IO ()
searchArticlesPrompt conn = do
    needle <- prompt "Пошуковий запит"
    rows <- searchArticles conn (T.pack needle)
    if null rows then putStrLn "Нічого не знайдено." else printArticleRows rows

addArticle :: Connection -> IO ()
addArticle conn = do
    putStrLn "\nДоступні автори:"
    listAuthorsBrief conn
    putStrLn "\nДоступні категорії:"
    listCategoriesBrief conn

    title <- prompt "Назва статті"
    mtype <- promptMaterialType TextMaterial
    abstractText <- prompt "Анотація"
    authorId' <- promptInt "ID автора"
    categoryId' <- promptInt "ID категорії"
    today <- utctDay <$> getCurrentTime
    published <- promptDay "Дата публікації (РРРР-ММ-ДД)" today

    authorFound <- findById conn authorId' :: IO (Maybe Author)
    categoryFound <- findById conn categoryId' :: IO (Maybe Category)
    case (authorFound, categoryFound) of
        (Nothing, _) -> putStrLn "Автора з таким ID не знайдено."
        (_, Nothing) -> putStrLn "Категорію з таким ID не знайдено."
        (Just _, Just _) -> do
            ident <- create conn (Article Nothing (T.pack title) mtype
                                  (T.pack abstractText) authorId' categoryId' published)
            putStrLn ("Статтю додано з ID " ++ show ident ++ ".")

editArticle :: Connection -> IO ()
editArticle conn = do
    ident <- promptInt "ID статті"
    found <- findById conn ident :: IO (Maybe Article)
    case found of
        Nothing -> putStrLn "Статтю не знайдено."
        Just article -> do
            title <- promptDef "Назва" (T.unpack (articleTitle article))
            mtype <- promptMaterialType (articleType article)
            abstractText <- promptDef "Анотація" (T.unpack (articleAbstract article))
            authorId' <- promptIntDef "ID автора" (articleAuthorId article)
            categoryId' <- promptIntDef "ID категорії" (articleCategoryId article)
            published <- promptDay "Дата публікації" (articlePublishedAt article)

            authorFound <- findById conn authorId' :: IO (Maybe Author)
            categoryFound <- findById conn categoryId' :: IO (Maybe Category)
            case (authorFound, categoryFound) of
                (Nothing, _) -> putStrLn "Автора з таким ID не знайдено."
                (_, Nothing) -> putStrLn "Категорію з таким ID не знайдено."
                (Just _, Just _) -> do
                    changed <- updateEntity conn article
                        { articleTitle = T.pack title
                        , articleType = mtype
                        , articleAbstract = T.pack abstractText
                        , articleAuthorId = authorId'
                        , articleCategoryId = categoryId'
                        , articlePublishedAt = published
                        }
                    putStrLn (if changed then "Статтю оновлено." else "Нічого не змінено.")

deleteArticle :: Connection -> IO ()
deleteArticle conn = do
    ident <- promptInt "ID статті"
    found <- findById conn ident :: IO (Maybe Article)
    case found of
        Nothing -> putStrLn "Статтю не знайдено."
        Just article -> do
            approved <- confirm ("Видалити статтю \"" ++ T.unpack (articleTitle article)
                                 ++ "\" разом з файлами, відгуками та статистикою?")
            when approved $ do
                deleted <- deleteEntity conn article
                putStrLn (if deleted then "Статтю видалено." else "Не вдалося видалити.")

--------------------------------------------------------------------------------
-- Файли публікацій
--------------------------------------------------------------------------------

materialsMenu :: Connection -> IO ()
materialsMenu conn = do
    putStrLn "\n--- ФАЙЛИ ПУБЛІКАЦІЙ ---"
    putStrLn "1. Список файлів"
    putStrLn "2. Додати файл"
    putStrLn "3. Редагувати файл"
    putStrLn "4. Видалити файл"
    putStrLn "0. Назад"
    choice <- prompt "Ваш вибір"
    case choice of
        "1" -> safe (showMaterials conn) >> materialsMenu conn
        "2" -> safe (addMaterial conn) >> materialsMenu conn
        "3" -> safe (editMaterial conn) >> materialsMenu conn
        "4" -> safe (deleteMaterial conn) >> materialsMenu conn
        "0" -> pure ()
        _   -> putStrLn "Невірний пункт." >> materialsMenu conn

showMaterials :: Connection -> IO ()
showMaterials conn = do
    rows <- listMaterialsWithArticle conn
    if null rows
        then putStrLn "Файлів немає."
        else do
            printHeader ["ID", "Стаття", "Назва статті", "Шлях", "Тип", "КБ"]
            forM_ rows $ \(ident, articleId', title, path, fileType, sizeKb) ->
                printRow [ show ident
                         , show articleId'
                         , short 28 title
                         , short 55 path
                         , T.unpack fileType
                         , show sizeKb
                         ]

addMaterial :: Connection -> IO ()
addMaterial conn = do
    articleId' <- promptInt "ID статті"
    articleFound <- findById conn articleId' :: IO (Maybe Article)
    case articleFound of
        Nothing -> putStrLn "Статтю не знайдено."
        Just _ -> do
            path <- prompt "Шлях до файлу"
            fileType <- prompt "Тип файлу (html/pdf/jpg/mp4/...)"
            sizeKb <- promptInt "Розмір, КБ"
            ident <- create conn (Material Nothing articleId' (T.pack path)
                                  (T.pack fileType) sizeKb)
            putStrLn ("Файл додано з ID " ++ show ident ++ ".")

editMaterial :: Connection -> IO ()
editMaterial conn = do
    ident <- promptInt "ID файлу"
    found <- findById conn ident :: IO (Maybe Material)
    case found of
        Nothing -> putStrLn "Файл не знайдено."
        Just material -> do
            articleId' <- promptIntDef "ID статті" (materialArticleId material)
            articleFound <- findById conn articleId' :: IO (Maybe Article)
            case articleFound of
                Nothing -> putStrLn "Статтю не знайдено."
                Just _ -> do
                    path <- promptDef "Шлях до файлу" (T.unpack (materialFilePath material))
                    fileType <- promptDef "Тип файлу" (T.unpack (materialFileType material))
                    sizeKb <- promptIntDef "Розмір, КБ" (materialSizeKb material)
                    changed <- updateEntity conn material
                        { materialArticleId = articleId'
                        , materialFilePath = T.pack path
                        , materialFileType = T.pack fileType
                        , materialSizeKb = sizeKb
                        }
                    putStrLn (if changed then "Файл оновлено." else "Нічого не змінено.")

deleteMaterial :: Connection -> IO ()
deleteMaterial conn = do
    ident <- promptInt "ID файлу"
    found <- findById conn ident :: IO (Maybe Material)
    case found of
        Nothing -> putStrLn "Файл не знайдено."
        Just material -> do
            approved <- confirm ("Видалити файл \"" ++ T.unpack (materialFilePath material) ++ "\"?")
            when approved $ do
                deleted <- deleteEntity conn material
                putStrLn (if deleted then "Файл видалено." else "Не вдалося видалити.")

--------------------------------------------------------------------------------
-- Читачі
--------------------------------------------------------------------------------

readersMenu :: Connection -> IO ()
readersMenu conn = do
    putStrLn "\n--- ЧИТАЧІ ---"
    putStrLn "1. Список читачів"
    putStrLn "2. Додати читача"
    putStrLn "3. Редагувати читача"
    putStrLn "4. Видалити читача"
    putStrLn "0. Назад"
    choice <- prompt "Ваш вибір"
    case choice of
        "1" -> safe (showReaders conn) >> readersMenu conn
        "2" -> safe (addReader conn) >> readersMenu conn
        "3" -> safe (editReader conn) >> readersMenu conn
        "4" -> safe (deleteReader conn) >> readersMenu conn
        "0" -> pure ()
        _   -> putStrLn "Невірний пункт." >> readersMenu conn

showReaders :: Connection -> IO ()
showReaders conn = do
    readers <- listAll conn :: IO [Reader]
    if null readers
        then putStrLn "Читачів немає."
        else do
            printHeader ["ID", "Ім'я", "Email", "Факультет"]
            forM_ readers $ \reader ->
                printRow [ showMaybeId (readerId reader)
                         , T.unpack (readerName reader)
                         , T.unpack (readerEmail reader)
                         , T.unpack (readerFaculty reader)
                         ]

listReadersBrief :: Connection -> IO ()
listReadersBrief conn = do
    readers <- listAll conn :: IO [Reader]
    forM_ readers $ \reader ->
        putStrLn ("  " ++ showMaybeId (readerId reader) ++ ". "
                  ++ T.unpack (readerName reader))

addReader :: Connection -> IO ()
addReader conn = do
    name <- prompt "Ім'я"
    email <- prompt "Email"
    faculty <- prompt "Факультет"
    ident <- create conn (Reader Nothing (T.pack name) (T.pack email) (T.pack faculty))
    putStrLn ("Додано читача з ID " ++ show ident ++ ".")

editReader :: Connection -> IO ()
editReader conn = do
    ident <- promptInt "ID читача"
    found <- findById conn ident :: IO (Maybe Reader)
    case found of
        Nothing -> putStrLn "Читача не знайдено."
        Just reader -> do
            name <- promptDef "Ім'я" (T.unpack (readerName reader))
            email <- promptDef "Email" (T.unpack (readerEmail reader))
            faculty <- promptDef "Факультет" (T.unpack (readerFaculty reader))
            changed <- updateEntity conn reader
                { readerName = T.pack name
                , readerEmail = T.pack email
                , readerFaculty = T.pack faculty
                }
            putStrLn (if changed then "Читача оновлено." else "Нічого не змінено.")

deleteReader :: Connection -> IO ()
deleteReader conn = do
    ident <- promptInt "ID читача"
    found <- findById conn ident :: IO (Maybe Reader)
    case found of
        Nothing -> putStrLn "Читача не знайдено."
        Just reader -> do
            approved <- confirm ("Видалити читача \"" ++ T.unpack (readerName reader) ++ "\"?")
            when approved $ do
                deleted <- deleteEntity conn reader
                putStrLn (if deleted then "Читача видалено." else "Не вдалося видалити.")

--------------------------------------------------------------------------------
-- Відгуки
--------------------------------------------------------------------------------

reviewsMenu :: Connection -> IO ()
reviewsMenu conn = do
    putStrLn "\n--- ВІДГУКИ ---"
    putStrLn "1. Список відгуків"
    putStrLn "2. Додати відгук"
    putStrLn "3. Редагувати відгук"
    putStrLn "4. Видалити відгук"
    putStrLn "0. Назад"
    choice <- prompt "Ваш вибір"
    case choice of
        "1" -> safe (showReviews conn) >> reviewsMenu conn
        "2" -> safe (addReview conn) >> reviewsMenu conn
        "3" -> safe (editReview conn) >> reviewsMenu conn
        "4" -> safe (deleteReview conn) >> reviewsMenu conn
        "0" -> pure ()
        _   -> putStrLn "Невірний пункт." >> reviewsMenu conn

showReviews :: Connection -> IO ()
showReviews conn = do
    rows <- listReviewsWithNames conn
    if null rows
        then putStrLn "Відгуків немає."
        else do
            printHeader ["ID", "Стаття", "Назва статті", "Читач", "Оцінка", "Коментар", "Дата"]
            forM_ rows $ \(ident, articleId', title, _readerId, reader, rating, comment, createdAt) ->
                printRow [ show ident
                         , show articleId'
                         , short 25 title
                         , short 18 reader
                         , show rating
                         , showComment comment
                         , formatDateTime createdAt
                         ]

addReview :: Connection -> IO ()
addReview conn = do
    putStrLn "\nДоступні статті:"
    showArticles conn
    putStrLn "\nДоступні читачі:"
    listReadersBrief conn

    articleId' <- promptInt "ID статті"
    readerId' <- promptInt "ID читача"
    rating <- promptRating "Оцінка (1-10)" 10
    comment <- promptMaybeText "Коментар"
    now <- getCurrentTime

    articleFound <- findById conn articleId' :: IO (Maybe Article)
    readerFound <- findById conn readerId' :: IO (Maybe Reader)
    case (articleFound, readerFound) of
        (Nothing, _) -> putStrLn "Статтю не знайдено."
        (_, Nothing) -> putStrLn "Читача не знайдено."
        (Just _, Just _) -> do
            ident <- create conn (Review Nothing articleId' readerId' rating comment now)
            putStrLn ("Відгук додано з ID " ++ show ident ++ ".")

editReview :: Connection -> IO ()
editReview conn = do
    ident <- promptInt "ID відгуку"
    found <- findById conn ident :: IO (Maybe Review)
    case found of
        Nothing -> putStrLn "Відгук не знайдено."
        Just review -> do
            rating <- promptRating "Оцінка (1-10)" (reviewRating review)
            comment <- promptMaybeText "Коментар"
            changed <- updateEntity conn review
                { reviewRating = rating
                , reviewComment = comment
                }
            putStrLn (if changed then "Відгук оновлено." else "Нічого не змінено.")

deleteReview :: Connection -> IO ()
deleteReview conn = do
    ident <- promptInt "ID відгуку"
    found <- findById conn ident :: IO (Maybe Review)
    case found of
        Nothing -> putStrLn "Відгук не знайдено."
        Just review -> do
            approved <- confirm ("Видалити відгук #" ++ show ident ++ "?")
            when approved $ do
                deleted <- deleteEntity conn review
                putStrLn (if deleted then "Відгук видалено." else "Не вдалося видалити.")

--------------------------------------------------------------------------------
-- Статистика
--------------------------------------------------------------------------------

statisticsMenu :: Connection -> IO ()
statisticsMenu conn = do
    putStrLn "\n--- СТАТИСТИКА ---"
    putStrLn "1. Показати статистику"
    putStrLn "2. Додати/оновити статистику для статті"
    putStrLn "3. Видалити запис статистики"
    putStrLn "0. Назад"
    choice <- prompt "Ваш вибір"
    case choice of
        "1" -> safe (showStatistics conn) >> statisticsMenu conn
        "2" -> safe (setStatistic conn) >> statisticsMenu conn
        "3" -> safe (deleteStatistic conn) >> statisticsMenu conn
        "0" -> pure ()
        _   -> putStrLn "Невірний пункт." >> statisticsMenu conn

showStatistics :: Connection -> IO ()
showStatistics conn = do
    rows <- listStatistics conn
    if null rows
        then putStrLn "Статистики немає."
        else do
            printHeader ["ID", "Стаття", "Назва статті", "Перегляди", "Вподобання"]
            forM_ rows $ \(ident, articleId', title, views, likes) ->
                printRow [show ident, show articleId', short 45 title, show views, show likes]

setStatistic :: Connection -> IO ()
setStatistic conn = do
    articleId' <- promptInt "ID статті"
    articleFound <- findById conn articleId' :: IO (Maybe Article)
    case articleFound of
        Nothing -> putStrLn "Статтю не знайдено."
        Just _ -> do
            views <- promptInt "Кількість переглядів"
            likes <- promptInt "Кількість вподобань"
            upsertStatistic conn articleId' views likes
            putStrLn "Статистику збережено."

deleteStatistic :: Connection -> IO ()
deleteStatistic conn = do
    ident <- promptInt "ID запису статистики"
    found <- findById conn ident :: IO (Maybe Statistic)
    case found of
        Nothing -> putStrLn "Запис статистики не знайдено."
        Just statistic -> do
            approved <- confirm ("Видалити запис статистики #" ++ show ident ++ "?")
            when approved $ do
                deleted <- deleteEntity conn statistic
                putStrLn (if deleted then "Запис статистики видалено." else "Не вдалося видалити.")

--------------------------------------------------------------------------------
-- Звіти
--------------------------------------------------------------------------------

reportsMenu :: Connection -> IO ()
reportsMenu conn = do
    putStrLn "\n--- ЗВІТИ ---"
    putStrLn "1. Топ-10 матеріалів за переглядами"
    putStrLn "2. Середній рейтинг статей"
    putStrLn "3. Кількість публікацій за авторами"
    putStrLn "4. Кількість публікацій за категоріями"
    putStrLn "5. Активність читачів"
    putStrLn "6. Статті без файлів"
    putStrLn "0. Назад"
    choice <- prompt "Ваш вибір"
    case choice of
        "1" -> safe (reportTopViews conn) >> reportsMenu conn
        "2" -> safe (reportRatings conn) >> reportsMenu conn
        "3" -> safe (reportAuthors conn) >> reportsMenu conn
        "4" -> safe (reportCategories conn) >> reportsMenu conn
        "5" -> safe (reportReaders conn) >> reportsMenu conn
        "6" -> safe (reportWithoutFiles conn) >> reportsMenu conn
        "0" -> pure ()
        _   -> putStrLn "Невірний пункт." >> reportsMenu conn

reportTopViews :: Connection -> IO ()
reportTopViews conn = do
    rows <- topByViews conn
    printHeader ["Місце", "ID", "Назва", "Перегляди", "Вподобання"]
    forM_ (zip [1 :: Int ..] rows) $ \(place, (ident, title, views, likes)) ->
        printRow [show place, show ident, short 55 title, show views, show likes]

reportRatings :: Connection -> IO ()
reportRatings conn = do
    rows <- averageRatings conn
    printHeader ["ID", "Назва", "Відгуків", "Середній бал"]
    forM_ rows $ \(ident, title, count, avgRating) ->
        printRow [ show ident
                 , short 55 title
                 , show count
                 , maybe "—" (show . roundTo2) avgRating
                 ]
  where
    roundTo2 :: Double -> Double
    roundTo2 x = fromIntegral (round (x * 100) :: Int) / 100

reportAuthors :: Connection -> IO ()
reportAuthors conn = do
    rows <- articlesByAuthor conn
    printHeader ["ID", "Автор", "Публікацій"]
    forM_ rows $ \(ident, name, count) ->
        printRow [show ident, T.unpack name, show count]

reportCategories :: Connection -> IO ()
reportCategories conn = do
    rows <- articlesByCategory conn
    printHeader ["ID", "Категорія", "Публікацій"]
    forM_ rows $ \(ident, name, count) ->
        printRow [show ident, T.unpack name, show count]

reportReaders :: Connection -> IO ()
reportReaders conn = do
    rows <- readerActivity conn
    printHeader ["ID", "Читач", "Відгуків", "Середня оцінка"]
    forM_ rows $ \(ident, name, count, avgRating) ->
        printRow [ show ident
                 , T.unpack name
                 , show count
                 , maybe "—" (show . roundTo2) avgRating
                 ]
  where
    roundTo2 :: Double -> Double
    roundTo2 x = fromIntegral (round (x * 100) :: Int) / 100

reportWithoutFiles :: Connection -> IO ()
reportWithoutFiles conn = do
    rows <- articlesWithoutFiles conn
    if null rows
        then putStrLn "Усі статті мають файли."
        else do
            printHeader ["ID", "Назва"]
            forM_ rows $ \(ident, title) ->
                printRow [show ident, T.unpack title]
