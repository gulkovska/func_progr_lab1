{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Узагальнений шар доступу до даних.
--
-- Тут демонструється головна ідея лабораторної роботи:
-- типовий клас 'Entity' описує метадані та перетворення сутності,
-- типовий клас 'Repository' — універсальні CRUD-операції,
-- а конкретні типи ('Author', 'Article', ...) отримують поведінку
-- через 'instance'.
module Newspaper.Entity
    ( Entity(..)
    , Repository(..)
    , Params(..)
    ) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Proxy (Proxy(..))
import Data.String (fromString)
import qualified Data.Text as T
import Database.MySQL.Simple
import Database.MySQL.Simple.Param (Action, render)
import Database.MySQL.Simple.QueryParams (QueryParams(..))
import Database.MySQL.Simple.QueryResults (QueryResults(..), convertError)
import Database.MySQL.Simple.Result (Result(..))

import Newspaper.Types

-- | Обгортка над списком параметрів довільної довжини.
--
-- Стандартні екземпляри 'QueryParams' у mysql-simple існують лише
-- для кортежів фіксованої арності. Ця обгортка дозволяє будувати
-- INSERT/UPDATE зі списком полів, визначеним у екземплярі 'Entity'.
newtype Params = Params [Action]

instance QueryParams Params where
    renderParams (Params actions) = actions

-- | Метадані сутності та перетворення між Haskell-записом і рядком MySQL.
class QueryResults a => Entity a where
    -- | Українська назва сутності для повідомлень.
    entityTitle :: Proxy a -> String

    -- | Назва таблиці в MySQL.
    tableName :: Proxy a -> String

    -- | Назва стовпця первинного ключа.
    idColumn :: Proxy a -> String

    -- | Отримати ідентифікатор (Nothing — запис ще не збережено).
    entityId :: a -> Maybe Int

    -- | Присвоїти ідентифікатор після INSERT.
    setEntityId :: Int -> a -> a

    -- | Коротке текстове представлення для списків і підтверджень.
    entityLabel :: a -> String

    -- | Стовпці для SELECT (у порядку 'QueryResults').
    selectColumns :: Proxy a -> [String]

    -- | Стовпці для INSERT (без id).
    insertColumns :: Proxy a -> [String]

    -- | Значення для INSERT (у порядку 'insertColumns').
    insertActions :: a -> [Action]

    -- | Присвоєння для UPDATE (без id).
    updateAssignments :: Proxy a -> [String]

    -- | Значення для UPDATE (у порядку 'updateAssignments').
    updateActions :: a -> [Action]

    -- Похідні запити; їх не потрібно переписувати в кожному instance.

    selectAllQuery :: Proxy a -> Query
    selectAllQuery p = fromString $
        "SELECT " ++ intercalate ", " (selectColumns p)
        ++ " FROM `" ++ tableName p ++ "`"
        ++ " ORDER BY `" ++ idColumn p ++ "`"

    selectByIdQuery :: Proxy a -> Query
    selectByIdQuery p = fromString $
        "SELECT " ++ intercalate ", " (selectColumns p)
        ++ " FROM `" ++ tableName p ++ "`"
        ++ " WHERE `" ++ idColumn p ++ "` = ?"

    insertQuery :: Proxy a -> Query
    insertQuery p = fromString $
        "INSERT INTO `" ++ tableName p ++ "` ("
        ++ intercalate ", " (insertColumns p) ++ ") VALUES ("
        ++ intercalate ", " (replicate (length (insertColumns p)) "?")
        ++ ")"

    updateQuery :: Proxy a -> Query
    updateQuery p = fromString $
        "UPDATE `" ++ tableName p ++ "` SET "
        ++ intercalate ", " (updateAssignments p)
        ++ " WHERE `" ++ idColumn p ++ "` = ?"

    deleteQuery :: Proxy a -> Query
    deleteQuery p = fromString $
        "DELETE FROM `" ++ tableName p ++ "` WHERE `" ++ idColumn p ++ "` = ?"

-- | Універсальний репозиторій. Усі методи мають реалізації за
-- замовчуванням, тому екземпляри виглядають як @instance Repository Author@.
class Entity a => Repository a where
    listAll :: Connection -> IO [a]
    listAll conn = query_ conn (selectAllQuery (Proxy @a))

    findById :: Connection -> Int -> IO (Maybe a)
    findById conn ident =
        listToMaybe <$> query conn (selectByIdQuery (Proxy @a)) (Only ident)

    -- | INSERT; повертає згенерований AUTO_INCREMENT ідентифікатор.
    create :: Connection -> a -> IO Int
    create conn value = do
        _ <- execute conn (insertQuery (Proxy @a)) (Params (insertActions value))
        fromIntegral <$> insertID conn

    -- | UPDATE за id; повертає True, якщо рядок існував.
    updateEntity :: Connection -> a -> IO Bool
    updateEntity conn value =
        case entityId value of
            Nothing -> pure False
            Just ident -> do
                affected <- execute conn (updateQuery (Proxy @a))
                    (Params (updateActions value ++ [render ident]))
                pure (affected > 0)

    -- | DELETE за id, взятим із сутності; повертає True, якщо рядок видалено.
    deleteEntity :: Connection -> a -> IO Bool
    deleteEntity conn value =
        case entityId value of
            Nothing -> pure False
            Just ident -> do
                affected <- execute conn (deleteQuery (Proxy @a)) (Only ident)
                pure (affected > 0)

    -- | INSERT або UPDATE залежно від наявності id.
    saveEntity :: Connection -> a -> IO Int
    saveEntity conn value =
        case entityId value of
            Nothing -> create conn value
            Just ident -> do
                _ <- updateEntity conn value
                pure ident

--------------------------------------------------------------------------------
-- QueryResults: перетворення рядка MySQL у Haskell-запис
--------------------------------------------------------------------------------

instance QueryResults Author where
    convertResults [fId, fName, fEmail, fDept] [vId, vName, vEmail, vDept] =
        Author (convert fId vId)
               (convert fName vName)
               (convert fEmail vEmail)
               (convert fDept vDept)
    convertResults fields values = convertError fields values 4

instance QueryResults Category where
    convertResults [fId, fName, fDescr] [vId, vName, vDescr] =
        Category (convert fId vId)
                 (convert fName vName)
                 (convert fDescr vDescr)
    convertResults fields values = convertError fields values 3

instance QueryResults Article where
    convertResults [fId, fTitle, fType, fAbstract, fAuthor, fCategory, fDate]
                   [vId, vTitle, vType, vAbstract, vAuthor, vCategory, vDate] =
        Article (convert fId vId)
                (convert fTitle vTitle)
                (fromMaybe TextMaterial (materialTypeFromDb (convert fType vType)))
                (convert fAbstract vAbstract)
                (convert fAuthor vAuthor)
                (convert fCategory vCategory)
                (convert fDate vDate)
    convertResults fields values = convertError fields values 7

instance QueryResults Material where
    convertResults [fId, fArticle, fPath, fType, fSize]
                   [vId, vArticle, vPath, vType, vSize] =
        Material (convert fId vId)
                 (convert fArticle vArticle)
                 (convert fPath vPath)
                 (convert fType vType)
                 (convert fSize vSize)
    convertResults fields values = convertError fields values 5

instance QueryResults Reader where
    convertResults [fId, fName, fEmail, fFaculty] [vId, vName, vEmail, vFaculty] =
        Reader (convert fId vId)
               (convert fName vName)
               (convert fEmail vEmail)
               (convert fFaculty vFaculty)
    convertResults fields values = convertError fields values 4

instance QueryResults Review where
    convertResults [fId, fArticle, fReader, fRating, fComment, fCreated]
                   [vId, vArticle, vReader, vRating, vComment, vCreated] =
        Review (convert fId vId)
               (convert fArticle vArticle)
               (convert fReader vReader)
               (convert fRating vRating)
               (convert fComment vComment)
               (convert fCreated vCreated)
    convertResults fields values = convertError fields values 6

instance QueryResults Statistic where
    convertResults [fId, fArticle, fViews, fLikes] [vId, vArticle, vViews, vLikes] =
        Statistic (convert fId vId)
                  (convert fArticle vArticle)
                  (convert fViews vViews)
                  (convert fLikes vLikes)
    convertResults fields values = convertError fields values 4

--------------------------------------------------------------------------------
-- Entity: метадані кожної таблиці
--------------------------------------------------------------------------------

instance Entity Author where
    entityTitle _ = "автор"
    tableName _ = "authors"
    idColumn _ = "id"
    entityId = authorId
    setEntityId ident author = author { authorId = Just ident }
    entityLabel = T.unpack . authorName
    selectColumns _ = ["id", "name", "email", "department"]
    insertColumns _ = ["name", "email", "department"]
    insertActions author =
        [ render (authorName author)
        , render (authorEmail author)
        , render (authorDepartment author)
        ]
    updateAssignments _ = ["name = ?", "email = ?", "department = ?"]
    updateActions author =
        [ render (authorName author)
        , render (authorEmail author)
        , render (authorDepartment author)
        ]

instance Entity Category where
    entityTitle _ = "категорія"
    tableName _ = "categories"
    idColumn _ = "id"
    entityId = categoryId
    setEntityId ident category = category { categoryId = Just ident }
    entityLabel = T.unpack . categoryName
    selectColumns _ = ["id", "name", "description"]
    insertColumns _ = ["name", "description"]
    insertActions category =
        [ render (categoryName category)
        , render (categoryDescription category)
        ]
    updateAssignments _ = ["name = ?", "description = ?"]
    updateActions category =
        [ render (categoryName category)
        , render (categoryDescription category)
        ]

instance Entity Article where
    entityTitle _ = "стаття/матеріал"
    tableName _ = "articles"
    idColumn _ = "id"
    entityId = articleId
    setEntityId ident article = article { articleId = Just ident }
    entityLabel = T.unpack . articleTitle
    selectColumns _ =
        [ "id", "title", "material_type", "abstract_text"
        , "author_id", "category_id", "published_at"
        ]
    insertColumns _ =
        [ "title", "material_type", "abstract_text"
        , "author_id", "category_id", "published_at"
        ]
    insertActions article =
        [ render (articleTitle article)
        , render (materialTypeToDb (articleType article))
        , render (articleAbstract article)
        , render (articleAuthorId article)
        , render (articleCategoryId article)
        , render (articlePublishedAt article)
        ]
    updateAssignments _ =
        [ "title = ?", "material_type = ?", "abstract_text = ?"
        , "author_id = ?", "category_id = ?", "published_at = ?"
        ]
    updateActions article =
        [ render (articleTitle article)
        , render (materialTypeToDb (articleType article))
        , render (articleAbstract article)
        , render (articleAuthorId article)
        , render (articleCategoryId article)
        , render (articlePublishedAt article)
        ]

instance Entity Material where
    entityTitle _ = "файл публікації"
    tableName _ = "materials"
    idColumn _ = "id"
    entityId = materialId
    setEntityId ident material = material { materialId = Just ident }
    entityLabel = T.unpack . materialFilePath
    selectColumns _ = ["id", "article_id", "file_path", "file_type", "size_kb"]
    insertColumns _ = ["article_id", "file_path", "file_type", "size_kb"]
    insertActions material =
        [ render (materialArticleId material)
        , render (materialFilePath material)
        , render (materialFileType material)
        , render (materialSizeKb material)
        ]
    updateAssignments _ =
        ["article_id = ?", "file_path = ?", "file_type = ?", "size_kb = ?"]
    updateActions material =
        [ render (materialArticleId material)
        , render (materialFilePath material)
        , render (materialFileType material)
        , render (materialSizeKb material)
        ]

instance Entity Reader where
    entityTitle _ = "читач"
    tableName _ = "readers"
    idColumn _ = "id"
    entityId = readerId
    setEntityId ident reader = reader { readerId = Just ident }
    entityLabel = T.unpack . readerName
    selectColumns _ = ["id", "name", "email", "faculty"]
    insertColumns _ = ["name", "email", "faculty"]
    insertActions reader =
        [ render (readerName reader)
        , render (readerEmail reader)
        , render (readerFaculty reader)
        ]
    updateAssignments _ = ["name = ?", "email = ?", "faculty = ?"]
    updateActions reader =
        [ render (readerName reader)
        , render (readerEmail reader)
        , render (readerFaculty reader)
        ]

instance Entity Review where
    entityTitle _ = "відгук"
    tableName _ = "reviews"
    idColumn _ = "id"
    entityId = reviewId
    setEntityId ident review = review { reviewId = Just ident }
    entityLabel review = "оцінка " ++ show (reviewRating review)
    selectColumns _ =
        ["id", "article_id", "reader_id", "rating", "comment", "created_at"]
    insertColumns _ =
        ["article_id", "reader_id", "rating", "comment", "created_at"]
    insertActions review =
        [ render (reviewArticleId review)
        , render (reviewReaderId review)
        , render (reviewRating review)
        , render (reviewComment review)
        , render (reviewCreatedAt review)
        ]
    updateAssignments _ =
        [ "article_id = ?", "reader_id = ?", "rating = ?"
        , "comment = ?", "created_at = ?"
        ]
    updateActions review =
        [ render (reviewArticleId review)
        , render (reviewReaderId review)
        , render (reviewRating review)
        , render (reviewComment review)
        , render (reviewCreatedAt review)
        ]

instance Entity Statistic where
    entityTitle _ = "статистика"
    tableName _ = "statistics"
    idColumn _ = "id"
    entityId = statisticId
    setEntityId ident stat = stat { statisticId = Just ident }
    entityLabel stat =
        "перегляди: " ++ show (statisticViews stat)
        ++ ", вподобання: " ++ show (statisticLikes stat)
    selectColumns _ = ["id", "article_id", "views", "likes"]
    insertColumns _ = ["article_id", "views", "likes"]
    insertActions stat =
        [ render (statisticArticleId stat)
        , render (statisticViews stat)
        , render (statisticLikes stat)
        ]
    updateAssignments _ = ["article_id = ?", "views = ?", "likes = ?"]
    updateActions stat =
        [ render (statisticArticleId stat)
        , render (statisticViews stat)
        , render (statisticLikes stat)
        ]

--------------------------------------------------------------------------------
-- Repository: порожні екземпляри вмикають усі CRUD-реалізації за замовчуванням
--------------------------------------------------------------------------------

instance Repository Author
instance Repository Category
instance Repository Article
instance Repository Material
instance Repository Reader
instance Repository Review
instance Repository Statistic
