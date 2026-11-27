{-# LANGUAGE OverloadedStrings #-}

-- | Модель предметної області інформаційної системи
-- "Мережева газета факультету".
module Newspaper.Types
    ( Author(..)
    , Category(..)
    , MaterialType(..)
    , Article(..)
    , Material(..)
    , Reader(..)
    , Review(..)
    , Statistic(..)
    , materialTypeToDb
    , materialTypeFromDb
    , materialTypeLabel
    , parseMaterialType
    ) where

import Data.Char (toLower)
import Data.Text (Text)
import Data.Time (Day, UTCTime)

-- | Автор публікації.
data Author = Author
    { authorId         :: Maybe Int
    , authorName       :: Text
    , authorEmail      :: Text
    , authorDepartment :: Text
    } deriving (Eq, Show)

-- | Рубрика (категорія) газети.
data Category = Category
    { categoryId          :: Maybe Int
    , categoryName        :: Text
    , categoryDescription :: Text
    } deriving (Eq, Show)

-- | Тип матеріалу, що зберігається в MySQL ENUM.
data MaterialType
    = TextMaterial
    | GraphicMaterial
    | VideoMaterial
    deriving (Eq, Show, Read, Enum, Bounded)

-- | Стаття або графічний/відеоматеріал.
data Article = Article
    { articleId          :: Maybe Int
    , articleTitle       :: Text
    , articleType        :: MaterialType
    , articleAbstract    :: Text
    , articleAuthorId    :: Int
    , articleCategoryId  :: Int
    , articlePublishedAt :: Day
    } deriving (Eq, Show)

-- | Файл публікації (html, pdf, jpg, mp4 тощо).
data Material = Material
    { materialId        :: Maybe Int
    , materialArticleId :: Int
    , materialFilePath  :: Text
    , materialFileType  :: Text
    , materialSizeKb    :: Int
    } deriving (Eq, Show)

-- | Читач газети.
data Reader = Reader
    { readerId      :: Maybe Int
    , readerName    :: Text
    , readerEmail   :: Text
    , readerFaculty :: Text
    } deriving (Eq, Show)

-- | Відгук читача про публікацію.
data Review = Review
    { reviewId        :: Maybe Int
    , reviewArticleId :: Int
    , reviewReaderId  :: Int
    , reviewRating    :: Int
    , reviewComment   :: Maybe Text
    , reviewCreatedAt :: UTCTime
    } deriving (Eq, Show)

-- | Статистика переглядів і вподобань окремого матеріалу.
data Statistic = Statistic
    { statisticId        :: Maybe Int
    , statisticArticleId :: Int
    , statisticViews     :: Int
    , statisticLikes     :: Int
    } deriving (Eq, Show)

materialTypeToDb :: MaterialType -> Text
materialTypeToDb TextMaterial    = "article"
materialTypeToDb GraphicMaterial = "graphic"
materialTypeToDb VideoMaterial   = "video"

materialTypeFromDb :: Text -> Maybe MaterialType
materialTypeFromDb "article" = Just TextMaterial
materialTypeFromDb "graphic" = Just GraphicMaterial
materialTypeFromDb "video"   = Just VideoMaterial
materialTypeFromDb _         = Nothing

materialTypeLabel :: MaterialType -> Text
materialTypeLabel TextMaterial    = "стаття"
materialTypeLabel GraphicMaterial = "графіка"
materialTypeLabel VideoMaterial   = "відео"

-- | Розбір типу матеріалу з підказки користувача.
parseMaterialType :: String -> Maybe MaterialType
parseMaterialType raw =
    case map toLower raw of
        "1"       -> Just TextMaterial
        "2"       -> Just GraphicMaterial
        "3"       -> Just VideoMaterial
        "article" -> Just TextMaterial
        "text"    -> Just TextMaterial
        "стаття"  -> Just TextMaterial
        "graphic" -> Just GraphicMaterial
        "image"   -> Just GraphicMaterial
        "графіка" -> Just GraphicMaterial
        "video"   -> Just VideoMaterial
        "відео"   -> Just VideoMaterial
        _         -> Nothing
