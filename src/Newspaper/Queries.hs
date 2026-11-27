{-# LANGUAGE OverloadedStrings #-}

-- | Готові запити для звітів і детальних карток.
module Newspaper.Queries
    ( ArticleRow
    , MaterialRow
    , ReviewRow
    , listArticleRows
    , findArticleRow
    , searchArticles
    , listMaterialsWithArticle
    , articleMaterials
    , listReviewsWithNames
    , articleReviews
    , listStatistics
    , articleStatistic
    , upsertStatistic
    , topByViews
    , averageRatings
    , articlesByAuthor
    , articlesByCategory
    , readerActivity
    , articlesWithoutFiles
    ) where

import Control.Monad (void)
import Data.Maybe (listToMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Database.MySQL.Simple

-- | id, назва, тип, анотація, автор, категорія, дата, файлів, переглядів, вподобань.
type ArticleRow = (Int, Text, Text, Text, Text, Text, Day, Int, Int, Int)

-- | id, article_id, назва статті, шлях, тип файлу, розмір.
type MaterialRow = (Int, Int, Text, Text, Text, Int)

-- | id, article_id, назва статті, reader_id, ім'я читача, оцінка, коментар, дата.
type ReviewRow = (Int, Int, Text, Int, Text, Int, Maybe Text, UTCTime)

articleSelect :: String
articleSelect =
    "SELECT a.id, a.title, a.material_type, a.abstract_text, \
    \au.name, c.name, a.published_at, \
    \(SELECT COUNT(*) FROM materials m WHERE m.article_id = a.id), \
    \COALESCE((SELECT views FROM statistics s WHERE s.article_id = a.id), 0), \
    \COALESCE((SELECT likes FROM statistics s WHERE s.article_id = a.id), 0) \
    \FROM articles a \
    \JOIN authors au ON au.id = a.author_id \
    \JOIN categories c ON c.id = a.category_id"

listArticleRows :: Connection -> IO [ArticleRow]
listArticleRows conn = query_ conn (fromString $
    articleSelect ++ " ORDER BY a.published_at DESC, a.id DESC")

findArticleRow :: Connection -> Int -> IO (Maybe ArticleRow)
findArticleRow conn ident = listToMaybe <$> query conn (fromString $
    articleSelect ++ " WHERE a.id = ?") (Only ident)

searchArticles :: Connection -> Text -> IO [ArticleRow]
searchArticles conn needle =
    let pattern = "%" <> needle <> "%"
    in query conn (fromString $
        articleSelect
        ++ " WHERE a.title LIKE ? OR a.abstract_text LIKE ?"
        ++ " ORDER BY a.published_at DESC, a.id DESC")
        (pattern, pattern)

listMaterialsWithArticle :: Connection -> IO [MaterialRow]
listMaterialsWithArticle conn = query_ conn
    "SELECT m.id, m.article_id, a.title, m.file_path, m.file_type, m.size_kb \
    \FROM materials m JOIN articles a ON a.id = m.article_id ORDER BY m.id"

articleMaterials :: Connection -> Int -> IO [(Int, Text, Text, Int)]
articleMaterials conn articleId = query conn
    "SELECT id, file_path, file_type, size_kb FROM materials \
    \WHERE article_id = ? ORDER BY id" (Only articleId)

listReviewsWithNames :: Connection -> IO [ReviewRow]
listReviewsWithNames conn = query_ conn
    "SELECT rv.id, rv.article_id, a.title, rv.reader_id, rd.name, \
    \rv.rating, rv.comment, rv.created_at \
    \FROM reviews rv \
    \JOIN articles a ON a.id = rv.article_id \
    \JOIN readers rd ON rd.id = rv.reader_id \
    \ORDER BY rv.created_at DESC, rv.id DESC"

articleReviews :: Connection -> Int -> IO [(Int, Text, Int, Maybe Text, UTCTime)]
articleReviews conn articleId = query conn
    "SELECT rv.id, rd.name, rv.rating, rv.comment, rv.created_at \
    \FROM reviews rv JOIN readers rd ON rd.id = rv.reader_id \
    \WHERE rv.article_id = ? ORDER BY rv.created_at DESC, rv.id DESC"
    (Only articleId)

listStatistics :: Connection -> IO [(Int, Int, Text, Int, Int)]
listStatistics conn = query_ conn
    "SELECT s.id, s.article_id, a.title, s.views, s.likes \
    \FROM statistics s JOIN articles a ON a.id = s.article_id \
    \ORDER BY s.views DESC, s.likes DESC"

articleStatistic :: Connection -> Int -> IO [(Int, Int)]
articleStatistic conn articleId = query conn
    "SELECT views, likes FROM statistics WHERE article_id = ?" (Only articleId)

-- | INSERT ... ON DUPLICATE KEY UPDATE без залежності від версії MySQL.
upsertStatistic :: Connection -> Int -> Int -> Int -> IO ()
upsertStatistic conn articleId views likes = do
    updated <- execute conn
        "UPDATE statistics SET views = ?, likes = ? WHERE article_id = ?"
        (views, likes, articleId)
    if updated == 0
        then void (execute conn
            "INSERT INTO statistics (article_id, views, likes) VALUES (?, ?, ?)"
            (articleId, views, likes))
        else pure ()

--------------------------------------------------------------------------------
-- Звіти
--------------------------------------------------------------------------------

topByViews :: Connection -> IO [(Int, Text, Int, Int)]
topByViews conn = query_ conn
    "SELECT a.id, a.title, s.views, s.likes \
    \FROM articles a JOIN statistics s ON s.article_id = a.id \
    \ORDER BY s.views DESC, s.likes DESC LIMIT 10"

averageRatings :: Connection -> IO [(Int, Text, Int, Maybe Double)]
averageRatings conn = query_ conn
    "SELECT a.id, a.title, COUNT(rv.id), ROUND(AVG(rv.rating), 2) \
    \FROM articles a LEFT JOIN reviews rv ON rv.article_id = a.id \
    \GROUP BY a.id, a.title \
    \ORDER BY AVG(rv.rating) DESC, COUNT(rv.id) DESC"

articlesByAuthor :: Connection -> IO [(Int, Text, Int)]
articlesByAuthor conn = query_ conn
    "SELECT au.id, au.name, COUNT(a.id) \
    \FROM authors au LEFT JOIN articles a ON a.author_id = au.id \
    \GROUP BY au.id, au.name ORDER BY COUNT(a.id) DESC, au.name"

articlesByCategory :: Connection -> IO [(Int, Text, Int)]
articlesByCategory conn = query_ conn
    "SELECT c.id, c.name, COUNT(a.id) \
    \FROM categories c LEFT JOIN articles a ON a.category_id = c.id \
    \GROUP BY c.id, c.name ORDER BY COUNT(a.id) DESC, c.name"

readerActivity :: Connection -> IO [(Int, Text, Int, Maybe Double)]
readerActivity conn = query_ conn
    "SELECT rd.id, rd.name, COUNT(rv.id), ROUND(AVG(rv.rating), 2) \
    \FROM readers rd LEFT JOIN reviews rv ON rv.reader_id = rd.id \
    \GROUP BY rd.id, rd.name ORDER BY COUNT(rv.id) DESC, rd.name"

articlesWithoutFiles :: Connection -> IO [(Int, Text)]
articlesWithoutFiles conn = query_ conn
    "SELECT a.id, a.title FROM articles a \
    \LEFT JOIN materials m ON m.article_id = a.id \
    \WHERE m.id IS NULL ORDER BY a.id"
