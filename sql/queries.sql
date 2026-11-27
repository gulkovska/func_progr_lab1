-- =====================================================================
--  Приклади SQL-запитів до БД "Мережева газета факультету"
--  (можна використовувати під час захисту лабораторної роботи)
-- =====================================================================

USE faculty_newspaper;
SET NAMES utf8mb4;

-- 1. Усі публікації з автором і категорією
SELECT a.id,
       a.title,
       a.material_type,
       au.name AS author,
       c.name  AS category,
       a.published_at
FROM articles a
JOIN authors au    ON au.id = a.author_id
JOIN categories c  ON c.id  = a.category_id
ORDER BY a.published_at DESC;

-- 2. Файли конкретної публікації
SELECT m.id, m.file_path, m.file_type, m.size_kb
FROM materials m
WHERE m.article_id = 1
ORDER BY m.id;

-- 3. Відгуки на публікацію разом з іменами читачів
SELECT rv.id, rd.name AS reader, rv.rating, rv.comment, rv.created_at
FROM reviews rv
JOIN readers rd ON rd.id = rv.reader_id
WHERE rv.article_id = 1
ORDER BY rv.created_at DESC;

-- 4. Статистика публікацій
SELECT a.id, a.title, s.views, s.likes
FROM articles a
JOIN statistics s ON s.article_id = a.id
ORDER BY s.views DESC;

-- 5. Топ-10 матеріалів за переглядами
SELECT a.id, a.title, s.views, s.likes
FROM articles a
JOIN statistics s ON s.article_id = a.id
ORDER BY s.views DESC, s.likes DESC
LIMIT 10;

-- 6. Середній рейтинг публікацій
SELECT a.id,
       a.title,
       COUNT(rv.id)              AS reviews_count,
       ROUND(AVG(rv.rating), 2)  AS avg_rating
FROM articles a
LEFT JOIN reviews rv ON rv.article_id = a.id
GROUP BY a.id, a.title
ORDER BY AVG(rv.rating) DESC, COUNT(rv.id) DESC;

-- 7. Кількість публікацій за авторами
SELECT au.id, au.name, COUNT(a.id) AS articles_count
FROM authors au
LEFT JOIN articles a ON a.author_id = au.id
GROUP BY au.id, au.name
ORDER BY articles_count DESC, au.name;

-- 8. Кількість публікацій за категоріями
SELECT c.id, c.name, COUNT(a.id) AS articles_count
FROM categories c
LEFT JOIN articles a ON a.category_id = c.id
GROUP BY c.id, c.name
ORDER BY articles_count DESC, c.name;

-- 9. Активність читачів
SELECT rd.id,
       rd.name,
       COUNT(rv.id)             AS reviews_count,
       ROUND(AVG(rv.rating), 2) AS avg_rating
FROM readers rd
LEFT JOIN reviews rv ON rv.reader_id = rd.id
GROUP BY rd.id, rd.name
ORDER BY reviews_count DESC, rd.name;

-- 10. Публікації без файлів
SELECT a.id, a.title
FROM articles a
LEFT JOIN materials m ON m.article_id = a.id
WHERE m.id IS NULL
ORDER BY a.id;

-- 11. Пошук за назвою або анотацією
SELECT id, title, abstract_text
FROM articles
WHERE title LIKE '%олімпіад%' OR abstract_text LIKE '%олімпіад%';

-- 12. Приклад коригування даних
-- UPDATE articles
-- SET title = 'Нова назва', abstract_text = 'Нова анотація'
-- WHERE id = 1;

-- 13. Приклад видалення (каскадно видалить файли, відгуки, статистику)
-- DELETE FROM articles WHERE id = 1;
