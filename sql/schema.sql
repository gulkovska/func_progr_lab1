-- =====================================================================
--  Інформаційна система "Мережева газета факультету"
--  Схема бази даних MySQL (7 таблиць)
--
--  Запуск:
--    mysql -uroot --default-character-set=utf8mb4 < sql/schema.sql
-- =====================================================================

CREATE DATABASE IF NOT EXISTS faculty_newspaper
    CHARACTER SET utf8mb4
    COLLATE utf8mb4_unicode_ci;

USE faculty_newspaper;

SET NAMES utf8mb4;

-- 1. Автори публікацій
CREATE TABLE IF NOT EXISTS authors (
    id          INT NOT NULL AUTO_INCREMENT,
    name        VARCHAR(120) NOT NULL,
    email       VARCHAR(160) NOT NULL,
    department  VARCHAR(160) NOT NULL,
    PRIMARY KEY (id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- 2. Категорії (рубрики) газети
CREATE TABLE IF NOT EXISTS categories (
    id          INT NOT NULL AUTO_INCREMENT,
    name        VARCHAR(120) NOT NULL,
    description VARCHAR(400) NOT NULL DEFAULT '',
    PRIMARY KEY (id),
    UNIQUE KEY uq_categories_name (name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- 3. Статті, графічні та відеоматеріали
CREATE TABLE IF NOT EXISTS articles (
    id            INT NOT NULL AUTO_INCREMENT,
    title         VARCHAR(255) NOT NULL,
    material_type ENUM('article', 'graphic', 'video') NOT NULL DEFAULT 'article',
    abstract_text TEXT NOT NULL,
    author_id     INT NOT NULL,
    category_id   INT NOT NULL,
    published_at  DATE NOT NULL,
    PRIMARY KEY (id),
    KEY idx_articles_author (author_id),
    KEY idx_articles_category (category_id),
    KEY idx_articles_published (published_at),
    CONSTRAINT fk_articles_author
        FOREIGN KEY (author_id) REFERENCES authors (id)
        ON DELETE RESTRICT ON UPDATE CASCADE,
    CONSTRAINT fk_articles_category
        FOREIGN KEY (category_id) REFERENCES categories (id)
        ON DELETE RESTRICT ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- 4. Файли публікацій
CREATE TABLE IF NOT EXISTS materials (
    id         INT NOT NULL AUTO_INCREMENT,
    article_id INT NOT NULL,
    file_path  VARCHAR(512) NOT NULL,
    file_type  VARCHAR(40) NOT NULL,
    size_kb    INT NOT NULL DEFAULT 0,
    PRIMARY KEY (id),
    KEY idx_materials_article (article_id),
    CONSTRAINT fk_materials_article
        FOREIGN KEY (article_id) REFERENCES articles (id)
        ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- 5. Читачі
CREATE TABLE IF NOT EXISTS readers (
    id      INT NOT NULL AUTO_INCREMENT,
    name    VARCHAR(120) NOT NULL,
    email   VARCHAR(160) NOT NULL,
    faculty VARCHAR(160) NOT NULL,
    PRIMARY KEY (id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- 6. Відгуки читачів
CREATE TABLE IF NOT EXISTS reviews (
    id         INT NOT NULL AUTO_INCREMENT,
    article_id INT NOT NULL,
    reader_id  INT NOT NULL,
    rating     TINYINT NOT NULL,
    comment    TEXT NULL,
    created_at DATETIME NOT NULL,
    PRIMARY KEY (id),
    KEY idx_reviews_article (article_id),
    KEY idx_reviews_reader (reader_id),
    CONSTRAINT fk_reviews_article
        FOREIGN KEY (article_id) REFERENCES articles (id)
        ON DELETE CASCADE ON UPDATE CASCADE,
    CONSTRAINT fk_reviews_reader
        FOREIGN KEY (reader_id) REFERENCES readers (id)
        ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- 7. Статистика переглядів і вподобань
CREATE TABLE IF NOT EXISTS statistics (
    id         INT NOT NULL AUTO_INCREMENT,
    article_id INT NOT NULL,
    views      INT NOT NULL DEFAULT 0,
    likes      INT NOT NULL DEFAULT 0,
    PRIMARY KEY (id),
    UNIQUE KEY uq_statistics_article (article_id),
    CONSTRAINT fk_statistics_article
        FOREIGN KEY (article_id) REFERENCES articles (id)
        ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
