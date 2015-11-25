-- Comment

DROP TABLE IF EXISTS myLibrary;
CREATE TABLE myLibrary
(
    title           VARCHAR(100),
    author          VARCHAR(100),
    author2         VARCHAR(100),
    publisher       VARCHAR(100),
    pages           INT,
    publish_date    DATE,
    isbn            VARCHAR(100) NOT NULL,
    book_language   VARCHAR(100)
);

INSERT INTO  myLibrary VALUES ('SQL Bible','Alex Kriegel','Boris M.Trukhnov','Wiley',888,'2008-04-07','978-0470229064','English');
INSERT INTO  myLibrary VALUES ('MindSwap','Robert Sheckley',NULL,'Wiley',224,'2006-05-30','978-0470229100','English');
INSERT INTO  myLibrary VALUES ('John Livigsnton Seagull','Richard Bach',NULL,'McMillan',100,CURRENT_DATE,'978-0470229001','English');

SELECT * FROM myLibrary;
SELECT * FROM myLibrary LIMIT 2;

UPDATE mylibrary SET pages=500,title='SQL Bible, 2nd Edition' WHERE title='SQL Bible';

ALTER TABLE myLibrary ADD COLUMN price REAL;
UPDATE myLibrary SET price=27.60 WHERE pages=500;
UPDATE myLibrary SET price=24.60 WHERE pages=224;
UPDATE myLibrary SET price=18.60 WHERE pages=100;

DELETE FROM mylibrary WHERE pages<200;

SELECT * FROM myLibrary;

INSERT INTO  myLibrary VALUES ('John Livigsnton Seagull','Richard Bach',NULL,'McMillan',100,CURRENT_DATE,'978-0470229001','English',18.60);
SELECT * FROM myLibrary WHERE author2 IS NULL;
SELECT * FROM myLibrary ORDER BY publisher DESC, author DESC;

SELECT * INTO cheapmyLibary FROM myLibrary WHERE price <25.00 ORDER BY price;
SELECT * FROM cheapmyLibary;
DROP TABLE cheapmyLibary;

SELECT * FROM myLibrary WHERE price BETWEEN 20 AND 26;
SELECT * FROM myLibrary WHERE pages IN (100,200,300,400,500);
SELECT * FROM myLibrary WHERE author LIKE 'R%';

SELECT publisher,SUM(pages) AS tot_pages FROM myLibrary GROUP BY publisher ORDER BY tot_pages DESC;
-- Wrong SELECT publisher FROM myLibrary WHERE SUM(pages)>150;
SELECT publisher,SUM(pages) AS tot_pages FROM myLibrary GROUP BY publisher HAVING SUM(pages)>150;

--Subqueries
SELECT * FROM myLibrary WHERE pages>(SELECT AVG(pages) FROM myLibrary);
SELECT * FROM myLibrary WHERE pages IN (SELECT pages FROM myLibrary WHERE author LIKE 'R%');
SELECT * FROM myLibrary WHERE pages = ANY (SELECT pages FROM myLibrary WHERE author LIKE 'R%');

DROP TABLE IF EXISTS tmpLibrary;
CREATE TABLE tmplibrary
(
    title           VARCHAR(100),
    author          VARCHAR(100),
    author2         VARCHAR(100),
    publisher       VARCHAR(100),
    pages           INT,
    publish_date    DATE,
    isbn            VARCHAR(100) NOT NULL,
    book_language   VARCHAR(100),
    price           REAL
);
INSERT INTO tmplibrary SELECT * FROM myLibrary WHERE pages<(SELECT AVG(pages) FROM myLibrary);
SELECT * FROM tmplibrary;
