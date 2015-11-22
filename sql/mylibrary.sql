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
SELECT TOP 2 * FROM myLibrary;
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