
-- Creating views

DROP VIEW t1;
CREATE VIEW t1 AS 
(
    SELECT country_name, SUM(population) AS tpop FROM optd_por_public
    WHERE population IS NOT NULL
    GROUP BY country_name 
    ORDER BY tpop
);

DROP VIEW t2;
CREATE VIEW t2 AS 
(
    SELECT country_name, count(*) AS nair FROM optd_por_public
    WHERE location_type='A' OR location_type='CA'
    GROUP BY country_name 
    ORDER BY nair
);

-- Using JOIN INNER

SELECT t1.country_name, (t1.tpop/t2.nair) AS aperc 
FROM  t1 INNER JOIN t2 ON t1.country_name=t2.country_name
WHERE t1.tpop>0 
ORDER BY aperc LIMIT 10;
