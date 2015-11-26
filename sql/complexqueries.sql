-- Top Ten countries with more population

SELECT country_name, SUM(population) AS tpop FROM optd_por_public
WHERE population IS NOT NULL
GROUP BY country_name 
ORDER BY tpop DESC LIMIT 10;

-- Number of airports per country

SELECT country_name, count(*) AS nair FROM optd_por_public
WHERE location_type='A' OR location_type='CA'
GROUP BY country_name 
ORDER BY nair DESC LIMIT 10;

-- Using both subqueries

SELECT t1.country_name, tpop/t2.nair AS aperc FROM

    (SELECT country_name, SUM(population) AS tpop FROM optd_por_public
    WHERE population IS NOT NULL
    GROUP BY country_name 
    ORDER BY tpop) as t1,

    (SELECT country_name, count(*) AS nair FROM optd_por_public
    WHERE location_type='A' OR location_type='CA'
    GROUP BY country_name 
    ORDER BY nair) as t2

WHERE (t1.country_name=t2.country_name) AND t1.tpop >0
ORDER BY aperc;

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

-- Both using views

SELECT t1.country_name, (t1.tpop/t2.nair) AS aperc FROM t1,t2
WHERE t1.country_name=t2.country_name AND t1.tpop>0
ORDER BY aperc LIMIT 10;
