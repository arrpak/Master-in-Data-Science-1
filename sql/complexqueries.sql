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


-- Using JOIN INNER

SELECT t1.country_name, (t1.tpop/t2.nair) AS aperc 
FROM  t1 INNER JOIN t2 ON t1.country_name=t2.country_name
WHERE t1.tpop>0 
ORDER BY aperc LIMIT 10;

SELECT airline_code_2c, flight_freq, name
FROM ref_airline_nb_of_flights AS r
INNER JOIN optd_airlines AS o
ON o."2char_code" = r.airline_code_2c
ORDER BY airline_code_2c;

-- Using LEFT OUTER JOIN

SELECT airline_code_2c, flight_freq, name
FROM ref_airline_nb_of_flights AS r
LEFT OUTER JOIN optd_airlines AS o
ON o."2char_code" = r.airline_code_2c
ORDER BY flight_freq DESC LIMIT 10;

-- Using RIGHT OUTER JOIN

SELECT airline_code_2c, flight_freq, name
FROM ref_airline_nb_of_flights AS r
RIGHT OUTER JOIN optd_airlines AS o
ON o."2char_code" = r.airline_code_2c
ORDER BY flight_freq DESC LIMIT 10;

-- Using multiple queries

SELECT name, country_name, elevation FROM optd_por_public
WHERE location_type='C' AND elevation > 
(
    SELECT AVG(elevation) FROM optd_por_public WHERE elevation IS NOT NULL AND location_type='C'
) 
ORDER BY elevation DESC LIMIT 10;

-- Countries with 3 o more cities higher than worldwide elevation average

SELECT country_name,COUNT(name) AS ncities FROM optd_por_public
WHERE location_type='C' AND elevation >
(
    SELECT AVG(elevation) FROM optd_por_public WHERE elevation IS NOT NULL AND location_type='C'
) 
GROUP BY country_name HAVING COUNT(name)>=3 ORDER BY COUNT(name) DESC;

-- Countries with 3 o more cities higher than its national elevation average

SELECT t1.country_name,COUNT(t1.name) AS ncities FROM optd_por_public AS t1
LEFT OUTER JOIN 
(
    SELECT country_name,AVG(elevation) AS avgelevation FROM optd_por_public
    WHERE elevation IS NOT NULL
    GROUP BY country_name
) AS t2
ON t1.country_name=t2.country_name
WHERE t1.elevation IS NOT NULL AND t1.elevation > t2.avgelevation
GROUP BY t1.country_name HAVING COUNT(t1.name)>=3
ORDER BY t1.country_name;
