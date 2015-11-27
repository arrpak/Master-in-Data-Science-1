
-- Countries with 3 o more cities with elevation higher than its national elevation average

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
