SELECT t1.country_name, npop, nairports, CAST(nairports AS pc
FROM

(SELECT country_name, SUM(population) AS npop
FROM opt_por_public_clean
WHERE population IS NOT NULL
GROUP BY country_name) AS t1,

(SELECT country_name, COUNT(*) AS nairports
FROM optd_po_public_clean
WHERE location_type='A' OR location_type='CA'
GROUP BY country_name) AS airports

WHERE pop.country_name = airports.country_name
ORDER BY pc LIMIT 10;