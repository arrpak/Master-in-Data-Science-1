SELECT airline_code_2c, name, flight_freq
FROM ref_airline_nb_of_flights AS r
LEFT OUTER JOIN optd_airlines AS o
ON o."2char_code" = r.airline_code_2c
ORDER BY flight_freq DESC LIMIT 10;

SELECT airline_code_2c, name, flight_freq
FROM optd_airlines AS o
LEFT OUTER JOIN ref_airline_nb_of_flights AS r
ON o."2char_code" = r.airline_code_2c
ORDER BY flight_freq DESC LIMIT 10;

SELECT airline_code_2c, name, flight_freq
FROM ref_airline_nb_of_flights AS r
RIGHT OUTER JOIN optd_airlines AS o
ON o."2char_code" = r.airline_code_2c
ORDER BY flight_freq DESC LIMIT 10;

SELECT airline_code_2c, name, flight_freq
FROM optd_airlines AS o
RIGHT OUTER JOIN ref_airline_nb_of_flights AS r
ON o."2char_code" = r.airline_code_2c
ORDER BY flight_freq DESC LIMIT 10;