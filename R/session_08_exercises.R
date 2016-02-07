#----------------------------------------------------------------------------
# ejercicios de agregación
#----------------------------------------------------------------------------

# El paquete plyr hace lo mismo que data.table o dplyr... solo que se atraganta con datos grandes
# En este ejercicio vas a hacer agregaciones en uno de tres ficheros "grandes" (alternativamente, en otro de tu interés):
# Un fichero de la EPA
# Parte (o todo el censo español del 2011)
# El fichero de retrasos de vuelos

# Para la EPA:
# Ve a http://www.ine.es/prodyser/microdatos.htm y localiza los datos de la EPA (encuesta población activa)
# Baja los de un trimestre
# Utiliza la función epa2005 del paquete MicroDatosEs para leerlos (mira la ayuda)
# Haz agregaciones (nota: para "contar" hay que sumar la variable factorel)

# Para el censo:
# Ve a http://www.ine.es/prodyser/micro_censopv.htm
# Baja o el fichero nacional o uno de los tres o cuatro en que han dividido el anterior (por si el primero es "demasiado")
# Utiliza la función censo2010 del paquete MicroDatosEs para leerlos (mira _muy detenidamente_ la ayuda)
# Haz agregaciones (nota: para "contar" hay que sumar la variable factor)

# Para los retrasos aéreos de EE.UU.:
# Ve a http://stat-computing.org/dataexpo/2009/the-data.html y baja los de un periodo
# Léelos usando la función fread de data.table (mucho más veloz que read.table para datos grandes)

# El objetivo es que compares plyr con los otros dos (y estos entre sí) y que practiques la sintaxis.

retrasos <- fread("http://stat-computing.org/dataexpo/2009/2008.csv.bz2")

data()
