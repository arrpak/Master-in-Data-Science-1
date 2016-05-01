#' ---
#' title: "Sistemas de Recomendación"
#' author: Jorge Ayuso Rejas
#' date: Abril 2016
#' output: 
#'  html_document:
#'    theme: cosmo
#'    highlight: tango
#'    css: img/base.css
#' ---

#+ include=FALSE
rm(list = ls());gc()

install.packages('magrittr')
library(magrittr)
knitr::opts_chunk$set(cache = FALSE,message = FALSE,warning = FALSE,echo = TRUE,fig.align = "center")

#' ## Introducción
#' De manera general un sistema de recomendación es un algortimo para recomendar a usuarios/clientes ciertos items 
#' (películas, productos de nuestra tienda, anuncios...). Con ello se consigue que el usuario tenga una mejor experiencia
#' y tarde menos en encontrar lo que necesita ... Y por otro lado hay más posibilidades que acabe
#' consumiendo/comprando nuestros productos ofertados.
#' 
#' ## Sistemas de recomendación clásicos
#' 
#' Existen multitud de sistemas de recomendaciones: estanterías en librerías, lineales de supermercado,
#' los mejores diez restaurantes de Madrid... 
#' 
#' Todos estos son sistemas no personalizados, es decir las recomendaciones son iguales para
#' todos los usuarios, y no necesitamos ninguna información de los usuarios. Este tipo de
#' recomendaciones suele ser simples pero de manera rápida nos ayuda a ordenar los items de una 
#' manera
#' eficiente.
#' 
#' Algunos ejemplos de estos sistemas son:
#' 
#' * Los más comprados/vistos.
#' * Los mejor puntuados.
#' * Valoración de expertos: Críticas de cines.
#' * Puntuaciones normalizadas: Muchas veces las métricas clásicas como "el mejor puntuado" puede tener grandes
#' sesgos, ya que puede haberlo puntuado poca gente o tener puntuaciones extremas.
#' * Reglas de asociación
#' 
#' ### Rating de IMDb y la media bayesiana
#' 
#' <center>
#' <img width="200" src="img/imdb.jpg"></img>   
#' </center> 
#' <br>
#' 
#' En algún momento todos hemos acabado consultando IMDb. En esta web además de una gran
#' base de datos de películas, actores, directores... encontramos un raking de las películas
#' mejor valoradas. En la entrada de la Wikipedia encontramos cómo realizan este ranking:
#' https://en.wikipedia.org/wiki/IMDb#Film_rankings_.28IMDb_Top_250.29.
#' 
#' Veamos con detalle cómo realizan el ranking y qué tiene de particular:
#' 
#' $$
#' {\large
#' W = \frac{Rv + Cm}{v+m}
#' }
#' $$
#' donde,
#' 
#' * $W$:&nbsp; Weighted rating.
#' * $R$:&nbsp; Average for the movie as a number from 0 to 10 (mean) = (Rating).
#' * $v$:&nbsp; Number of votes for the movie = (votes).
#' * $m$:&nbsp; Minimum votes required to be listed in the Top 250 (currently 25,000).
#' * $C$:&nbsp; The mean vote across the whole report (currently 7.0).
#' 
#' Con esta fórmula se intenta mitigar el problema de puntuaciones muy altas para películas con pocos votos.
#' Es claro que es más fácil tener un 10 si solo te ha votado una persona que tener un 10 si te han votado 1.000.
#' 
#' De esta manera se intenta reducir la credibilidad que le damos a una puntuación si la gente que ha votado ha sido poca,
#' esto es la base de la [estadística bayesiana](https://en.wikipedia.org/wiki/Bayesian_inference) donde si
#' tenemos poca muestra la credibilidad es baja y usamos una
#' distribución "aprior" que en el caso de IMDb sería $C$ la media de los votos.
#' 
#' 
#' ### El carrito de la compra
#' 
#' El "carrito de la compra" se conoce como un ejemplo típico de **reglas de asociación**.
#' 
#' Las [Reglas de asociación](https://en.wikipedia.org/wiki/Association_rule_learning) se utilizan para 
#' encontrar patrones habituales en ciertos datos. Por ejemplo qué tipos de productos se suelen 
#' comprar juntos: 
#' 
#' * Si compra $A$ suele comprar $B$ ($A\to B$).
#' * El que suele comprar $A$ y $C$ suele comprar $D$ ($\{A,C\}\to D$).
#' * ...
#' 
#' Esta técnica es usada en los linelaes del supermercado para saber qué tipo de productos poner juntos,
#' por ejemplo los frutos secos cerca de las bebidas.
#' 
#' Queremos que las reglas que encontremos sean significativas, para ello se suele
#'  utilizar como medida el
#' **soporte** y la **confianza**:
#' 
#' $$
#' {\large
#' \begin{eqnarray*}
#' supp(A) &=& \frac{|A|}{|D|} \\[1.5em]
#' conf(A\to B) &=& \frac{supp(A \cup B)}{supp(A)} 
#' \end{eqnarray*}
#' }
#' $$
#' 
#' Donde $D$ es el número total de transicciones.
#' 
#' De manera intuitiva, el soporte es la proporción de veces que
#' aparece el conjunto $A$ en el total de transacciones. Y la confianza es la proporción de veces
#' que aparece $A$ y $B$ cuando aparece $A$.
#'  
#' Con R usaremos el paquete `arules` para realizar este tipo de recomendaciones.
#' 
#' Introducimos un ejemplo sencillo (para más detalle consultar http://www.salemmarafi.com/code/market-basket-analysis-with-r/):
#' 
#' 

library(arules)
library(arulesViz)
library(datasets)

data(Groceries)
help("Groceries")

#' Vamos a usar los datos de `Groceries`, estos datos contiene las transacciones reales
#' de un supermercado durante un mes 

class(Groceries)
str(Groceries)

Groceries@data
Groceries@itemInfo

#' Los datos están en un objeto de tipo `transactions`. Dentro de este objeto hay una matriz con
#' las transacciones:
#' 
Groceries@data[1:10,1:20]

#' El paquete `arules` nos proporciona algunas funciones para explorar los datos:

itemFrequencyPlot(Groceries,topN = 20,type = "absolute",col = "#A7DBD8")

#' Usamos la función `apriori` para calcular las reglas. Normalemente se fija a
#' mano el soporte y la confianza que queremos, para ello se suele ser exigentes
#' (para conseguir pocas reglas) y poco a poco relajamos las condiciones hasta conseguir un número
#' adecuado de reglas en función de nuestros propósitos.
#' 

apriori(Groceries)    # Pruebo primero a pelo y luego voy ajustando supp y conf porque no encuentra nada (0 rules)

rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))
rules

#' Para ver las 5 primeras reglas: (cuidado no están ordenadas) 
#' support : veces que aparece lhs del total
#' conf : prob que dandose lsh se dé rhs
inspect(rules[1:15])

#' También podemos hacer un resumen del objeto:
summary(rules)


#' Podemos realizar muchas más cosas con la función `apriori`, alguna de ellas

rules <- apriori(
                data = Groceries, parameter = list(supp = 0.001,conf = 0.15,minlen = 2), 
                appearance = list(default = "rhs",lhs = "whole milk")
               )

inspect(rules)

#' Suele ser conveniente ordenar las reglas por algún parámetro, por ejmplo por la confianza:

rules <- sort(rules, decreasing = TRUE,by = "confidence")
inspect(rules)

#' Para terminar, con el paquete `arulesViz` podemos dibujar las reglas de varias maneras

plot(rules)
plot(rules,method = "graph")


#' 
#' ## Sistemas de recomendación personalizados
#' 
#' Es claro que nuestros clientes/usuarios son diferentes. Cada uno tenemos unos gustos y unas 
#' preferencias, si podemos utilizar toda la información que tengamos de nuestros usuarios 
#' podemos construir recomendaciones personalizadas a cada uno y de este modo conseguir
#' mayor calidad en nuestras recomendaciones.
#' 
#' Una de las empresas que más ha investigado en los sisemas de recomendación es Netflix y el concurso que
#' protagonizo: 
#' 
#' 
#' <center>
#' <img style="background-color:#B9090B;" width="220" src="img/netflix.gif"></img>   
#' http://www.netflixprize.com
#' </center> 
#' <br>
#' Existen varios tipos de estrategias y algoritmos en los sistemas de recomendación
#' unos de los más usados son los **filtros colaborativos**.
#' 
#' En los [**filtrados colaborativos**](https://en.wikipedia.org/wiki/Collaborative_filtering) usamos la
#' información que nos da nuestros propios usuarios para inferir y recomendar a todos ellos. Una de las
#' ventajas de estos algoritmos es que solo necesitamos la relación de nuestros usuarios y
#' los items a recomendar.
#' 
#' Norlamente se suele usar una notación matricial:    
#' 
#' 
#' $$
#' {\large 
#' M_{(n,p)} =
#' \begin{pmatrix}
#' m_{11} &  m_{12}  & \ldots & m_{1p}\\
#' m_{21}  &  m_{22} & \ldots & m_{2p}\\
#' \vdots & \vdots & \ddots & \vdots\\
#' m_{n1}  &   m_{n2}       &\ldots & m_{np}
#' \end{pmatrix}
#' }
#' $$
#' 
#' Donde,
#' 
#' * $n$: Número total de usuarios.
#' * $p$: Número total de items.
#' * $m_{ij}$: Relación entre el usuario $i$ y el item $j$.
#' A esta relación se le suele denotar como el rating usuario/item (cuantas veces lo ha consumido, valoración dada por el usuario,...)
#' 
#' En el siguiente gráfico podemos ver un ejemplo intuitivo de filtro
#'  colaborativo y la matriz definida:
#' 
#' <center>
#' <img  src="img/450px-Collaborative_filtering.gif"></img>   
#' </center> 
#' 
#' Es claro que no todos los usuarios han tenido contacto con todos los items y por lo tanto la 
#' matriz $M$ tiene muchas entradas vacías (tenemos una matriz *sparse*).
#' Así que podemos entender el problema de recomendación
#' como estimar esos huecos vacíos y predecir de algún  modo qué rating tendrá cada
#' usuario en todos los items y de este modo recomendar los items que 
#' predecimos van a ser más afines.
#' 
#' Para hacer esto existen varias extrategias y algortimos. Uno de ellos es usar la estratégia
#' del vecino más próximo (o *K-nearest neighbors* o *KNN*) donde la idea es
#' encontrar en nuestra base de datos (la matriz $M$) gente "parecida"
#' y recomendar items que gustan a esas "almas gemelas".
#' 
#' Otro de los algoritmos y que ha desmotrado tener mejores resultados (ver [Matrix Factorization Techniques for Recommender Systems
#' ](http://dl.acm.org/citation.cfm?id=1608614)) que el *KNN*
#' son los basados en factorización de matrices.
#' 
#' 
#' ### Factorización de matrices
#' 
#' Matemáticamente factorizar una matriz es encontrar un conjunto de dos o más matrices con algunas
#' propiedas cuyo producto sea nuestra matriz original.
#' 
#' Existen multitud de factorizaciones distintas usadas para distintos propósitios.
#' 
#' Una factorización clásica es la descomposición en valores singulares o *SVD*
#' (*singular value decomposition*):
#' 
#' $$
#' {\large
#' A = U \Sigma V^T
#' }
#' $$
#' donde,
#' 
#' * $A$: Matriz original de dimensión $(m \times n)$
#' * $U$ y $V$: Matrices ortogonales de $(m \times \tau)$ y $(\tau \times n)$ respectivamente.
#' * $\Sigma$: Matriz diagonal de dimensión $(\tau\times\tau)$.
#' 
#' Un esquema gráfico de la descomposición: 
#' 
#' <center>
#' <img  src="img/svd.png"></img>   
#' </center> <br>
#' 
#' Esta descomposición tiene algunas propiedades útiles
#' como por ejemplo la reducción de dimensionalidad. La matriz $\Sigma$ contiene
#' los valores singuales de mayor a menor, así que podemos quedarnos con una submatriz
#' de tamaño $(k \times k)$ por ejemplo y aunque estamos perdiendo iformación
#' (reduciendo la dimensionalidad) estamos desechando la parte "menos valiosa" de la matriz.
#' 
#' Veamos un ejemplo con una imagen del Greco. Usamos el paquete `png` para 
#' leer la imagen y la representamos.


library(png)
library(Matrix)

m <- readPNG("datos/greco.png")
class(m)
dim(m)
fivenum(m)   # Entre 0 y 1 el nivel de negros
print(object.size(m),units = "auto")
m[1:5,1:5]

#+ fig.height=6, fig.width=6
par(mar = rep(0,4))
plot(1:2,type = 'n',axes = FALSE, xlab = "", ylab = "")   # Pinta gráfica vacía
rasterImage(m, 1, 1, 2, 2)

#' Vemos que la dimensión de la matriz es $(836 \times 660)$
#' y además el objeto en la memoria de R ocupa unos 4 Mb.
#' 
#' Realizamos la descomposición con `svd`:
 
svd.m <- svd(m)    # Genera lista lista con las tres matrices UVE (cuidado porque E se guarda solo la diagonal como vector por ahorrar)

# compruebo la relación de factorización A=U*E*Vt

fivenum(svd.m$u %*% diag(svd.m$d) %*% t(svd.m$v) - m)   # Efectivamente da 0

str(svd.m)
names(svd.m)
print(object.size(svd.m),units = "Kb")

#' La función `svd` nos devuelve una lista con un vector `d` que es nuestra matriz diagonal
#' y dos matrices `u` y `v` que son las matrices ortogonales. Si nos centramos en la matriz
#' diagonal. Las primeras entradas contienen los principales valores singulares.
#' Si hacemos un gráfico acumulativo podemos ver:

plot(cumsum(svd.m$d) / sum(svd.m$d))   # Si desecho la mitad de la derecha pierdo menos info

#' Vamos ahora a reducir la dimensionalidad:

filtra.svd <- function(tmp, k){
  
  tmp$d[(k + 1):length(tmp$d)] <- 0                  # me quedo con los primeros k de la diagonal y luego al multiplicar por u y v se quedará a 0. Encima en modo sparse ni se guardará
  res <- tmp$u %*% diag(tmp$d) %*% t(tmp$v)
  
  res[res > 1] <- 1                                  # quito errores de <0 >1 por epsilon de la máquina
  res[res < 0] <- 0
  res <- Matrix(as(res, "sparseMatrix"),sparse = TRUE)
  
  plot(1:2, type = 'n',axes = FALSE, xlab = "", ylab = "")
  print(object.size(res),units = "Kb")
  rasterImage(as.matrix(res), 1, 1, 2, 2)
}

#+ fig.height=7, fig.width=7,results='hold'
par(mfrow = c(2,2),mar = rep(0,4))              # Divide la pantalla en cuatro y luego plotea cada grafica en un trozo
filtra.svd(svd.m, 1)
filtra.svd(svd.m, 5)
filtra.svd(svd.m, 30)
filtra.svd(svd.m, 100)

#' 
#' Acabamos de comprobar cómo muchas veces no toda la información de una matriz
#' es relevante y que en una matriz podemos calcular valores que nos caracterizan esa matriz.
#' 
#' La descomposición $SVD$ se ha utilizado también sobre la matriz de 
#' ratings para conseguir recomendaciones. Pero aunque la propiedad de reducir la dimensionalidad
#' es muy buena la ortogonalidad de las matrices en este caso no es una propiedad que
#' vayamos a utilizar y los signos negativos hace difícil la comprensión de los resultados.    
#' 
#' De este modo se introduce otro tipo de descomposición que es la que vamos a ver en profunidad,
#' la descomposición *Low-Rank Factorization*, que busca:
#' 
#' $$
#'  M_{(n,p)} = U_{(n,k)}V_{(k,p)}
#' $$
#' donde,   
#' 
#' $k$: Es un parámetro a definir y está asociado a las variables latentes del problema.
#' 
#' Esta factorización es un problema de optimización donde se intenta reducir la perdida de 
#' información al reducir la dimensionalidad, ya que pasamos de $(n\times p)$ a 
#' $(n\times k) + (k\times p)$ y como en general $k << p << n$ la primera matriz es
#' bastante más grande    
#' 
#' <br>
#' 
#' #### El proyecto MovieLens
#' 
#' <center>
#' <img style="background-color:#f06624;" src="img/movielens-logo-white.svg"></img>   
#' https://movielens.org
#' </center> <br>
#' 
#' El proyecto [MovieLens](https://movielens.org/) es una base de datos 
#' de películas y puntuaciones de usuarios. Su fin es puramente académico y es mantenida por
#' "GroupLens, a research lab at the University of Minnesota". Toda la base de datos está
#' disponible para uso no comercial y la podemos descargar desde
#' [aquí](http://grouplens.org/datasets/movielens/).
#' 
#' Vamos a trabajar con estos datos y el algoritmo de factorización *ALS* 
#' (*Alternating Least Squares*) que viene implementado en Spark.  
#' 
#' 
#' 
#' **NOTA:** Abrir el notebook `spark_als/Recomendaciones MovieLens.ipynb` con Spark.
#' <br><br>
#' 