# Importando las librerías necesarias
library(arules)
library(plyr)
library(arulesViz)

# Leer el archivo por analizar
Datos_a_priori <- read.csv("lastfm1.csv", sep=";", dec=".", header=TRUE)

# Las transacciones deben ser numéricas y los productos factores
str(Datos_a_priori)

# Damos el formato de transacción a Datos_a_priori
# Por cada transacción (usuario) se crea una fila conteniendo todos los productos (artistas) separados con una coma
Lista_artistas <- ddply(Datos_a_priori, c("user"), function(df1) paste(df1$artist, collapse=","))

# Quitamos la columna Transaccion (user)
Lista_artistas$user <- NULL

# Guardamos los productos como CSV
# Es muy importante incluir row.names=FALSE para que no agregue una columna con el número de fila
# quote=FALSE dejará un producto por celda
write.csv(Lista_artistas, "Lista_artistas.csv", quote=FALSE, row.names=FALSE)

# Leemos el archivo con las transacciones que se escribieron
transacciones <- read.transactions("Lista_artistas.csv", format="basket", sep=",", header=TRUE)

# Desplegamos las transacciones en la consola
inspect(transacciones)

# Graficamos los productos que se han vendido más (artistas más escuchados)
itemFrequencyPlot(transacciones, topN=10, type='absolute')

# Obtenemos las reglas de asociación por medio del algoritmo apriori
reglas <- apriori(transacciones, parameter=list(supp=0.04, conf=0.3, minlen=2))

# Ordenamos las reglas en base a su confianza
reglas <- sort(reglas, by="confidence", decreasing=TRUE)

# Viendo la estructura del objeto reglas
str(reglas)

# Desplegamos las reglas
inspect(reglas)

# Revisamos si hay reglas duplicadas
duplicated(reglas)

# Verificamos si hay reglas redundantes
redundantes <- is.redundant(reglas)
redundantes

# Ver cuales son las reglas redundantesm
which(redundantes)

# Quitar las reglas redundantes
reglas_podadas <- reglas[!redundantes]

# Desplegamos las reglas sin redundancia
inspect(reglas_podadas)

# Creamos un grafo con las reglas
plot(reglas_podadas, method="graph", engine="interactive", shading="confidence")
