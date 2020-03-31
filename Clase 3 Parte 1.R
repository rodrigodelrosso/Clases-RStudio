
## INTRODUCCIÓN A TEST DE HIPOTESIS MEDIANTE RSTUDIO
## Docente: Rodrigo Del Rosso

# Limpiar la consola
rm(list = ls())

# Setear la ruta de trabajo
path = "C:/Users/rdelr/OneDrive - Facultad de Ciencias Económicas - Universidad de Buenos Aires/751 - 1C2019/RStudio/Clases/Clase 3 - 03-04/"
setwd(path)

# Para la media de una población normal
# Generamos una muestra de tamaño 30 de una distribución normal 
# con media 5 y desvío 1. 

set.seed(seed = 34)        # Fijamos la semilla
x <- rnorm(n = 30,mean = 5,sd = 1)  # Generamos una normal con media 5 y desvío 1
t.test(x = x,mu = 5)

# Podemos incorporar más argumentos

t.test(x = x, 
       alternative = "two.sided",
       mu = 5,
       conf.level = 0.99)

# Si planteamos un test unilateral a izquierda

t.test(x = x, 
       alternative = "less",
       mu = 5,
       conf.level = 0.95)

# Si planteamos un test unilateral a derecha para mu=4 y nivel de confianza 0.99

t.test(x = x, 
       alternative = "greater",
       mu = 4,
       conf.level = 0.99)

# Para comparar la media de dos poblaciones normales independientes

# Por default es un test bilateral, y  un intervalo de confianza del 95%, diferencia 0 y varianzas distintas.

# Asumiendo varianzas iguales
# Por ejemplo, generamos otra muestra de tamaño 25 
# de una distribución normal con media 6 y desvío 1. 

set.seed(seed = 35)
y <- rnorm(n = 25,mean = 6,sd = 1)
t.test(x = x,
       y = y,
       var.equal = TRUE)

# Si queremos ver si la media de x es menor en una unidad que la media de y

t.test(x = x,
       y = y,
       mu=1,
       alternative = "less",
       var.equal = TRUE)

# Cuando la variable de interés está definida mediante un factor

# Generamos el data frame datos 

set.seed(seed = 20)
variable <- rnorm(n = 40,mean = 20,sd = 2)

set.seed(20)
sexo <- sample(x = c("F","M"),
               size = 40,
               replace=TRUE)

datos <- data.frame(variable,sexo)

t.test(variable ~ sexo, 
       data = datos,
       var.equal = TRUE)

# Asumiendo varianzas distintas (Test de Welch)

set.seed(seed = 29)
x1<-rnorm(n = 25,mean = 5,sd = 0.5)

set.seed(seed = 30)
x2 <- rnorm(n = 20,mean = 5.2,sd = 1)

t.test(x = x1,y = x2)

# Para muestras apareadas

set.seed(seed = 29)
w1<-rnorm(n = 25,mean = 5,sd = 0.5)

set.seed(seed = 30)
w2 <- rnorm(n = 25,mean = 3,sd = 1)

t.test(x = w1,
       y = w2,
       paired = TRUE)  ## con la instrucción paired igual TRUE le indicó que las muestras son apareadas

# Test F para igualdad de varianzas
# La instrucción en R para aplicar el test F es  var.test
# El default es un test bilateral para cociente 1  y  un intervalo de confianza del 95%
# Chequeamos el supuesto para los test realizados anteriormente.

var.test(x = x,y = y)

var.test(x = x1,y = x2)

var.test(variable ~ sexo, 
         data = datos)  ### el símbolo para categorizar "variable" por "sexo"

#Ver el help para otras opciones

# Test de Shapiro Wilks 
# Pone a prueba el supuesto de normalidad.
# Para los ejemplos vistos

# Ejemplo 1
shapiro.test(x = x)

# Ejemplo 2
shapiro.test(x = y)

# Ejemplo 3

# Para muestras apareadas la diferencia 
# debe tener distribución normal 
# el supuesto es que la diferencia tiene distribución normal

shapiro.test(x = w1 - w2)

# Ejemplo 15, los grupos están dados por un un factor

tapply(X = datos$variable,
       INDEX = datos$sexo,
       FUN = shapiro.test)

## PARA DOS POBLACIONES

x = c(418,421,421,422,425,427,431,434,437,439,446,447,448,453,454,463,465)          
length(x)

y = c(429,430,430,431,36,437,440,441,445,446,447)
length(y)

test2 <- t.test(x = x,
                y = y,
                alternative = "two.sided",
                mu = 0,
                var.equal = F,
                conf.level = 0.95)

test2$p.value
test2$statistic

test3 = shapiro.test(x = x)

fila1 = rbind(test2$p.value,test2$statistic)
fila2 = rbind(test3$p.value,test3$statistic)
tabla = cbind(fila1,fila2)
colnames(tabla) = c("Diferencia","Shapiro")
rownames(tabla) = c("P-Value","Estadístico")
tabla

write.csv(x = tabla, file = paste0(path,"Prueba.csv"))

