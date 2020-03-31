
## TEST DE HIPOTESIS MEDIANTE RSTUDIO
## Docente: Rodrigo Del Rosso

# Limpiar la consola
rm(list = ls())

# Setear la ruta de trabajo
path = "C:/Users/rdelr/OneDrive - Facultad de Ciencias Económicas - Universidad de Buenos Aires/751 - 1C2019/RStudio/Clases/Clase 3 - 03-04/"
setwd(path)

# Comparación de medias y varianzas - 2 grupos

# t.test(): comparación de medias, prueba paramétrica
# wilcox.test(): comparación de medias, prueba no paramétrica
# var.test(): comparación de varianzas entre 2 muestras
# bartlett.test(): comparación de varianzas entre 2 o más muestras

pre <- c(0.7, -1.6, -0.2, -1.2, -0.1, 3.4, 3.7, 0.8, 0.0, 2.0)

post <- c(1.9, 0.8, 1.1, 0.1, -0.1, 4.4, 5.5, 1.6, 4.6, 3.4)

# Comparación de medias de datos emparejados (antes - después)
t.test(pre, post, paired = TRUE)

# Comparación de medias de datos emparejados (antes - después)
wilcox.test(pre, post, paired = TRUE)

# Comparación de medias de datos emparejados (antes - después): 
# diferencia frente a 0
diff <- pre - post
hist(diff)

# Comparación de medias de datos emparejados (antes - después): t-test
t.test(diff, mu = 0)

# Comparación de medias de datos emparejados (antes - después): wilcoxon
wilcox.test(diff, mu = 0)

# Comparación de medias entre 2 grupos independientes: datos
?sleep
data(sleep)
boxplot(extra ~ factor(group), data = sleep)

# Comparación de medias entre 2 grupos independientes: igualdad de varianzas
bartlett.test(extra ~ factor(group), data = sleep) # igualdad de varianzas

# Comparación de medias entre 2 grupos independientes: t-test asumiendo varianzas iguales
t.test(extra ~ factor(group), data = sleep, var.equal = TRUE)

# Comparación de medias entre 2 grupos independientes: t-test sin asumir igualdad de varianzas
t.test(extra ~ factor(group), data = sleep)

# Comparación de medias entre 2 grupos independientes: wilcoxon
wilcox.test(extra ~ factor(group), data = sleep)

# Diferencia de medias entre más de 2 grupos: ANOVA
# oneway.test(): análisis de la varianza de 1 vía, paramétrico
# kruskal.test(): análisis de la varianza de 1 vía, no paramétrico
# pairwise.t.test(): múltiples comparaciones (paramétrico)
# pairwise.wilcox.test: múltiples comparaciones (no paramétrico)
# aov(): anális de la varianza en general

# Índice de pulsatilidad en la arteria cerebral 
# media según exposición a cannabis

grupo <- c("control", "control", "control", "control", "control", "control", "activo", "activo", "activo", "activo", "activo", "activo", "ex", "ex", "ex", "ex", "ex", "ex")
index <- c(.9, .9, .9, .8, .8, NA, 1, 1, 1, 1, .9, NA, 1, 1, 1, 1, .9, .9)
pulsatilidad <- data.frame(grupo, index)
pulsatilidad

boxplot(index ~ grupo, data = pulsatilidad)

# ANOVA: igualdad de varianzas
bartlett.test(index ~ grupo, data = pulsatilidad)

# ANOVA: omnibus test
oneway.test(index ~ grupo, data = pulsatilidad)

# ANOVA: corrección para múltiples tests
pairwise.t.test(pulsatilidad$index, pulsatilidad$grupo)

# ANOVA: omnibus test con la función aov()
aov.res <- aov(index ~ grupo, data = pulsatilidad) # ANOVA clásico
summary(aov.res)

# ANOVA: corrección para múltiples tests
TukeyHSD(aov.res)

# ANOVA: gráfico para múltiples tests
plot(TukeyHSD(aov.res))

# ANOVA: prueba no paramétrica
kruskal.test(index ~ grupo, data = pulsatilidad)

# ANOVA no paramétrico: corrección para múltiples tests
pairwise.wilcox.test(index, grupo, data = pulsatilidad)

# Correlación: funciones en R
# cor(): correlaciones paramétricas y no paramétricas
# cor.test(): test de significación de la correlación
# cov(): covarianza entre 2 variables

# Correlación lineal: Anscombe 1
?anscombe # base de datos histórica en **R**
data(anscombe)

cor(anscombe$x1, anscombe$y1)
cor.test(anscombe$x1, anscombe$y1)
plot(anscombe$x1, anscombe$y1)
abline(lm(y1 ~ x1, data=anscombe))

# Correlación lineal: Anscombe 2
cor.test(anscombe$x2, anscombe$y2)
plot(anscombe$x2, anscombe$y2)
abline(lm(y2 ~ x2, data=anscombe))

# Correlación lineal: Anscombe 3
cor.test(anscombe$x3, anscombe$y3)
plot(anscombe$x3, anscombe$y3)
abline(lm(y3 ~ x3, data=anscombe))

# Correlación lineal: Anscombe 4
cor.test(anscombe$x4, anscombe$y4)
plot(anscombe$x4, anscombe$y4)
abline(lm(y4 ~ x4, data=anscombe))

# Correlación no implica causación: nidos de cigüeñas y nacimientos
births <- c(4, 20, 60, 30, 7, 6, 14, 20, 18) # número de nacimientos
nests <- c(2, 6, 8, 7, 0, 1, 2, 2, 3) # número de nidos
plot(nests, births) # ¿hay asociación?

cor.test(nests, births) # correlación de Pearson por defecto
cor.test(nests, births, method = "spearman")
cor.test(nests, births, method = "kendall")
