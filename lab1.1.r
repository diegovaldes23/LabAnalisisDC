# Laboratorio N°1 
# Integrantes : Diego Valdés y Valentina Campos
# Grupo N° 1
# Datos. Diabates

# Parte 1 : Cargar los datos

# Cargar librerías necesarias
library(tidyverse)
library(dplyr)
library(lmtest)
library(sandwich)
# Cargar el conjunto de datos
diabetes_data <- read.csv("Escritorio/labanalisis/archive/diabetes.csv")

# Mostrar las primeras filas del conjunto de datos
head(diabetes_data)

# Estructura de los datos
str(diabetes_data)

# Resumen estadístico básico de todas las variables
summary(diabetes_data)

# Al analizar el resumen estadístico se destaca que los valores de 0 en las variables como Glucose,
# BloodPressure, SkinThickness, Insulin, y BMI probablemente representan datos faltantes. 
# Esto es importante para limpiar los datos antes de realizar cualquier análisis inferencial,ya que, se pueden malinterpretar 
# el análisis se se colocan estos datos.
#--- 
# Parte 2 : Limpieza de los datos
# Reemplazar valores de 0 por NA en las variables específicas
diabetes_data$Glucose[diabetes_data$Glucose == 0] <- NA
diabetes_data$BloodPressure[diabetes_data$BloodPressure == 0] <- NA
diabetes_data$SkinThickness[diabetes_data$SkinThickness == 0] <- NA
diabetes_data$Insulin[diabetes_data$Insulin == 0] <- NA
diabetes_data$BMI[diabetes_data$BMI == 0] <- NA

# Eliminar filas con cualquier NA
diabetes_data <- na.omit(diabetes_data)

# Resumen estadístico básico de todas las variables
summary(diabetes_data)



# Funciones media, mediana, desviación estándar y varianza a todas las columnas numéricas
sapply(diabetes_data, function(x) c(Media = mean(x, na.rm = TRUE), 
                                    Mediana = median(x, na.rm = TRUE),
                                    Desviación = sd(x, na.rm = TRUE), 
                                    Varianza = var(x, na.rm = TRUE)))

# Contar el número de filas (mujeres) en el conjunto de datos 
nrow(diabetes_data)

# Contiene datos de 392 mujeres

# Tras limpiar los datos se puede continuar con el análisis de los datos

# Parte 3: Visualización de la distribución de probabilidades

# Histograma con curva de densidad para Glucosa
hist(diabetes_data$Glucose, probability = TRUE, main = "Distribución de Glucosa", col = "lightblue", border = "black")
lines(density(diabetes_data$Glucose, na.rm = TRUE), col = "blue", lwd = 2)

# Histograma con curva de densidad para Presión Sanguínea
hist(diabetes_data$BloodPressure, probability = TRUE, main = "Distribución de Presión Sanguínea", col = "lightgreen", border = "black")
lines(density(diabetes_data$BloodPressure, na.rm = TRUE), col = "green", lwd = 2)

# Histograma con curva de densidad para Grosor de Piel
hist(diabetes_data$SkinThickness, probability = TRUE, main = "Distribución de Grosor de Piel", col = "lightpink", border = "black")
lines(density(diabetes_data$SkinThickness, na.rm = TRUE), col = "red", lwd = 2)

# Histograma con curva de densidad para Insulina
hist(diabetes_data$Insulin, probability = TRUE, main = "Distribución de Insulina", col = "lightyellow", border = "black")
lines(density(diabetes_data$Insulin, na.rm = TRUE), col = "orange", lwd = 2)

# Histograma con curva de densidad para BMI
hist(diabetes_data$BMI, probability = TRUE, main = "Distribución de BMI", col = "lightcyan", border = "black")
lines(density(diabetes_data$BMI, na.rm = TRUE), col = "cyan", lwd = 2)

# Histograma con curva de densidad para Edad
hist(diabetes_data$Age, probability = TRUE, main = "Distribución de Edad", col = "lightgray", border = "black")
lines(density(diabetes_data$Age, na.rm = TRUE), col = "darkgray", lwd = 2)

# Tras analizar el histograma se realiza una prueba de normalidad a las variables
# Para saber con que pruebas seguir, si paramétricas o no paramétricas o métodos robustos.
# Con shapiro-Wilk porque esta prueba es recomendable para muestras pequeñas

# Prueba de normalidad de Shapiro-Wilk para las variables 

shapiro.test(diabetes_data$Glucose)
shapiro.test(diabetes_data$BloodPressure)
shapiro.test(diabetes_data$SkinThickness)
shapiro.test(diabetes_data$Insulin)
shapiro.test(diabetes_data$BMI)
shapiro.test(diabetes_data$Age)

# Variable -> p-valor	
# Glucose	      3.442e-08	   No es normal
# BloodPressure	0.008712	   No es normal
# SkinThickness	0.001991	   No es normal
# Insulin	      < 2.2e-16	   No es normal
# BMI	          1.657e-06	   No es normal
# Age	          < 2.2e-16	   No es normal

# Ninguna de las variables sigue con una distribución normal.
# Ahora se realizara la prueba de homocedasticidad
# Hipótesis nula (H0): Los residuos tienen una varianza constante (homocedasticidad).
# Hipótesis alternativa (H1): Los residuos no tienen varianza constante (heterocedasticidad).

# Prueba de Breusch-Pagan para homocedasticidad

modelo <- lm(Glucose ~ Age + BMI, data = diabetes_data)
bptest(modelo)


# p-value = 0.0138: Como el valor p es menor que 0.05, rechazas la hipótesis 
# nula y concluyes que existe heterocedasticidad en el modelo.

# Tras relizar las pruebas y verificar que no hay nomralidad ni homocedasticidad en los datos
# Se procederá a realizar métodos robustos

coeftest(modelo, vcov = vcovHC(modelo, type = "HC1"))

