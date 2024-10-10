# Laboratorio N°1 
# Integrantes: Diego Valdés y Valentina Campos
# Grupo N° 1
# Datos: Diabates

# Parte 1: Cargar los datos

# Cargar librerías necesarias
library(tidyverse)
library(dplyr)
library(lmtest)
library(sandwich)

# Obtener la ruta del directorio de trabajo actual
current_dir <- getwd()

# Buscar el archivo "diabetes.csv" en el directorio actual y subdirectorios
file_name <- "diabetes.csv"
file_path <- list.files(path = current_dir, pattern = file_name, recursive = TRUE, full.names = TRUE)

# Cargar el archivo si existe
if (length(file_path) > 0) {
  diabetes_data <- read.csv(file_path[1])
  print("Archivo cargado correctamente.")
} else {
  print("El archivo no fue encontrado.")
}

# Mostrar las primeras filas del conjunto de datos
head(diabetes_data)

# Estructura de los datos
str(diabetes_data)

# Resumen estadístico básico de todas las variables
summary(diabetes_data)

# Parte 2: Limpieza de datos
# Al analizar el resumen estadístico se destaca que los valores de 0 en las variables como Glucose,
# BloodPressure, SkinThickness, Insulin, y BMI probablemente representan datos faltantes. 
# Esto es importante para limpiar los datos antes de realizar cualquier análisis inferencial, ya que, se puede malinterpretar 
# el análisis si se colocan estos datos.
# Por lo tanto, reemplazar valores de 0 por NA en las variables específicas.
diabetes_data$Glucose[diabetes_data$Glucose == 0] <- NA
diabetes_data$BloodPressure[diabetes_data$BloodPressure == 0] <- NA
diabetes_data$SkinThickness[diabetes_data$SkinThickness == 0] <- NA
diabetes_data$Insulin[diabetes_data$Insulin == 0] <- NA
diabetes_data$BMI[diabetes_data$BMI == 0] <- NA

# Eliminar filas con cualquier NA
diabetes_data <- na.omit(diabetes_data)

# Resumen estadístico básico de todas las variables, con los datos limpios.
summary(diabetes_data)

# Parte 3: Estadísticas básicas
# Funciones media, mediana, desviación estándar y varianza a todas las columnas numéricas
sapply(diabetes_data, function(x) c(Media = mean(x, na.rm = TRUE), 
                                    Mediana = median(x, na.rm = TRUE),
                                    Desviación = sd(x, na.rm = TRUE), 
                                    Varianza = var(x, na.rm = TRUE)))

# Contar el número de filas (mujeres) en el conjunto de datos 
nrow(diabetes_data)

# Parte 4: Visualización de la distribución de las variables

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

# Tras analizar el histograma se realiza una prueba de normalidad a las variables, 
# para saber con que pruebas seguir, si paramétricas o no paramétricas o métodos robustos.
# Con shapiro-Wilk porque esta prueba es recomendable para muestras pequeñas.

# Prueba de normalidad de Shapiro-Wilk para las variables.
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

# Parte 5: Prueba no paramétrica (Wilcoxon) para todas las variables numéricas

# Extraer las variables numéricas del conjunto de datos
variables_numericas <- diabetes_data %>% select_if(is.numeric)

# Aplicar la prueba de Wilcoxon para cada par de variables numéricas
resultados_wilcoxon <- combn(names(variables_numericas), 2, function(vars) {
  # Realizar la prueba de Wilcoxon entre dos variables
  prueba <- wilcox.test(variables_numericas[[vars[1]]], variables_numericas[[vars[2]]])
  
  # Guardar el nombre de las variables y el valor p
  list(
    variable_1 = vars[1],
    variable_2 = vars[2],
    p_value = prueba$p.value
  )
}, simplify = FALSE)

# Mostrar los resultados
for (res in resultados_wilcoxon) {
  cat("Comparación entre", res$variable_1, "y", res$variable_2, "-> p-valor:", res$p_value, "\n")
}

# Existen variables con p-valores extremadamente bajos, estas muestran diferencias
# significativas y pueden ser consideradas posteriormente.

# Asegurarse de que Outcome es un factor
diabetes_data$Outcome <- factor(diabetes_data$Outcome)

# Ajustar el modelo de regresión logística
modelo_logistico <- glm(Outcome ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age, 
                        data = diabetes_data, 
                        family = binomial)

# Resumen del modelo
summary(modelo_logistico)

# Predecir probabilidades de tener diabetes (Outcome = 1)
prob_predicciones <- predict(modelo_logistico, type = "response")

# Convertir probabilidades a predicciones binarias (0 o 1)
predicciones_binarias <- ifelse(prob_predicciones > 0.35, 1, 0)

# Matriz de confusión
matriz_confusion <- table(Predicho = predicciones_binarias, Real = diabetes_data$Outcome)

# Calcular sensibilidad y especificidad
VP <- matriz_confusion[2, 2]  # Verdaderos positivos (Outcome = 1 y predicho = 1)
VN <- matriz_confusion[1, 1]  # Verdaderos negativos (Outcome = 0 y predicho = 0)
FP <- matriz_confusion[2, 1]  # Falsos positivos (Outcome = 0 pero predicho = 1)
FN <- matriz_confusion[1, 2]  # Falsos negativos (Outcome = 1 pero predicho = 0)

# Sensibilidad
sensibilidad <- VP / (VP + FN)
cat("Sensibilidad:", round(sensibilidad, 4), "\n")

# Especificidad
especificidad <- VN / (VN + FP)
cat("Especificidad:", round(especificidad, 4), "\n")

# Calcular la precisión del modelo
precision <- mean(predicciones_binarias == diabetes_data$Outcome)
print(paste("Precisión del modelo:", round(precision, 4)))

# También se pueden calcular métricas como Sensibilidad y Especificidad.

# Instalar y cargar el paquete pROC para la curva ROC
library(pROC)

# Calcular la curva ROC y AUC
roc_obj <- roc(diabetes_data$Outcome, prob_predicciones)
plot(roc_obj)
auc(roc_obj)

# Encontrar el mejor umbral según la curva ROC
coords(roc_obj, "best", ret = "threshold")

# Aplicar el nuevo umbral óptimo
nuevo_umbral <- 0.2853508

# Convertir probabilidades a predicciones binarias usando el nuevo umbral
predicciones_binarias_optimo <- ifelse(prob_predicciones > nuevo_umbral, 1, 0)

# Matriz de confusión con el nuevo umbral
matriz_confusion_optimo <- table(Predicho = predicciones_binarias_optimo, Real = diabetes_data$Outcome)

# Mostrar la nueva matriz de confusión
print(matriz_confusion_optimo)

# Calcular la nueva sensibilidad y especificidad
VP_optimo <- matriz_confusion_optimo[2, 2]  # Verdaderos positivos
VN_optimo <- matriz_confusion_optimo[1, 1]  # Verdaderos negativos
FP_optimo <- matriz_confusion_optimo[2, 1]  # Falsos positivos
FN_optimo <- matriz_confusion_optimo[1, 2]  # Falsos negativos

# Sensibilidad
sensibilidad_optima <- VP_optimo / (VP_optimo + FN_optimo)
cat("Nueva Sensibilidad:", round(sensibilidad_optima, 4), "\n")

# Especificidad
especificidad_optima <- VN_optimo / (VN_optimo + FP_optimo)
cat("Nueva Especificidad:", round(especificidad_optima, 4), "\n")

# Calcular la precisión del modelo con el nuevo umbral
precision_optima <- mean(predicciones_binarias_optimo == diabetes_data$Outcome)
cat("Nueva Precisión del modelo:", round(precision_optima, 4), "\n")

# Instalar y cargar el paquete caret para validación cruzada
install.packages("caret")
library(caret)

# Configurar los parámetros de validación cruzada
control <- trainControl(method = "cv", number = 10)  # 10 pliegues

# Ajustar el modelo con validación cruzada
modelo_cv <- train(Outcome ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age, 
                   data = diabetes_data, 
                   method = "glm", 
                   family = binomial, 
                   trControl = control)

# Mostrar los resultados de la validación cruzada
print(modelo_cv)

# Mostrar la importancia de los predictores
summary(modelo_logistico)

# Ajustar un nuevo modelo sin las variables no significativas
modelo_logistico_reducido <- glm(Outcome ~ Glucose + BMI + DiabetesPedigreeFunction + Age, 
                                 data = diabetes_data, 
                                 family = binomial)
summary(modelo_logistico_reducido)

