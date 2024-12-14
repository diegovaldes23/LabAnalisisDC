#Importar librerías
library(C50)
library(ggplot2)
library(caret)
library(rpart.plot)
library(party)
library(reshape2)
library(dplyr)
library(scales)

# Obtener la ruta del directorio de trabajo actual
current_dir <- getwd()

setwd("~/Desktop/LabAnalisisDC")

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

# Refactorizar el Outcome
diabetes_data$Outcome <- as.factor(diabetes_data$Outcome)

# PREPROCESAMIENTO

# # Calcular el número y porcentaje de ceros en cada columna
valores_cero <- sapply(diabetes_data, function(col) sum(col == 0, na.rm = TRUE))
porcentaje_cero <- (valores_cero / nrow(diabetes_data)) * 100

# Crear un data frame para visualizar los resultados
analisis_ceros <- data.frame(Variable = names(diabetes_data),
                             ValoresCero = valores_cero,
                             PorcentajeCero = porcentaje_cero)

# Mostrar las variables con valores cero y sus porcentajes
print(analisis_ceros)

# IMPUTACIÓN
# Definir función para reemplazar ceros por la mediana
reemplazar_con_mediana <- function(data, columnas) {
  for (col in columnas) {
    # Calcular la mediana excluyendo ceros
    mediana <- median(data[[col]][data[[col]] != 0], na.rm = TRUE)
    # Reemplazar ceros con la mediana
    data[[col]][data[[col]] == 0] <- mediana
  }
  return(data)
}

# Especificar las columnas a procesar
columnas_a_procesar <- c("Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI")

# Aplicar la función al dataset
diabetes_data <- reemplazar_con_mediana(diabetes_data, columnas_a_procesar)

# Verificar resultados
summary(diabetes_data)

# Análisis exploratorio
ggplot(diabetes_data, aes(x = Glucose)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Distribución de Glucosa", x = "Glucosa", y = "Frecuencia")

ggplot(diabetes_data, aes(x = Pregnancies)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Distribución de Embarazos", x = "Embarazos", y = "Frecuencia")

ggplot(diabetes_data, aes(x = BloodPressure)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Distribución de Presión Arterial", x = "Presión Arterial", y = "Frecuencia")

ggplot(diabetes_data, aes(x = SkinThickness)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Distribución de Grosor de Piel", x = "Grosor de Piel", y = "Frecuencia")

ggplot(diabetes_data, aes(x = Insulin)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Distribución de Insulina", x = "Insulina", y = "Frecuencia")

ggplot(diabetes_data, aes(x = BMI)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Distribución de IMC", x = "IMC", y = "Frecuencia")

ggplot(diabetes_data, aes(x = DiabetesPedigreeFunction)) + 
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black", alpha = 0.7) + 
  labs(title = "Distribución de la Función de Pedigrí de Diabetes", x = "Diabetes Pedigree Function", y = "Frecuencia") + 
  theme_minimal()

ggplot(diabetes_data, aes(x = Age)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Distribución de Edad", x = "Edad", y = "Frecuencia")

ggplot(diabetes_data, aes(x = Outcome)) + 
  geom_bar(fill = "salmon", color = "black") + 
  labs(title = "Distribución de Outcome", x = "Resultado de Diabetes", y = "Frecuencia")

# Estadísticas descriptivas de las variables
summary(diabetes_data)

# Predicción en el conjunto de prueba
predicciones <- predict(modelo_c50, test_data)

# Crear una matriz de confusión
confusion_matrix <- confusionMatrix(predicciones, test_data$Outcome)

# Mostrar resultados de la matriz de confusión
print(confusion_matrix)

# Dividir los datos en conjunto de entrenamiento y prueba
set.seed(42) # Para reproducibilidad

indices <- createDataPartition(diabetes_data$Outcome, p = 0.7, list = FALSE)
train_data <- diabetes_data[indices, ] # EXPLICAR POR QUÉ SE ESCOGIÓ EL 70% DE LOS DATOS PARA EL ENTRENAMIENTO
test_data <- diabetes_data[-indices, ]

# Falsos positivos y negativos
false_positive <- which(predicciones == 1 & test_data$Outcome == 0)
false_negative <- which(predicciones == 0 & test_data$Outcome == 1)

# Mostrar los ejemplos mal clasificados
print(test_data[false_positive, ])
print(test_data[false_negative, ])

# Proporción de clases en el conjunto de entrenamiento y prueba
table(train_data$Outcome) 
table(test_data$Outcome)

# Entrenamiento del modelo con C5.0
modelo_c50 <- C5.0(Outcome ~ ., data = train_data, trials = 10, rules = TRUE, minCases = 5)

# Resumen del modelo
summary(modelo_c50)

# Predicción en el conjunto de prueba
predicciones <- predict(modelo_c50, test_data)

# Crear una matriz de confusión
confusion_matrix <- confusionMatrix(predicciones, test_data$Outcome)

# Mostrar resultados de la matriz de confusión
print(confusion_matrix)

# Poda del modelo C5.0
modelo_c50_podado <- C5.0(Outcome ~ ., data = train_data, trials = 5, rules = TRUE, minCases = 10)

# Resumen del modelo podado
summary(modelo_c50_podado)

# Conversión de los modelos
modelo_ctree <- ctree(Outcome ~ ., data = train_data)
modelo_ctree_podado <- ctree(
  Outcome ~ ., 
  data = train_data,
  controls = ctree_control(
    minbucket = 10, # Mínimo de observaciones por hoja
    minsplit = 20,  # Mínimo de observaciones para realizar una división
    maxdepth = 5    # Profundidad máxima del árbol (ajustar según sea necesario)
  )
)

# Graficar modelos
plot(modelo_ctree)
plot(modelo_ctree_podado)


# Extraer las reglas generadas por el modelo
reglas <- modelo_c50_podado$rules
print(reglas)

# Importancia de las variables
importancia <- C5imp(modelo_c50_podado)
print(importancia)

# Comparación de modelo original y poda
precision_original <- confusionMatrix(predict(modelo_c50, test_data), test_data$Outcome)$byClass['Pos Pred Value']
recall_original <- confusionMatrix(predict(modelo_c50, test_data), test_data$Outcome)$byClass['Sensitivity']
f1_score_original <- 2 * (precision_original * recall_original) / (precision_original + recall_original)

precision_podado <- confusionMatrix(predict(modelo_c50_podado, test_data), test_data$Outcome)$byClass['Pos Pred Value']
recall_podado <- confusionMatrix(predict(modelo_c50_podado, test_data), test_data$Outcome)$byClass['Sensitivity']
f1_score_podado <- 2 * (precision_podado * recall_podado) / (precision_podado + recall_podado)

# Calcular Accuracy para el modelo original
accuracy_original <- confusionMatrix(predict(modelo_c50, test_data), test_data$Outcome)$overall['Accuracy']

# Calcular Accuracy para el modelo podado
accuracy_podado <- confusionMatrix(predict(modelo_c50_podado, test_data), test_data$Outcome)$overall['Accuracy']

# Crear un data frame con las métricas
metrics <- data.frame(
  Modelo = c("Original", "Podado"),
  Accuracy = c(accuracy_original, accuracy_podado),
  Precision = c(precision_original, precision_podado),
  Recall = c(recall_original, recall_podado),
  F1_Score = c(f1_score_original, f1_score_podado)
)

# Comparar las métricas con gráficos
metrics_long <- reshape(metrics, varying = c("Accuracy", "Precision", "Recall", "F1_Score"),
                        v.names = "Valor", timevar = "Métrica", times = c("Accuracy", "Precision", "Recall", "F1_Score"),
                        direction = "long")

# Gráfico de comparación de métricas
ggplot(metrics_long, aes(x = Modelo, y = Valor, fill = Métrica)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Comparación de Métricas entre el Modelo Original y Podado", x = "Modelo", y = "Valor") +
  facet_wrap(~Métrica, scales = "free_y") +
  theme_minimal()

# Crear gráfico de importancia de las variables
importancia <- C5imp(modelo_c50_podado)
importance_df <- data.frame(Variable = rownames(importancia), Importancia = importancia[, 1])

ggplot(importance_df, aes(x = reorder(Variable, Importancia), y = Importancia)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  coord_flip() +
  labs(title = "Importancia de las variables", x = "Variable", y = "Importancia")

