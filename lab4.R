#Importar librerías
library(C50)
library(ggplot2)
library(caret)
library(rpart.plot)
library(party)
library(reshape2)
library(dplyr)
library(scales)
library(pROC)

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

# Selección de columnas numéricas
variables_numericas <- diabetes_data[sapply(diabetes_data, is.numeric)]

# Calcular la matriz de correlación
matriz_correlacion <- cor(variables_numericas, use = "complete.obs")

# Ver la matriz de correlación
print(matriz_correlacion)

# Graficar matriz de correlacion
correlacion_larga <- melt(matriz_correlacion)
ggplot(data = correlacion_larga, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), name = "Correlación") +
  theme_minimal() +
  labs(title = "Matriz de Correlación", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Discretización de las variables (CON FUENTES)
diabetes_data$Glucose <- cut(diabetes_data$Glucose, breaks = c(-Inf, 140, Inf), 
                             labels = c("Normal", "Alto"))
diabetes_data$BMI <- cut(diabetes_data$BMI, breaks = c(-Inf, 18.5, 25, 30, Inf), 
                         labels = c("Bajo", "Normal", "Sobrepeso", "Obesidad"))
diabetes_data$Insulin <- cut(diabetes_data$Insulin, breaks = c(-Inf, 25, 85, Inf),
                             labels = c("Normal", "Alto", "Muy alto"))
diabetes_data$BloodPressure <- cut(diabetes_data$BloodPressure, breaks = c(-Inf, 80, 90, Inf), 
                                   labels = c("Normal", "Alta", "Muy alta"))
diabetes_data$Pregnancies <- cut(diabetes_data$Pregnancies, breaks = c(-Inf, 2, 5, Inf), 
                                 labels = c("Pocos", "Moderados", "Muchos"))
###################################################################################
# FALTA JUSTIFICAR ACÁ POR QUÉ SE ESCOGIERON LOS VALORES CON CITAS DE RESVISTAS IMPORTANTES

diabetes_data$SkinThickness <- cut(diabetes_data$SkinThickness, breaks = c(-Inf, 20, 30, Inf), 
                                   labels = c("Bajo", "Medio", "Alto"))
diabetes_data$DiabetesPedigreeFunction <- cut(diabetes_data$DiabetesPedigreeFunction, breaks = c(-Inf, 0.2, 0.5, Inf), 
                                              labels = c("Baja", "Media", "Alta"))
diabetes_data$Age <- cut(diabetes_data$Age, breaks = c(-Inf, 35, 50, Inf), 
                         labels = c("Joven", "Mediana", "Mayor"))
diabetes_data$Outcome <- as.factor(diabetes_data$Outcome)

summary(diabetes_data)

# Dividir los datos en conjunto de entrenamiento y prueba
set.seed(42) # Para reproducibilidad

indices <- createDataPartition(diabetes_data$Outcome, p = 0.7, list = FALSE)
train_data <- diabetes_data[indices, ] # EXPLICAR POR QUÉ SE ESCOGIÓ EL 70% DE LOS DATOS PARA EL ENTRENAMIENTO
test_data <- diabetes_data[-indices, ]

# Proporción de clases en el conjunto de entrenamiento y prueba
table(train_data$Outcome) 
table(test_data$Outcome)

# Grid search de hiperparámetros con métricas adicionales
mejores_resultados <- NULL
valores_cf <- c(0.05, 0.1, 0.15, 0.25)
valores_minCases <- c(5, 10, 15)
valores_trials <- c(5, 10, 20)

for (cf in valores_cf) {
  for (min_cases in valores_minCases) {
    for (trials in valores_trials) {
      # Entrenar el modelo con la combinación actual
      modelo <- C5.0(Outcome ~ ., data = train_data, 
                     control = C5.0Control(CF = cf, minCases = min_cases), 
                     trials = trials)
      
      # Evaluar en el conjunto de prueba
      predicciones <- predict(modelo, test_data)
      matriz_confusion <- confusionMatrix(predicciones, test_data$Outcome)
      
      # Calcular métricas adicionales
      accuracy <- matriz_confusion$overall['Accuracy']
      sensitivity <- matriz_confusion$byClass['Sensitivity']
      specificity <- matriz_confusion$byClass['Specificity']
      precision <- matriz_confusion$byClass['Pos Pred Value']
      f1_score <- (2 * precision * sensitivity) / (precision + sensitivity)
      
      # Guardar los resultados
      mejores_resultados <- rbind(
        mejores_resultados,
        data.frame(
          CF = cf, 
          minCases = min_cases, 
          Trials = trials, 
          Accuracy = accuracy, 
          Sensitivity = sensitivity, 
          Specificity = specificity, 
          Precision = precision, 
          F1_Score = f1_score
        )
      )
    }
  }
}

# Ordenar los resultados por la métrica deseada (ej., F1_Score o Accuracy)
mejores_resultados <- mejores_resultados[order(-mejores_resultados$Accuracy, -mejores_resultados$F1_Score), ]

# Mostrar las mejores combinaciones
print(mejores_resultados)

# Ejemplo de ponderación de métricas
ponderacion <- c(Sensitivity = 0.4, F1_Score = 0.3, Precision = 0.15, Specificity = 0.1, Accuracy = 0.05)

# Añadir una columna con la métrica ponderada
mejores_resultados$Metrica_Ponderada <- with(mejores_resultados, 
                                             (Sensitivity * ponderacion["Sensitivity"]) + 
                                               (F1_Score * ponderacion["F1_Score"]) + 
                                               (Precision * ponderacion["Precision"]) + 
                                               (Specificity * ponderacion["Specificity"]) + 
                                               (Accuracy * ponderacion["Accuracy"]))

# Ordenar los resultados por la métrica ponderada
mejores_resultados <- mejores_resultados[order(-mejores_resultados$Metrica_Ponderada), ]
print(mejores_resultados)


modelo_c50 <- C5.0(Outcome ~ ., data = train_data, 
               control = C5.0Control(CF = 0.05, minCases = 5), 
               trials = 5)

# Gráfico del mejor modelo
plot(modelo_c50, trial = 4)

# Resumen del mejor modelo
summary(modelo_c50)

# Predicciones con datos de pruebas
predicciones <- predict(modelo_c50, test_data)
matriz_confusion <- confusionMatrix(predicciones, test_data$Outcome)

# Matriz de confusión
print(matriz_confusion)

# Calcular la probabilidad de predicción
probabilidades <- predict(modelo_c50, test_data, type = "prob")

# Crear la curva ROC
roc_curve <- roc(test_data$Outcome, probabilidades[, 2], levels = rev(levels(test_data$Outcome)))
plot(roc_curve, col = "blue", lwd = 2, main = "Curva ROC del Modelo C5.0")
auc_value <- auc(roc_curve)
print(paste("Área Bajo la Curva (AUC):", round(auc_value, 4)))

# Crear gráfico de importancia de las variables
importancia <- C5imp(modelo_c50)
importance_df <- data.frame(Variable = rownames(importancia), Importancia = importancia[, 1])

ggplot(importance_df, aes(x = reorder(Variable, Importancia), y = Importancia)) +
  geom_bar(stat = "identity", fill = "darkblue", color = "black") +
  coord_flip() +
  labs(title = "Importancia de las variables", x = "Variable", y = "Importancia")

