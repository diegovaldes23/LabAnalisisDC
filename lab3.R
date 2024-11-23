# Carga de librerías
library(arules)
library(arulesViz)

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

# Discretización de las variables (CON FUENTES)
diabetes_data$Glucose <- cut(diabetes_data$Glucose, breaks = c(-Inf, 140, 200, Inf), 
                             labels = c("Normal", "Alto", "Muy alto"))
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

# Verificar estructura después del preprocesamiento
str(diabetes_data)

# TRANSFORMACIÓN EN FORMATO TRANSACCIONAL
transacciones <- as(diabetes_data, "transactions")

# Resumen de las transacciones
summary(transacciones)

# APLICACIÓN DEL ALGORITMO APRIORI
# Aplicar el algoritmo
reglas <- apriori(transacciones, parameter = list(support = 0.1, confidence = 0.7, minlen = 2))

summary(reglas)

# Filtrar por reglas lift
reglas_significativas <- subset(reglas, lift > 1.2)

# Mostrar las 10 reglas más importantes
inspect(head(sort(reglas_significativas, by = "lift"), 50))

plot(reglas_significativas, method ="graph")

reglas_filtradas <- head(sort(reglas_significativas, by = "lift"), 50)

# Visualización en forma de red
plot(reglas_filtradas, method = "graph")
