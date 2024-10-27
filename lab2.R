
# Cargar los paquetes
library(clustMixType)
library(cluster)
library(factoextra)
# Parte 1

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


# Parte 2: Limpieza de datos
# Al analizar el resumen estadístico se destaca que los valores de 0 en las variables como Glucose,
# BloodPressure, SkinThickness, Insulin, y BMI probablemente representan datos faltantes. 
# Esto es importante para limpiar los datos antes de realizar cualquier análisis inferencial, ya que, se puede malinterpretar 
# el análisis si se colocan estos datos.
# Por lo tanto, reemplazar valores de 0 por NA en las variables específicas.

# Reemplazar los ceros por NA en las columnas donde no tiene sentido que haya ceros
diabetes_data$Glucose[diabetes_data$Glucose == 0] <- NA
diabetes_data$BloodPressure[diabetes_data$BloodPressure == 0] <- NA
diabetes_data$SkinThickness[diabetes_data$SkinThickness == 0] <- NA
diabetes_data$Insulin[diabetes_data$Insulin == 0] <- NA
diabetes_data$BMI[diabetes_data$BMI == 0] <- NA
diabetes_data$DiabetesPedigreeFunction[diabetes_data$DiabetesPedigreeFunction == 0] <- NA

diabetes_data$Glucose[is.na(diabetes_data$Glucose)] <- mean(diabetes_data$Glucose, na.rm = TRUE)
diabetes_data$BloodPressure[is.na(diabetes_data$BloodPressure)] <- mean(diabetes_data$BloodPressure, na.rm = TRUE)
diabetes_data$SkinThickness[is.na(diabetes_data$SkinThickness)] <- mean(diabetes_data$SkinThickness, na.rm = TRUE)
diabetes_data$Insulin[is.na(diabetes_data$Insulin)] <- mean(diabetes_data$Insulin, na.rm = TRUE)
diabetes_data$BMI[is.na(diabetes_data$BMI)] <- mean(diabetes_data$BMI, na.rm = TRUE)
diabetes_data$DiabetesPedigreeFunction[is.na(diabetes_data$DiabetesPedigreeFunction)] <- mean(diabetes_data$DiabetesPedigreeFunction, na.rm = TRUE)

head(diabetes_data)

# Estructura de los datos
str(diabetes_data)

# Resumen estadístico básico de todas las variables
summary(diabetes_data)