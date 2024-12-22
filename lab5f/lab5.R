# Librerías necesarias
library(signal)  # Para procesamiento de señales
library(TSA)     # Para análisis de series temporales
library(oce)     # Para transformadas espectrales y gráficos avanzados

# Configurar ruta para archivos
current_dir <- getwd()
file_normo <- "DY000.txt"
file_hiper <- "DY001.txt"

# Cargar archivos si existen
file_path_normo <- list.files(path = current_dir, pattern = file_normo, recursive = TRUE, full.names = TRUE)
file_path_hiper <- list.files(path = current_dir, pattern = file_hiper, recursive = TRUE, full.names = TRUE)

if (length(file_path_normo) > 0 && length(file_path_hiper) > 0) {
  sujeto_normo <- read.table(file_path_normo[1], header = FALSE, col.names = c("PAM", "CO2", "VFSC"))
  sujeto_hiper <- read.table(file_path_hiper[1], header = FALSE, col.names = c("PAM", "CO2", "VFSC"))
  print("Archivos cargados correctamente.")
} else {
  stop("Uno o ambos archivos no fueron encontrados.")
}

# Graficar las señales
plot(sujeto_normo$PAM, type = "l", col = "blue", main = "Señal PAM - Sujeto Normocápnico")
lines(sujeto_normo$VFSC, col = "red")
legend("topright", legend = c("PAM", "VFSC"), col = c("blue", "red"), lty = 1)

# Correlación cruzada entre PAM y VFSC
ccf(sujeto_normo$PAM, sujeto_normo$VFSC, main = "Correlación Cruzada (Normocápnico)")

# Autocorrelación de PAM
acf(sujeto_normo$PAM, main = "Autocorrelación de PAM (Normocápnico)")

# Transformada de Fourier (frecuencia)
spec.pgram(cbind(sujeto_normo$PAM, sujeto_normo$VFSC), taper = 0.1, plot = TRUE)

# Simular un escalón inverso en PAM
escalon <- rep(0, length(sujeto_normo$PAM))  # Crear un vector de ceros
escalon[1:10] <- -20  # Aplicar el escalón en los primeros 10 puntos

# Simular la respuesta de VFSC al escalón inverso
vfsc_simulado <- sujeto_normo$VFSC + escalon  # Suma del escalón a VFSC

# Graficar la respuesta simulada
plot(vfsc_simulado, type = "l", col = "blue", main = "Respuesta a Escalón Inverso")
lines(sujeto_normo$VFSC, col = "red")  # Comparar con la señal original
legend("topright", legend = c("Simulado", "Original"), col = c("blue", "red"), lty = 1)

# Comparación entre normocápnico e hipercápnico
par(mfrow = c(2, 1))
plot(sujeto_normo$PAM, type = "l", col = "blue", main = "Sujeto Normocápnico")
lines(sujeto_normo$VFSC, col = "red")
plot(sujeto_hiper$PAM, type = "l", col = "green", main = "Sujeto Hipercápnico")
lines(sujeto_hiper$VFSC, col = "orange")
legend("topright", legend = c("PAM", "VFSC"), col = c("blue", "red", "green", "orange"), lty = 1)

# Conclusión
print("Comparación y análisis finalizado.")
