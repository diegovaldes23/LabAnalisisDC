# Librerías necesarias
library(signal)  # Para procesamiento de señales
library(TSA)     # Para análisis de series temporales
library(oce)     # Para transformadas espectrales y gráficos avanzados

# Configurar ruta para archivos
current_dir <- getwd()
file_normo <- "DY000.txt"  # Archivo para el sujeto normocápnico
file_hiper <- "DY001.txt"  # Archivo para el sujeto hipercápnico

# Cargar los datos
file_path_normo <- list.files(path = current_dir, pattern = file_normo, recursive = TRUE, full.names = TRUE)
file_path_hiper <- list.files(path = current_dir, pattern = file_hiper, recursive = TRUE, full.names = TRUE)

if (length(file_path_normo) > 0 && length(file_path_hiper) > 0) {
  sujeto_normo <- read.table(file_path_normo[1], header = FALSE, col.names = c("PAM", "CO2", "VFSC"))
  sujeto_hiper <- read.table(file_path_hiper[1], header = FALSE, col.names = c("PAM", "CO2", "VFSC"))
  print("Archivos cargados correctamente.")
} else {
  stop("Uno o ambos archivos no fueron encontrados.")
}

# 1. Analizar la relación entre la PAM y la VFSC mediante correlación cruzada
# Correlación cruzada para normocápnico
ccf(sujeto_normo$PAM, sujeto_normo$VFSC, main = "Correlación Cruzada (Normocápnico)", ylab = "Correlación", xlab = "Lag")
cc_normo <- ccf(sujeto_normo$PAM, sujeto_normo$VFSC, plot = FALSE)
lag_max_normo <- cc_normo$lag[which.max(cc_normo$acf)]
cat("Lag máximo para normocápnico:", lag_max_normo, "\n")

# Correlación cruzada para hipercápnico
ccf(sujeto_hiper$PAM, sujeto_hiper$VFSC, main = "Correlación Cruzada (Hipercápnico)", ylab = "Correlación", xlab = "Lag")
cc_hiper <- ccf(sujeto_hiper$PAM, sujeto_hiper$VFSC, plot = FALSE)
lag_max_hiper <- cc_hiper$lag[which.max(cc_hiper$acf)]
cat("Lag máximo para hipercápnico:", lag_max_hiper, "\n")

# 2. Calcular la autocorrelación de la señal PAM
# Autocorrelación de PAM para normocápnico
acf(sujeto_normo$PAM, main = "Autocorrelación de PAM (Normocápnico)", ylab = "Autocorrelación", xlab = "Lag")

# Autocorrelación de PAM para hipercápnico
acf(sujeto_hiper$PAM, main = "Autocorrelación de PAM (Hipercápnico)", ylab = "Autocorrelación", xlab = "Lag")

# 3. Modelar la relación entre la PAM y la VFSC utilizando una función de transferencia
# Transformada de Fourier para normocápnico
spec.pgram(cbind(sujeto_normo$PAM, sujeto_normo$VFSC), taper = 0.1, plot = TRUE, main = "Transformada de Fourier (Normocápnico)")

# Transformada de Fourier para hipercápnico
spec.pgram(cbind(sujeto_hiper$PAM, sujeto_hiper$VFSC), taper = 0.1, plot = TRUE, main = "Transformada de Fourier (Hipercápnico)")

# 4. Simular la aplicación de un escalón inverso de PAM
# Escalón inverso en PAM para normocápnico
escalon_normo <- rep(0, length(sujeto_normo$PAM))
escalon_normo[1:10] <- -20
vfsc_simulado_normo <- sujeto_normo$VFSC + escalon_normo

# Graficar respuesta
plot(vfsc_simulado_normo, type = "l", col = "blue", main = "Respuesta a Escalón Inverso (Normocápnico)", ylab = "VFSC", xlab = "Tiempo")
lines(sujeto_normo$VFSC, col = "red")
legend("topright", legend = c("Simulado", "Original"), col = c("blue", "red"), lty = 1)

# Escalón inverso en PAM para hipercápnico
escalon_hiper <- rep(0, length(sujeto_hiper$PAM))
escalon_hiper[1:10] <- -20
vfsc_simulado_hiper <- sujeto_hiper$VFSC + escalon_hiper

# Graficar respuesta
plot(vfsc_simulado_hiper, type = "l", col = "blue", main = "Respuesta a Escalón Inverso (Hipercápnico)", ylab = "VFSC", xlab = "Tiempo")
lines(sujeto_hiper$VFSC, col = "red")
legend("topright", legend = c("Simulado", "Original"), col = c("blue", "red"), lty = 1)

