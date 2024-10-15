# Crear un vector de números
numeros <- c(10, 20, 30, 40, 50, 60)

# Calcular el promedio de los números
promedio <- mean(numeros)

# Imprimir el promedio
print(paste("El promedio es:", promedio))

# Crear un gráfico de barras de los números
barplot(numeros, main="Gráfico de barras", xlab="Índice", ylab="Valor", col="blue")
