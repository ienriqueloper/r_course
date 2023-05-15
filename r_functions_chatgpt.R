#Hacer funcion en R que tome como argumentos una media (x), una desviación
#estándar (y) y un número (z). Que la función genere números aleatorios con 
#la media y desviación estándar proporcionadas como argumentos. Si el número 
#aleatorio generado es mayor que el argumento "z" proporcionado, que el ciclo 
#while se termine. Que la función devuelva el número de iteraciones antes de 
#que se supere el número "z" y nos de también el número que provocó la interrupción

numeros_aleatorios <- function(x, y, z){
  flag <- TRUE
  contador <- 0
  
  while (flag) {
    contador <- contador + 1
    numero_aleatorio <- rnorm(1, mean = x, sd = y)
    
    if(numero_aleatorio > z){
      flag <- FALSE
    }
  }
  
  print(paste("Numero de iteraciones:", contador))
  print(paste("Numero que rompió la ejecución:", numero_aleatorio))
}



numeros_aleatorios(x = 100, y = 5, z = 120)



#CHATGPT CODE

mi_funcion <- function(x, y, z) {
  n_iter <- 0
  repeat {
    n_iter <- n_iter + 1
    num_aleatorio <- rnorm(1, x, y)
    if (num_aleatorio > z) {
      break
    }
  }
  list(iteraciones = n_iter, num_interrupcion = num_aleatorio)
}


mi_funcion(x = 100, y = 5, z = 120)


mi_funcion <- function(x, y, z) {
  n_iter <- 0
  num_aleatorio <- rnorm(1, x, y)
  while (num_aleatorio <= z) {
    n_iter <- n_iter + 1
    num_aleatorio <- rnorm(1, x, y)
  }
  list(iteraciones = n_iter, num_interrupcion = num_aleatorio)
}


mi_funcion(x = 100, y = 5, z = 120)

# Que la función tome como argumento un dataframe con una columna de fechas y 
# otra columna de valores numéricos, y devuelva un nuevo dataframe que contenga 
#las sumas acumulativas de los valores numéricos en la columna para cada fecha

sumas_acumulativas <- function(df) {
  df <- df[order(df$fecha), ]  # Ordenamos el dataframe por fecha
  
  df$suma_acumulativa <- cumsum(df$valor)  # Calculamos la suma acumulativa de los valores
  
  return(df)
}

# Creamos un dataframe de ejemplo con una columna de fechas y una columna de valores numéricos
fecha <- c("2022-01-01", "2022-01-02", "2022-01-03", "2022-01-04", "2022-01-05")
valor <- c(10, 5, 12, 8, 15)
df <- data.frame(fecha, valor)

# Llamamos a la función sumas_acumulativas para obtener las sumas acumulativas de los valores por fecha
sumas_acumulativas(df)


# Crear función que use un for loop para calcular la suma de los números pares entre 1 y un número n dado


suma_pares <- function(n) {
  suma <- 0
  
  for (i in 1:n) {
    if (i %% 2 == 0) {
      suma <- suma + i
    }
  }
  
  return(suma)
}


suma_pares(1000)














