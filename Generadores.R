#' Generador de congruencia lineal
#'
#' Genera números aleatorios usando el método de congruencia lineal.
#' @param n Cantidad de números aleatorios.
#' @param semilla Valor inicial.
#' @param a Constante multiplicativa.
#' @param c Constante aditiva.
#' @param m Módulo.
#' @param min Valor mínimo.
#' @param max Valor máximo.
#' @return Vector de números aleatorios entre min y max.
#' @export
generador_congruencial <- function(n, semilla, a, c, m, min = 0, max = 20) {
  x <- numeric(n)
  x[1] <- semilla
  for (i in 2:n) {
    x[i] <- (a * x[i - 1] + c) %% m
  }
  min + (x / m) * (max - min)
}

#' Generador de notas
#'
#' Genera notas aleatorias: enteras o decimales.
#' @param n Cantidad de notas.
#' @param tipo "decimal" o "entero".
#' @return Vector de notas aleatorias.
#' @export
generador_notas <- function(n, tipo = "decimal") {
  if (tipo == "decimal") {
    round(runif(n, 0, 20), 2)
  } else if (tipo == "entero") {
    sample(0:20, n, replace = TRUE)
  } else {
    stop("El tipo debe ser 'decimal' o 'entero'")
  }
}
