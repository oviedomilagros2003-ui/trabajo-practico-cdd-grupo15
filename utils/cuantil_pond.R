# Cuantil ponderado: x = valores, w = ponderador, p = probabilidad (0 a 1)
cuantil_pond <- function(x, w, p) {
  o <- order(x)
  x <- x[o]
  w <- w[o]
  x[which(cumsum(w) / sum(w) >= p)[1]]
}
