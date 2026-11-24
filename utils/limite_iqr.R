# Límite superior de la regla del rango intercuartílico: Q3 + 1,5 * IQR
limite_iqr <- function(x) {
  q1 <- unname(quantile(x, 0.25, na.rm = TRUE))
  q3 <- unname(quantile(x, 0.75, na.rm = TRUE))
  q3 + 1.5 * (q3 - q1)
}
