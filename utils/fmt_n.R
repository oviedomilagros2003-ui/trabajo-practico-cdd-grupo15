# Enteros con punto de miles y coma decimal
fmt_n <- function(x) {
  format(x, big.mark = ".", decimal.mark = ",", trim = TRUE)
}
