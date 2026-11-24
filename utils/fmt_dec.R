# Decimales con coma
fmt_dec <- function(x, decimales = 1) {
  format(round(x, decimales), decimal.mark = ",", nsmall = decimales, trim = TRUE)
}
