# Resumen del trabajo doméstico (horas por día) de un data frame
library(dplyr)

resumir_dom <- function(d) {
  d |> summarise(
    n       = n(),
    media   = round(mean(hs_domestico), 2),
    mediana = round(median(hs_domestico), 2),
    sd      = round(sd(hs_domestico), 2),
    p90     = round(unname(quantile(hs_domestico, 0.9)), 2),
    max     = round(max(hs_domestico), 2)
  )
}
