# Tendencia loess de hs_total según edad_sel, solo entre los percentiles 2,5 y 97,5 de edad
library(dplyr)
library(tibble)

suavizar <- function(d) {
  m <- loess(hs_total ~ edad_sel, data = d, span = 0.75, degree = 2)
  rango <- seq(ceiling(as.numeric(quantile(d$edad_sel, 0.025))),
               floor(as.numeric(quantile(d$edad_sel, 0.975))))
  p <- predict(m, newdata = data.frame(edad_sel = rango), se = TRUE)
  tibble(edad_sel = rango, fit = p$fit)
}
