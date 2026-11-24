# Guarda un gráfico ggplot2 en output/graficos con ggsave() y lo muestra en pantalla
library(ggplot2)

guardar_grafico <- function(grafico, nombre_archivo, ancho = 20, alto = 14, dpi = 300) {
  carpeta <- file.path("output", "graficos")
  dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)
  ruta <- file.path(carpeta, nombre_archivo)
  ggsave(filename = ruta, plot = grafico, width = ancho, height = alto,
         units = "cm", dpi = dpi)
  print(grafico)
  message("Gráfico guardado en: ", ruta)
  invisible(ruta)
}
