# Los CSV no guardan factores: se vuelven a declarar con su categoría de referencia
library(dplyr)

declarar_tipos <- function(d) {
  d |> mutate(
    educacion_4cat = factor(educacion_4cat, levels = c(
      "1. Hasta Sec. Incompleto", "2. Secundario Completo",
      "3. Terciario/Univ. Incompleto", "4. Terciario/Univ. Completo")),
    servicio_pago  = factor(servicio_pago, levels = c("Sin servicio pago", "Con servicio pago")),
    estado_laboral = factor(estado_laboral, levels = c("No ocupada", "Ocupada")),
    dia_tipo       = factor(dia_tipo, levels = c("Día Laborable", "Día No Laborable"))
  )
}
