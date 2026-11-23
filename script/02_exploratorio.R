# ==============================================================================
# Script 02: Análisis Exploratorio de Datos (EDA)
# Trabajo Práctico — Grupo 15
# ==============================================================================

options(scipen = 999)
library(tidyverse)
library(skimr)

# Directorios de entrada y salida
input_dir    <- "input"
out_dir      <- "output"
out_tablas   <- file.path(out_dir, "tablas")
out_graficos <- file.path(out_dir, "graficos")

dir.create(out_tablas, recursive = TRUE, showWarnings = FALSE)
dir.create(out_graficos, recursive = TRUE, showWarnings = FALSE)

# Carga de la base limpia
ruta_base_limpia <- file.path(input_dir, "enut_limpia.csv")

if (!file.exists(ruta_base_limpia)) {
  stop("No se encuentra '", ruta_base_limpia, "'. Ejecute primero el script 01_limpieza_eda.R.")
}

enut_clean <- read_csv(ruta_base_limpia, show_col_types = FALSE)

# ==============================================================================
# 1. ESTADÍSTICAS DESCRIPTIVAS MUESTRALES Y TABLAS CRUZADAS
# ==============================================================================

# 1.1. Resumen de variables numéricas (N = 6.046)
tabla_skim <- enut_clean |> 
  select(hs_domestico, hs_cuidado, edad_sel, cant_personashasta13) |> 
  skim() |> 
  as_tibble() 

write_csv(tabla_skim, file.path(out_tablas, "descriptivas_skim.csv"))
tabla_skim

# 1.2. Porcentaje de ceros y revisión de jornadas excesivas
ceros_resumen <- enut_clean |> 
  summarise( 
    pct_cero_domestico = round(100 * mean(hs_domestico == 0), 1), 
    pct_cero_cuidado   = round(100 * mean(hs_cuidado == 0), 1) 
  )
ceros_resumen

jornadas_excesivas <- enut_clean |> 
  summarise( 
    cuidado_mas_24   = sum(hs_cuidado > 24), 
    domestico_mas_24 = sum(hs_domestico > 24), 
    suma_mas_24      = sum(hs_domestico + hs_cuidado > 24) 
  )
jornadas_excesivas

# 1.3. Tabla cruzada: Nivel educativo vs. Servicio doméstico pago
tabla_cruzada_conteo <- enut_clean |> 
  filter(!is.na(servicio_pago)) |> 
  count(educacion_4cat, servicio_pago) |> 
  pivot_wider(names_from = servicio_pago, values_from = n, values_fill = 0) 

write_csv(tabla_cruzada_conteo, file.path(out_tablas, "tabla_cruzada_tercerizacion_conteo.csv"))
tabla_cruzada_conteo

tabla_cruzada_pct <- enut_clean |> 
  filter(!is.na(servicio_pago)) |> 
  group_by(educacion_4cat, servicio_pago) |> 
  summarise(n = n(), .groups = "drop_last") |> 
  mutate(pct = round(100 * n / sum(n), 1)) |> 
  select(-n) |> 
  pivot_wider(names_from = servicio_pago, values_from = pct, values_fill = 0) 

write_csv(tabla_cruzada_pct, file.path(out_tablas, "tabla_cruzada_tercerizacion_pct.csv"))
tabla_cruzada_pct

# ==============================================================================
# 2. TABLAS PONDERADAS Y CRUCES DE VARIABLES
# ==============================================================================

# 2.1. Trabajo del hogar y tercerización por nivel educativo
tabla_1_educacion <- enut_clean |> 
  group_by(educacion_4cat) |> 
  summarise( 
    casos_muestra       = n(), 
    poblacion_expandida = sum(wper), 
    edad_prom           = round(weighted.mean(edad_sel, w = wper), 1), 
    hs_domestico_prom   = round(weighted.mean(hs_domestico, w = wper), 2), 
    hs_cuidado_prom     = round(weighted.mean(hs_cuidado, w = wper), 2), 
    pct_servicio_pago   = round(100 * weighted.mean(servicio_pago == "Con servicio pago", 
                                                    w = wper, na.rm = TRUE), 1), 
    .groups = "drop" 
  )

write_csv(tabla_1_educacion, file.path(out_tablas, "tabla_1_educacion_tercerizacion.csv"))
tabla_1_educacion

variacion_tercerizacion <- tabla_1_educacion |> 
  select(educacion_4cat, pct_servicio_pago) |> 
  mutate( 
    dif_puntos = round(pct_servicio_pago - lag(pct_servicio_pago), 1), 
    veces      = round(pct_servicio_pago / lag(pct_servicio_pago), 1) 
  )
variacion_tercerizacion

# 2.2. Control por tipo de día (Laborable vs. No laborable)
tabla_2_dias <- enut_clean |> 
  group_by(educacion_4cat, dia_tipo) |> 
  summarise( 
    casos             = n(), 
    hs_domestico_prom = round(weighted.mean(hs_domestico, w = wper), 2), 
    hs_cuidado_prom   = round(weighted.mean(hs_cuidado, w = wper), 2), 
    .groups           = "drop" 
  )

write_csv(tabla_2_dias, file.path(out_tablas, "tabla_2_control_dias.csv"))
tabla_2_dias

mezcla_dias <- enut_clean |> 
  group_by(educacion_4cat) |> 
  summarise( 
    pct_dia_no_laborable = round(100 * mean(dia_laborable == 2), 1), 
    .groups              = "drop" 
  )
mezcla_dias

razon_dias <- tabla_2_dias |> 
  group_by(educacion_4cat) |> 
  summarise( 
    razon_domestico = round(hs_domestico_prom[dia_tipo == "Día No Laborable"] / 
                              hs_domestico_prom[dia_tipo == "Día Laborable"], 2), 
    razon_cuidado   = round(hs_cuidado_prom[dia_tipo == "Día No Laborable"] / 
                              hs_cuidado_prom[dia_tipo == "Día Laborable"], 2), 
    .groups         = "drop" 
  )
razon_dias

# 2.3. Mujeres ocupadas según presencia de niños
tabla_3_ocupadas <- enut_clean |> 
  filter(estado_laboral == "Ocupada") |> 
  group_by(educacion_4cat, presencia_ninos) |> 
  summarise( 
    casos             = n(), 
    hs_domestico_prom = round(weighted.mean(hs_domestico, w = wper), 2), 
    hs_cuidado_prom   = round(weighted.mean(hs_cuidado, w = wper), 2), 
    hs_cuidado_simple = round(mean(hs_cuidado), 2), 
    pct_servicio_pago = round(100 * weighted.mean(servicio_pago == "Con servicio pago", 
                                                    w = wper, na.rm = TRUE), 1), 
    .groups           = "drop" 
  )

write_csv(tabla_3_ocupadas, file.path(out_tablas, "tabla_3_mujeres_ocupadas_ninos.csv"))
tabla_3_ocupadas

# 2.4. Cruces adicionales según otras variables sociodemográficas
tabla_cruces <- enut_clean |> 
  select(hs_domestico, hs_cuidado, wper, 
         servicio_pago, estado_laboral, ninos_cat, dia_tipo) |> 
  pivot_longer( 
    cols      = c(servicio_pago, estado_laboral, ninos_cat, dia_tipo), 
    names_to  = "variable", 
    values_to = "grupo" 
  ) |> 
  filter(!is.na(grupo)) |> 
  group_by(variable, grupo) |> 
  summarise( 
    casos             = n(), 
    hs_domestico_prom = round(weighted.mean(hs_domestico, w = wper), 2), 
    hs_cuidado_prom   = round(weighted.mean(hs_cuidado, w = wper), 2), 
    .groups           = "drop" 
  ) |> 
  group_by(variable) |> 
  mutate(pct_muestra = round(100 * casos / sum(casos), 1)) |> 
  ungroup()

write_csv(tabla_cruces, file.path(out_tablas, "tabla_cruces_adicionales.csv"))
tabla_cruces

# 2.5. Perfil sociodemográfico por nivel educativo
perfil_educ <- enut_clean |> 
  group_by(educacion_4cat) |> 
  summarise( 
    casos             = n(), 
    edad_media        = round(weighted.mean(edad_sel, w = wper), 1), 
    pct_ocupadas      = round(100 * weighted.mean(estado_laboral == "Ocupada", w = wper), 1), 
    pct_con_ninos     = round(100 * weighted.mean(presencia_ninos == "Con niños (0-13)", w = wper), 1), 
    pct_servicio_pago = round(100 * weighted.mean(servicio_pago == "Con servicio pago", 
                                                  w = wper, na.rm = TRUE), 1), 
    .groups           = "drop" 
  )

write_csv(perfil_educ, file.path(out_tablas, "perfil_por_nivel_educativo.csv"))
perfil_educ

# ==============================================================================
# 3. VISUALIZACIÓN Y ANÁLISIS DE VALORES ATÍPICOS
# ==============================================================================

# 3.1. Configuración de estética y etiquetas
col_hist       <- "#B0245A"
cols_educ      <- c("#7B1650", "#C0566F", "#E8A1C4", "#D81B60")
cols_dias      <- c("#9B1048", "#E0245E")
etiquetas_educ <- c("Hasta sec. incompleto", "Secundario completo", 
                    "Terciario/univ. incompleto", "Terciario/univ. completo")

guardar_y_mostrar <- function(grafico, archivo, ancho = 800, alto = 600) {
  png(file.path(out_graficos, archivo), width = ancho, height = alto, res = 120)
  grafico()
  invisible(dev.off())
  grafico()
}

# 3.2. Histograma del trabajo doméstico diario
grafico_hist_domestico <- function() {
  hist(enut_clean$hs_domestico,
       breaks = 20, col = col_hist, border = "white",
       main = "Distribución del Trabajo Doméstico Diario",
       xlab = "Horas diarias", ylab = "Frecuencia")
}
guardar_y_mostrar(grafico_hist_domestico, "histograma_trabajo_domestico.png")

# 3.3. Sensibilidad del histograma según intervalos
grafico_sensibilidad <- function() {
  op <- par(mfrow = c(2, 2))
  on.exit(par(op))
  for (b in c(5, 10, 20, 40)) {
    hist(enut_clean$hs_domestico,
         breaks = b, col = "grey80", border = "white",
         main = paste("Sensibilidad: breaks =", b),
         xlab = "Horas diarias", ylab = "Frecuencia")
  }
}
guardar_y_mostrar(grafico_sensibilidad, "histograma_sensibilidad.png")

# 3.4. Boxplot del trabajo doméstico según nivel educativo
grafico_boxplot_educ <- function() {
  op <- par(mar = c(14, 5, 4, 2))
  on.exit(par(op))
  boxplot(hs_domestico ~ educacion_4cat,
          data = enut_clean, col = cols_educ, las = 2,
          main = "Trabajo Doméstico Diario según Nivel Educativo",
          xlab = "", ylab = "Horas diarias")
}
guardar_y_mostrar(grafico_boxplot_educ, "boxplot_domestico_educacion.png")

# 3.5. Análisis de valores atípicos
bp_stats <- boxplot.stats(enut_clean$hs_domestico)
umbral_outlier <- bp_stats$stats[5]

atipicos <- enut_clean |> filter(hs_domestico > umbral_outlier)

perfil_atipicos <- atipicos |> 
  select(educacion_4cat, estado_laboral, presencia_ninos, dia_tipo) |> 
  pivot_longer(everything(), names_to = "variable", values_to = "categoria") |> 
  count(variable, categoria) |> 
  mutate(pct = round(100 * n / nrow(atipicos), 1))

print(perfil_atipicos, n = Inf)

# 3.6. Cuidado directo de ocupadas con niños por nivel educativo
cuidado_con_ninos <- tabla_3_ocupadas |> 
  filter(presencia_ninos == "Con niños (0-13)")

grafico_barras_cuidado <- function() {
  op <- par(mar = c(10, 5, 4, 2))
  on.exit(par(op))
  barplot(cuidado_con_ninos$hs_cuidado_prom,
          names.arg = etiquetas_educ, las = 2,
          col = cols_dias[2], border = "white", ylim = c(0, 6),
          main = "Cuidado directo de ocupadas con niños (0-13)",
          ylab = "Horas diarias (promedio ponderado)")
}
guardar_y_mostrar(grafico_barras_cuidado, "barras_cuidado_ocupadas_ninos.png")

# 3.7. Trabajo doméstico en día laborable vs. no laborable
dias_ancho <- tabla_2_dias |> 
  select(educacion_4cat, dia_tipo, hs_domestico_prom) |> 
  pivot_wider(names_from = dia_tipo, values_from = hs_domestico_prom)

grafico_barras_dias <- function() {
  op <- par(mar = c(5, 11, 4, 2))
  on.exit(par(op))
  mat <- t(as.matrix(dias_ancho[, c("Día Laborable", "Día No Laborable")]))
  colnames(mat) <- etiquetas_educ
  barplot(mat[, 4:1], beside = TRUE, horiz = TRUE, las = 1,
          col = cols_dias, border = "white", xlim = c(0, 6),
          main = "Trabajo doméstico según tipo de día",
          xlab = "Horas diarias (promedio ponderado)",
          legend.text = c("Laborable", "No laborable"),
          args.legend = list(x = "bottomright", bty = "n"))
}
guardar_y_mostrar(grafico_barras_dias, "barras_domestico_tipo_dia.png")

# ==============================================================================
# 4. PERCENTILES, RATIOS DE DESIGUALDAD Y CORRELACIONES
# ==============================================================================

# 4.1. Percentiles y ratios de desigualdad
percentiles_dom <- quantile(enut_clean$hs_domestico, probs = c(0.10, 0.25, 0.50, 0.75, 0.90))

ratios_tabla <- tibble(
  p10 = percentiles_dom["10%"],
  p25 = percentiles_dom["25%"],
  p50 = percentiles_dom["50%"],
  p75 = percentiles_dom["75%"],
  p90 = percentiles_dom["90%"],
  ratio_90_10 = round(p90 / p10, 2),
  ratio_75_25 = round(p75 / p25, 2),
  ratio_50_10 = round(p50 / p10, 2),
  ratio_90_50 = round(p90 / p50, 2)
)

write_csv(ratios_tabla, file.path(out_tablas, "ratios_percentiles.csv"))
ratios_tabla

# 4.2. Matriz de correlaciones (Pearson vs. Spearman)
vars_num <- enut_clean |> 
  select(hs_domestico, hs_cuidado, edad_sel, cant_personashasta13)

mat_cor      <- cor(vars_num, use = "pairwise.complete.obs")
mat_spearman <- cor(vars_num, use = "pairwise.complete.obs", method = "spearman")

# 4.3. Ranking de correlaciones de Pearson
cor_ranking <- as.data.frame(as.table(mat_cor)) |> 
  as_tibble() |> 
  rename(var1 = Var1, var2 = Var2, r = Freq) |> 
  filter(var1 != var2) |> 
  mutate(par = map2_chr(as.character(var1), as.character(var2),
                        ~ paste(sort(c(.x, .y)), collapse = " - "))) |> 
  distinct(par, .keep_all = TRUE) |> 
  arrange(desc(abs(r))) |> 
  mutate(r = round(r, 2)) |> 
  select(par, r)

write_csv(cor_ranking, file.path(out_tablas, "correlaciones_ranking.csv"))
cor_ranking

# 4.4. Matriz de dispersión
grafico_pairs <- function() {
  pairs(vars_num, pch = 19, cex = 0.5,
        col = rgb(0.69, 0.14, 0.35, 0.3),
        main = "Matriz de Dispersión entre Variables Cuantitativas")
}
guardar_y_mostrar(grafico_pairs, "matriz_dispersion_pairs.png", ancho = 800, alto = 800)

# ==============================================================================
# 5. CONTROLES DE CONSISTENCIA Y ROBUSTEZ
# ==============================================================================

cuantil_pond <- function(x, w, p) {
  o <- order(x)
  x <- x[o]
  w <- w[o]
  x[which(cumsum(w) / sum(w) >= p)[1]]
}

# 5.1. Comparación de promedios: Ponderado vs. Simple
comparacion_pond_simple <- enut_clean |> 
  group_by(educacion_4cat) |> 
  summarise( 
    edad_pond        = round(weighted.mean(edad_sel, w = wper), 1), 
    edad_simple      = round(mean(edad_sel), 1), 
    domestico_pond   = round(weighted.mean(hs_domestico, w = wper), 2), 
    domestico_simple = round(mean(hs_domestico), 2), 
    cuidado_pond     = round(weighted.mean(hs_cuidado, w = wper), 2), 
    cuidado_simple   = round(mean(hs_cuidado), 2), 
    servicio_pond    = round(100 * weighted.mean(servicio_pago == "Con servicio pago", 
                                                 w = wper, na.rm = TRUE), 1), 
    servicio_simple  = round(100 * mean(servicio_pago == "Con servicio pago", 
                                        na.rm = TRUE), 1), 
    .groups          = "drop" 
  )

write_csv(comparacion_pond_simple, file.path(out_tablas, "comparacion_ponderado_vs_simple.csv"))
comparacion_pond_simple

# 5.2. Medianas simples y ponderadas
medianas_educ <- enut_clean |> 
  group_by(educacion_4cat) |> 
  summarise( 
    mediana_dom_simple = round(median(hs_domestico), 2), 
    mediana_dom_pond   = round(cuantil_pond(hs_domestico, wper, 0.5), 2), 
    mediana_cui_simple = round(median(hs_cuidado), 2), 
    mediana_cui_pond   = round(cuantil_pond(hs_cuidado, wper, 0.5), 2), 
    .groups            = "drop" 
  )

write_csv(medianas_educ, file.path(out_tablas, "medianas_educacion.csv"))
medianas_educ

# 5.3. Controles por condición laboral y presencia de niños
dom_educ_estado <- enut_clean |> 
  group_by(educacion_4cat, estado_laboral) |> 
  summarise( 
    casos     = n(), 
    domestico = round(weighted.mean(hs_domestico, w = wper), 2), 
    cuidado   = round(weighted.mean(hs_cuidado,   w = wper), 2), 
    .groups   = "drop" 
  )

dom_educ_ninos <- enut_clean |> 
  group_by(educacion_4cat, presencia_ninos) |> 
  summarise( 
    casos     = n(), 
    domestico = round(weighted.mean(hs_domestico, w = wper), 2), 
    cuidado   = round(weighted.mean(hs_cuidado,   w = wper), 2), 
    .groups   = "drop" 
  )

# 5.4. Percentiles muestrales vs. ponderados
prob <- c(0.10, 0.50, 0.90)
percentiles_comparacion <- tibble(
  percentil    = prob,
  horas_simple = as.numeric(quantile(enut_clean$hs_domestico, prob)),
  horas_pond   = sapply(prob, function(p) cuantil_pond(enut_clean$hs_domestico, enut_clean$wper, p))
)
percentiles_comparacion

pct_atipicos_pond <- round(100 * sum(enut_clean$wper[enut_clean$hs_domestico > umbral_outlier]) / 
                             sum(enut_clean$wper), 1)
pct_atipicos_pond

# 5.5. Dispersión y errores estándar en cuidado directo
cuidado_ocupadas_ninos <- enut_clean |> 
  filter(estado_laboral == "Ocupada", presencia_ninos == "Con niños (0-13)") |> 
  group_by(educacion_4cat) |> 
  summarise( 
    casos          = n(), 
    cuidado_pond   = round(weighted.mean(hs_cuidado, w = wper), 2), 
    cuidado_simple = round(mean(hs_cuidado), 2), 
    desvio         = round(sd(hs_cuidado), 2), 
    error_estandar = round(sd(hs_cuidado) / sqrt(casos), 2), 
    .groups        = "drop" 
  )

write_csv(cuidado_ocupadas_ninos, file.path(out_tablas, "cuidado_ocupadas_ninos_robustez.csv"))
cuidado_ocupadas_ninos

# ==============================================================================
# 6. EXPORTACIÓN DE RESULTADOS Y ARCHIVADO
# ==============================================================================

# 6.1. Copia de respaldo de la muestra limpia activa (N = 6.046)
write_csv(enut_clean, file.path(out_tablas, "enut_muestra_6046_clean.csv"))

# 6.2. Inventario de archivos generados en la carpeta output
archivos_generados <- list.files("output", recursive = TRUE)
print(archivos_generados)

# 6.3. Compresión del directorio de salidas para entrega
if (dir.exists("output")) {
  zip("output_instancia1.zip", files = "output", flags = "-r9Xq")
  message("Estado de compresión output_instancia1.zip: ", file.exists("output_instancia1.zip"))
