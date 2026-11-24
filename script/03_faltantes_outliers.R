# ==============================================================================
# Script 03: Valores faltantes y valores atípicos (outliers)
# Trabajo Práctico - Grupo 15 (ENUT 2021)
# Entradas : input/enut_mujeres_18_64.csv, input/enut_limpia.csv,
#            output/tablas/codigos_no_respuesta.csv
# Salidas  : tablas en output/tablas/ y gráficos en output/graficos/
# ==============================================================================

options(scipen = 999)
library(tidyverse)

source(file.path("utils", "tema_scripts.R"))
source(file.path("utils", "guardar_grafico.R"))
source(file.path("utils", "declarar_tipos.R"))
source(file.path("utils", "limite_iqr.R"))
source(file.path("utils", "resumir_dom.R"))

input_dir  <- "input"
out_tablas <- file.path("output", "tablas")
dir.create(out_tablas, recursive = TRUE, showWarnings = FALSE)

ruta_mujeres <- file.path(input_dir, "enut_mujeres_18_64.csv")
ruta_limpia  <- file.path(input_dir, "enut_limpia.csv")
ruta_codigos <- file.path(out_tablas, "codigos_no_respuesta.csv")
for (r in c(ruta_mujeres, ruta_limpia, ruta_codigos)) {
  if (!file.exists(r)) stop("No se encuentra '", r, "'. Ejecute primero script/01_limpieza_eda.R.")
}

mujeres    <- read_csv(ruta_mujeres, show_col_types = FALSE)   # 6.077, antes de excluir
enut_clean <- read_csv(ruta_limpia,  show_col_types = FALSE)   # 6.046, muestra de análisis
codigos    <- read_csv(ruta_codigos, show_col_types = FALSE)

# ==============================================================================
# 1. VALORES FALTANTES
# En esta base no hay NA: los faltantes vienen como códigos (99 = Ns/Nc, 8 = educación especial)
# ==============================================================================

# 1.1. Cuántos casos hay por código (mujeres de 18 a 64 años, n = 6.077)
codigos <- codigos |>
  mutate(motivo = paste0(if_else(variable == "nivel_educativo_sel",
                                 "Nivel educativo: ", "Servicio pago: "), significado))

total_excluidos <- sum(codigos$mujeres_18_64)
cat("Casos con códigos sin respuesta:", total_excluidos,
    "(", round(100 * total_excluidos / nrow(mujeres), 2), "% de", nrow(mujeres), ")\n")

# 1.2. Perfil de las mujeres excluidas frente a la muestra de análisis
excl_educ <- mujeres |> filter(incluir_analisis == "NO")
excl_serv <- enut_clean |> filter(is.na(servicio_pago))

perfil_excluidos <- bind_rows(
  "Excluidas por nivel educativo (códigos 8 y 99)" = excl_educ,
  "Excluidas por servicio pago (código 99)"        = excl_serv,
  "Muestra de análisis"                            = enut_clean,
  .id = "grupo"
) |>
  group_by(grupo) |>
  summarise(
    n                = n(),
    edad_media       = round(mean(edad_sel), 1),
    pct_ocupadas     = round(100 * mean(estado_laboral == "Ocupada"), 1),
    pct_con_ninos    = round(100 * mean(presencia_ninos == "Con niños (0-13)"), 1),
    pct_no_laborable = round(100 * mean(dia_tipo == "Día No Laborable"), 1),
    .groups = "drop"
  )
write_csv(perfil_excluidos, file.path(out_tablas, "perfil_casos_excluidos.csv"))
perfil_excluidos

# 1.3. Impacto de excluir: estadísticas antes (6.077) y después (6.046)
impacto <- bind_rows("Antes de excluir" = mujeres, "Después de excluir" = enut_clean,
                     .id = "muestra") |>
  group_by(muestra) |>
  summarise(
    n               = n(),
    media_domestico = round(mean(hs_domestico), 2),
    mediana_dom     = round(median(hs_domestico), 2),
    media_cuidado   = round(mean(hs_cuidado), 2),
    pct_ocupadas    = round(100 * mean(estado_laboral == "Ocupada"), 1),
    edad_media      = round(mean(edad_sel), 1),
    .groups = "drop"
  ) |>
  arrange(desc(n))
write_csv(impacto, file.path(out_tablas, "impacto_exclusion_estadisticas.csv"))
impacto

# 1.4. Dónde faltan los datos de servicio pago (por nivel educativo)
faltante_servicio <- enut_clean |>
  group_by(educacion_4cat) |>
  summarise(n = n(), sin_dato_servicio = sum(is.na(servicio_pago)), .groups = "drop")
write_csv(faltante_servicio, file.path(out_tablas, "faltantes_servicio_por_educacion.csv"))

# 1.5. Gráfico de casos excluidos por motivo
g_excl <- codigos |>
  mutate(motivo = fct_reorder(motivo, mujeres_18_64)) |>
  ggplot(aes(x = motivo, y = mujeres_18_64)) +
  geom_col(fill = col_con, width = 0.65) +
  geom_text(aes(label = mujeres_18_64), hjust = -0.3, size = 3.8, color = col_texto) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Casos con códigos de no respuesta, por motivo",
       subtitle = paste0("Mujeres de 18 a 64 años (n = ",
                         format(nrow(mujeres), big.mark = ".", decimal.mark = ","),
                         "). Casos excluidos en cada análisis."),
       x = NULL, y = "Cantidad de mujeres", caption = fuente_enut) +
  theme_tp()
guardar_grafico(g_excl, "casos_excluidos.png", ancho = 20, alto = 10)

# ==============================================================================
# 2. VALORES ATÍPICOS
# Muestra con dato de servicio pago (n = 6.042), la misma de la regresión y el PCA
# ==============================================================================

enut_serv <- enut_clean |>
  declarar_tipos() |>
  filter(!is.na(servicio_pago))

# 2.1. Regla del rango intercuartílico sobre el trabajo doméstico
q1         <- unname(quantile(enut_serv$hs_domestico, 0.25))
q3         <- unname(quantile(enut_serv$hs_domestico, 0.75))
limite_sup <- limite_iqr(enut_serv$hs_domestico)

enut_serv <- enut_serv |> mutate(outlier_dom = hs_domestico > limite_sup)
enut_serv_sin <- enut_serv |> filter(!outlier_dom)

criterio <- tibble(
  q1 = q1, q3 = q3, iqr = q3 - q1, limite_superior = limite_sup,
  n_atipicos = sum(enut_serv$outlier_dom),
  pct_atipicos = round(100 * mean(enut_serv$outlier_dom), 2),
  pct_atipicos_ponderado = round(100 * sum(enut_serv$wper[enut_serv$outlier_dom]) / sum(enut_serv$wper), 2),
  minimo_atipico = min(enut_serv$hs_domestico[enut_serv$outlier_dom]),
  maximo_atipico = max(enut_serv$hs_domestico[enut_serv$outlier_dom]),
  p90_muestra = unname(quantile(enut_serv$hs_domestico, 0.9)),
  atipicos_dom_mas_cuidado_24 = sum(enut_serv$outlier_dom & (enut_serv$hs_domestico + enut_serv$hs_cuidado > 24))
) |>
  mutate(across(where(is.numeric), ~ round(.x, 2)))
write_csv(criterio, file.path(out_tablas, "criterio_outliers_domestico.csv"))
criterio

# 2.2. Perfil de los casos atípicos frente al resto
perfil_out <- enut_serv |>
  group_by(outlier_dom) |>
  summarise(
    n                 = n(),
    pct_no_laborable  = round(100 * mean(dia_tipo == "Día No Laborable"), 1),
    pct_ocupada       = round(100 * mean(estado_laboral == "Ocupada"), 1),
    edad_media        = round(mean(edad_sel), 1),
    pct_hasta_sec_inc = round(100 * mean(educacion_4cat == "1. Hasta Sec. Incompleto"), 1),
    .groups = "drop"
  )
write_csv(perfil_out, file.path(out_tablas, "perfil_outliers.csv"))
perfil_out

# 2.3. Descriptivas del trabajo doméstico con y sin atípicos
n_out <- sum(enut_serv$outlier_dom)
desc_outliers <- bind_rows(
  con_atipicos = resumir_dom(enut_serv),
  sin_atipicos = resumir_dom(enut_serv_sin),
  .id = "muestra"
)
write_csv(desc_outliers, file.path(out_tablas, "descriptivas_con_sin_outliers.csv"))
desc_outliers

# 2.4. Otras actividades: límite IQR y casos marcados
# Con muchos ceros el IQR marca como atípicos a casos que no lo son (ver cuidado directo)
vars_act <- c("hs_domestico", "hs_cuidado", "hs_remunerado", "hs_convivencia", "hs_medios")

limites_actividades <- bind_rows(lapply(vars_act, function(v) {
  x <- enut_serv[[v]]
  lim <- limite_iqr(x)
  tibble(variable = v,
         limite_iqr = round(lim, 2),
         n_atipicos = sum(x > lim),
         pct_atipicos = round(100 * mean(x > lim), 1),
         pct_ceros = round(100 * mean(x == 0), 1),
         pct_atipicos_con_ninos = round(100 * mean(enut_serv$cant_personashasta13[x > lim] > 0), 1))
}))
write_csv(limites_actividades, file.path(out_tablas, "limites_iqr_actividades.csv"))
limites_actividades

# Cuidado directo: media y % de ceros con y sin niños en el hogar
cuidado_ninos <- enut_serv |>
  group_by(presencia_ninos) |>
  summarise(n = n(),
            media_cuidado = round(mean(hs_cuidado), 2),
            mediana_cuidado = round(median(hs_cuidado), 2),
            pct_ceros = round(100 * mean(hs_cuidado == 0), 1),
            .groups = "drop")
write_csv(cuidado_ninos, file.path(out_tablas, "cuidado_con_sin_ninos.csv"))
cuidado_ninos

# 2.5. Gráfico: histograma con el umbral de atípicos
g_out <- ggplot(enut_serv, aes(x = hs_domestico)) +
  geom_histogram(binwidth = 0.5, boundary = 0, fill = col_hist, color = "white") +
  geom_vline(xintercept = limite_sup, linetype = "dashed", color = col_texto) +
  annotate("text", x = limite_sup + 0.3, y = Inf, vjust = 2, hjust = 0, size = 3.6,
           color = col_texto,
           label = paste0("Umbral: ", format(round(limite_sup, 1), decimal.mark = ","),
                          " h\n", n_out, " mujeres por encima")) +
  scale_x_continuous(breaks = seq(0, 20, 4), labels = scales::label_number(decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  labs(title = "Trabajo doméstico diario y umbral de valores atípicos",
       subtitle = "Mujeres de 18 a 64 años con dato de servicio pago. Umbral: Q3 + 1,5 × rango intercuartílico.",
       x = "Horas diarias de trabajo doméstico no remunerado",
       y = "Cantidad de mujeres",
       caption = fuente_enut) +
  theme_tp()
guardar_grafico(g_out, "histograma_atipicos_domestico.png")

cat("Script 03 completado: faltantes y outliers (", n_out, " atípicos). Tablas y gráficos exportados.\n")
