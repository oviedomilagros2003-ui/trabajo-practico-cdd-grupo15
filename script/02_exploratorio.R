# ==============================================================================
# Script 02: Análisis exploratorio de datos (EDA)
# Trabajo Práctico - Grupo 15 (ENUT 2021)
# Entrada : input/enut_limpia.csv
# Salidas : tablas en output/tablas/ y gráficos en output/graficos/
# ==============================================================================

options(scipen = 999)
library(tidyverse)
library(skimr)

source(file.path("utils", "tema_scripts.R"))
source(file.path("utils", "guardar_grafico.R"))
source(file.path("utils", "cuantil_pond.R"))

input_dir  <- "input"
out_tablas <- file.path("output", "tablas")
dir.create(out_tablas, recursive = TRUE, showWarnings = FALSE)

ruta_base <- file.path(input_dir, "enut_limpia.csv")
if (!file.exists(ruta_base)) {
  stop("No se encuentra '", ruta_base, "'. Ejecute primero script/01_limpieza_eda.R.")
}

enut_clean <- read_csv(ruta_base, show_col_types = FALSE)

n_txt <- format(nrow(enut_clean), big.mark = ".", decimal.mark = ",")

# ==============================================================================
# 1. Descriptivas muestrales
# ==============================================================================

# 1.1. Resumen general con skim
tabla_skim <- enut_clean |>
  select(hs_domestico, hs_cuidado, hs_remunerado, hs_convivencia, hs_medios,
         edad_sel, cant_personashasta13) |>
  skim() |>
  as_tibble()
write_csv(tabla_skim, file.path(out_tablas, "descriptivas_skim.csv"))

# 1.2. Descriptivas de las variables numéricas (media, dispersión, percentiles, % de ceros)
descriptivas <- enut_clean |>
  select(hs_domestico, hs_cuidado, hs_remunerado, hs_convivencia, hs_medios,
         edad_sel, cant_personashasta13) |>
  pivot_longer(everything(), names_to = "variable", values_to = "valor") |>
  group_by(variable) |>
  summarise(
    n         = n(),
    media     = mean(valor),
    mediana   = median(valor),
    desvio    = sd(valor),
    minimo    = min(valor),
    p25       = unname(quantile(valor, 0.25)),
    p75       = unname(quantile(valor, 0.75)),
    p90       = unname(quantile(valor, 0.90)),
    maximo    = max(valor),
    pct_ceros = 100 * mean(valor == 0),
    .groups   = "drop"
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 2)))
write_csv(descriptivas, file.path(out_tablas, "descriptivas_numericas.csv"))
descriptivas

# 1.3. Jornadas que superan las 24 horas (la ENUT permite actividades simultáneas)
jornadas_excesivas <- enut_clean |>
  summarise(
    cuidado_mas_24   = sum(hs_cuidado > 24),
    domestico_mas_24 = sum(hs_domestico > 24),
    suma_mas_24      = sum(hs_domestico + hs_cuidado > 24)
  )
write_csv(jornadas_excesivas, file.path(out_tablas, "jornadas_mas_24hs.csv"))
jornadas_excesivas

# 1.4. Frecuencia simple del nivel educativo
frec_educ <- enut_clean |>
  count(educacion_4cat, name = "casos") |>
  mutate(pct = round(100 * casos / sum(casos), 1), acumulado = cumsum(pct))
write_csv(frec_educ, file.path(out_tablas, "frecuencia_nivel_educativo.csv"))
frec_educ

# 1.5. Tabla cruzada: nivel educativo y servicio doméstico pago
tabla_cruzada_conteo <- enut_clean |>
  filter(!is.na(servicio_pago)) |>
  count(educacion_4cat, servicio_pago) |>
  pivot_wider(names_from = servicio_pago, values_from = n, values_fill = 0)
write_csv(tabla_cruzada_conteo, file.path(out_tablas, "tabla_cruzada_tercerizacion_conteo.csv"))

tabla_cruzada_pct <- enut_clean |>
  filter(!is.na(servicio_pago)) |>
  count(educacion_4cat, servicio_pago) |>
  group_by(educacion_4cat) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = servicio_pago, values_from = pct, values_fill = 0)
write_csv(tabla_cruzada_pct, file.path(out_tablas, "tabla_cruzada_tercerizacion_pct.csv"))
tabla_cruzada_pct

# ==============================================================================
# 2. Tablas ponderadas con wper y cruces
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
  mutate(dif_puntos = round(pct_servicio_pago - lag(pct_servicio_pago), 1),
         veces      = round(pct_servicio_pago / lag(pct_servicio_pago), 1))
write_csv(variacion_tercerizacion, file.path(out_tablas, "variacion_tercerizacion.csv"))
variacion_tercerizacion

# 2.2. Control por tipo de día
tabla_2_dias <- enut_clean |>
  group_by(educacion_4cat, dia_tipo) |>
  summarise(
    casos             = n(),
    hs_domestico_prom = round(weighted.mean(hs_domestico, w = wper), 2),
    hs_cuidado_prom   = round(weighted.mean(hs_cuidado, w = wper), 2),
    .groups = "drop"
  )
write_csv(tabla_2_dias, file.path(out_tablas, "tabla_2_control_dias.csv"))

# Mezcla de días: % de respuestas de día no laborable en cada nivel
mezcla_dias <- enut_clean |>
  group_by(educacion_4cat) |>
  summarise(pct_dia_no_laborable = round(100 * mean(dia_laborable == 2), 1), .groups = "drop")
write_csv(mezcla_dias, file.path(out_tablas, "mezcla_dias_por_educacion.csv"))
mezcla_dias

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
    .groups = "drop"
  )
write_csv(tabla_3_ocupadas, file.path(out_tablas, "tabla_3_mujeres_ocupadas_ninos.csv"))
tabla_3_ocupadas

# 2.4. Cruces con otras variables (devolución de la instancia 1)
tabla_cruces <- enut_clean |>
  select(hs_domestico, hs_cuidado, wper, servicio_pago, estado_laboral, ninos_cat, dia_tipo) |>
  pivot_longer(cols = c(servicio_pago, estado_laboral, ninos_cat, dia_tipo),
               names_to = "variable", values_to = "grupo") |>
  filter(!is.na(grupo)) |>
  group_by(variable, grupo) |>
  summarise(
    casos             = n(),
    hs_domestico_prom = round(weighted.mean(hs_domestico, w = wper), 2),
    hs_cuidado_prom   = round(weighted.mean(hs_cuidado, w = wper), 2),
    .groups = "drop"
  ) |>
  group_by(variable) |>
  mutate(pct_muestra = round(100 * casos / sum(casos), 1)) |>
  ungroup()
write_csv(tabla_cruces, file.path(out_tablas, "tabla_cruces_adicionales.csv"))
tabla_cruces

# 2.5. Perfil de cada nivel educativo (ponderado)
perfil_educ <- enut_clean |>
  group_by(educacion_4cat) |>
  summarise(
    casos             = n(),
    edad_media        = round(weighted.mean(edad_sel, w = wper), 1),
    pct_ocupadas      = round(100 * weighted.mean(estado_laboral == "Ocupada", w = wper), 1),
    pct_con_ninos     = round(100 * weighted.mean(presencia_ninos == "Con niños (0-13)", w = wper), 1),
    pct_servicio_pago = round(100 * weighted.mean(servicio_pago == "Con servicio pago",
                                                  w = wper, na.rm = TRUE), 1),
    .groups = "drop"
  )
write_csv(perfil_educ, file.path(out_tablas, "perfil_por_nivel_educativo.csv"))
perfil_educ

# ==============================================================================
# 3. Gráficos
# ==============================================================================

enut_graf <- enut_clean |>
  mutate(nivel = factor(educacion_4cat, levels = niveles_educ, labels = etiquetas_educ))

# 3.1. Histograma del trabajo doméstico
g_hist <- ggplot(enut_graf, aes(x = hs_domestico)) +
  geom_histogram(binwidth = 0.5, boundary = 0, fill = col_hist, color = "white") +
  scale_x_continuous(breaks = seq(0, 20, 4), labels = scales::label_number(decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  labs(title = "Distribución del trabajo doméstico diario",
       subtitle = paste0("Mujeres de 18 a 64 años (n = ", n_txt, "). Intervalos de media hora."),
       x = "Horas diarias de trabajo doméstico no remunerado",
       y = "Cantidad de mujeres",
       caption = fuente_enut) +
  theme_tp()
guardar_grafico(g_hist, "histograma_trabajo_domestico.png")

# 3.2. Boxplot del trabajo doméstico por nivel educativo
g_box <- ggplot(enut_graf, aes(x = nivel, y = hs_domestico, fill = nivel)) +
  geom_boxplot(outlier.alpha = 0.3, show.legend = FALSE) +
  scale_fill_manual(values = cols_educ) +
  scale_x_discrete(labels = function(x) str_wrap(x, 14)) +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",")) +
  labs(title = "Trabajo doméstico diario según nivel educativo",
       subtitle = paste0("Mujeres de 18 a 64 años (n = ", n_txt, "). Horas por día."),
       x = "Máximo nivel educativo alcanzado",
       y = "Horas diarias de trabajo doméstico no remunerado",
       caption = fuente_enut) +
  theme_tp()
guardar_grafico(g_box, "boxplot_domestico_educacion.png")

# 3.3. Cuidado directo de mujeres ocupadas con niños, por nivel educativo
cuidado_con_ninos <- tabla_3_ocupadas |>
  filter(presencia_ninos == "Con niños (0-13)") |>
  mutate(nivel = factor(educacion_4cat, levels = niveles_educ, labels = etiquetas_educ))

g_cuidado <- ggplot(cuidado_con_ninos, aes(x = nivel, y = hs_cuidado_prom)) +
  geom_col(fill = col_con, width = 0.7) +
  geom_text(aes(label = format(hs_cuidado_prom, decimal.mark = ",", nsmall = 1)),
            vjust = -0.4, size = 3.5, color = col_texto) +
  scale_x_discrete(labels = function(x) str_wrap(x, 14)) +
  scale_y_continuous(limits = c(0, 6), labels = scales::label_number(decimal.mark = ",")) +
  labs(title = "Cuidado directo de mujeres ocupadas con niños/as de 0 a 13 años",
       subtitle = "Promedio ponderado con wper, horas por día. El eje vertical llega hasta 6 horas.",
       x = "Máximo nivel educativo alcanzado",
       y = "Horas diarias de cuidado directo",
       caption = fuente_enut) +
  theme_tp()
guardar_grafico(g_cuidado, "barras_cuidado_ocupadas_ninos.png")

# 3.4. Trabajo doméstico según tipo de día
dias_graf <- tabla_2_dias |>
  mutate(nivel = factor(educacion_4cat, levels = niveles_educ, labels = etiquetas_educ),
         dia_tipo = factor(dia_tipo, levels = c("Día Laborable", "Día No Laborable")))

g_dias <- ggplot(dias_graf, aes(x = nivel, y = hs_domestico_prom, fill = dia_tipo)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  scale_fill_manual(values = cols_dias, name = NULL) +
  scale_x_discrete(labels = function(x) str_wrap(x, 14)) +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",")) +
  labs(title = "Trabajo doméstico diario según tipo de día",
       subtitle = "Promedio ponderado con wper, horas por día, por nivel educativo.",
       x = "Máximo nivel educativo alcanzado",
       y = "Horas diarias de trabajo doméstico",
       caption = fuente_enut) +
  theme_tp()
guardar_grafico(g_dias, "barras_domestico_tipo_dia.png")

# ==============================================================================
# 4. Percentiles, ratios y correlaciones
# ==============================================================================

percentiles_dom <- unname(quantile(enut_clean$hs_domestico, probs = c(0.10, 0.25, 0.50, 0.75, 0.90)))

ratios_tabla <- tibble(
  p10 = percentiles_dom[1], p25 = percentiles_dom[2], p50 = percentiles_dom[3],
  p75 = percentiles_dom[4], p90 = percentiles_dom[5]
) |>
  mutate(ratio_90_10 = round(p90 / p10, 2),
         ratio_75_25 = round(p75 / p25, 2),
         ratio_50_10 = round(p50 / p10, 2),
         ratio_90_50 = round(p90 / p50, 2))
write_csv(ratios_tabla, file.path(out_tablas, "ratios_percentiles.csv"))
ratios_tabla

vars_num <- enut_clean |>
  select(hs_domestico, hs_cuidado, edad_sel, cant_personashasta13)

mat_cor      <- cor(vars_num, use = "pairwise.complete.obs")
mat_spearman <- cor(vars_num, use = "pairwise.complete.obs", method = "spearman")

write_csv(as.data.frame(round(mat_cor, 3)) |> rownames_to_column("variable"),
          file.path(out_tablas, "matriz_correlaciones_pearson.csv"))
write_csv(as.data.frame(round(mat_spearman, 3)) |> rownames_to_column("variable"),
          file.path(out_tablas, "matriz_correlaciones_spearman.csv"))

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

# Gráfico de la matriz de correlaciones de Pearson
nombres_cor <- c(hs_domestico = "Trabajo doméstico (h)", hs_cuidado = "Cuidado directo (h)",
                 edad_sel = "Edad (años)", cant_personashasta13 = "Niños 0-13 (cantidad)")

cor_graf <- as.data.frame(as.table(mat_cor)) |>
  as_tibble() |>
  rename(var1 = Var1, var2 = Var2, r = Freq) |>
  mutate(var1 = factor(nombres_cor[as.character(var1)], levels = nombres_cor),
         var2 = factor(nombres_cor[as.character(var2)], levels = rev(nombres_cor)))

g_cor <- ggplot(cor_graf, aes(x = var1, y = var2, fill = r)) +
  geom_tile(color = "white") +
  geom_text(aes(label = format(round(r, 2), decimal.mark = ",")), size = 4, color = col_texto) +
  scale_fill_gradient2(low = "#4575b4", mid = "white", high = col_con,
                       midpoint = 0, limits = c(-1, 1), name = "Correlación") +
  scale_x_discrete(labels = function(x) str_wrap(x, 12)) +
  labs(title = "Correlaciones de Pearson entre variables numéricas",
       subtitle = paste0("Mujeres de 18 a 64 años (n = ", n_txt, ")."),
       x = NULL, y = NULL, caption = fuente_enut) +
  theme_tp() +
  theme(panel.grid = element_blank())
guardar_grafico(g_cor, "matriz_correlaciones.png")

# ==============================================================================
# 5. Controles de consistencia: ponderado vs. sin ponderar
# ==============================================================================

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
    servicio_simple  = round(100 * mean(servicio_pago == "Con servicio pago", na.rm = TRUE), 1),
    .groups = "drop"
  )
write_csv(comparacion_pond_simple, file.path(out_tablas, "comparacion_ponderado_vs_simple.csv"))

medianas_educ <- enut_clean |>
  group_by(educacion_4cat) |>
  summarise(
    mediana_dom_simple = round(median(hs_domestico), 2),
    mediana_dom_pond   = round(cuantil_pond(hs_domestico, wper, 0.5), 2),
    mediana_cui_simple = round(median(hs_cuidado), 2),
    mediana_cui_pond   = round(cuantil_pond(hs_cuidado, wper, 0.5), 2),
    .groups = "drop"
  )
write_csv(medianas_educ, file.path(out_tablas, "medianas_educacion.csv"))

dom_educ_estado <- enut_clean |>
  group_by(educacion_4cat, estado_laboral) |>
  summarise(casos     = n(),
            domestico = round(weighted.mean(hs_domestico, w = wper), 2),
            cuidado   = round(weighted.mean(hs_cuidado, w = wper), 2),
            .groups = "drop")
write_csv(dom_educ_estado, file.path(out_tablas, "domestico_cuidado_educ_estado_laboral.csv"))

dom_educ_ninos <- enut_clean |>
  group_by(educacion_4cat, presencia_ninos) |>
  summarise(casos     = n(),
            domestico = round(weighted.mean(hs_domestico, w = wper), 2),
            cuidado   = round(weighted.mean(hs_cuidado, w = wper), 2),
            .groups = "drop")
write_csv(dom_educ_ninos, file.path(out_tablas, "domestico_cuidado_educ_ninos.csv"))

prob <- c(0.10, 0.50, 0.90)
percentiles_comparacion <- tibble(
  percentil    = prob,
  horas_simple = as.numeric(quantile(enut_clean$hs_domestico, prob)),
  horas_pond   = sapply(prob, function(p) cuantil_pond(enut_clean$hs_domestico, enut_clean$wper, p))
)
write_csv(percentiles_comparacion, file.path(out_tablas, "percentiles_simple_vs_ponderado.csv"))

cuidado_ocupadas_ninos <- enut_clean |>
  filter(estado_laboral == "Ocupada", presencia_ninos == "Con niños (0-13)") |>
  group_by(educacion_4cat) |>
  summarise(
    casos          = n(),
    cuidado_pond   = round(weighted.mean(hs_cuidado, w = wper), 2),
    cuidado_simple = round(mean(hs_cuidado), 2),
    desvio         = round(sd(hs_cuidado), 2),
    error_estandar = round(sd(hs_cuidado) / sqrt(n()), 2),
    .groups = "drop"
  )
write_csv(cuidado_ocupadas_ninos, file.path(out_tablas, "cuidado_ocupadas_ninos_robustez.csv"))

cat("Script 02 completado: tablas y gráficos del EDA generados.\n")
