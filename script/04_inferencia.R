# ==============================================================================
# Script 04: Inferencia estadística y regresión
# Trabajo Práctico - Grupo 15 (ENUT 2021)
# Entrada : input/enut_limpia.csv
# Salidas : tablas en output/tablas/ y gráficos en output/graficos/
# Métodos : ANOVA / Welch / Games-Howell / Kruskal-Wallis, Chi-cuadrado, regresión MCO (HC1)
#           y modelos de cuidado directo (completo, con niños, dos partes)

options(scipen = 999)
set.seed(2026)

library(tidyverse)
library(car)           # Levene y VIF
library(rstatix)       # Games-Howell
library(lmtest)        # Breusch-Pagan, RESET, coeftest
library(sandwich)      # errores robustos HC1
library(modelsummary)  # gráfico de coeficientes
library(broom)         # tablas de resultados

source(file.path("utils", "tema_scripts.R"))
source(file.path("utils", "guardar_grafico.R"))
source(file.path("utils", "declarar_tipos.R"))
source(file.path("utils", "limite_iqr.R"))

input_dir  <- "input"
out_tablas <- file.path("output", "tablas")
dir.create(out_tablas, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("output", "graficos"), recursive = TRUE, showWarnings = FALSE)  

ruta_base <- file.path(input_dir, "enut_limpia.csv")
if (!file.exists(ruta_base)) {
  stop("No se encuentra '", ruta_base, "'. Ejecute primero script/01_limpieza_eda.R.")
}

enut_reg  <- read_csv(ruta_base, show_col_types = FALSE) |> declarar_tipos()   # 6.046
enut_serv <- enut_reg |> filter(!is.na(servicio_pago))                         # 6.042

# Umbral de atípicos de trabajo doméstico (regla del IQR). Se define sobre las 6.042,
# que es donde se identificaron los 73 casos.                                
umbral_dom <- limite_iqr(enut_serv$hs_domestico)

# ==============================================================================
# 1. ANOVA, Welch, Games-Howell y Kruskal-Wallis: trabajo doméstico según educación
# H0: las 4 categorías educativas tienen la misma media de horas de trabajo doméstico
# H1: al menos una categoría tiene una media distinta
# ==============================================================================

# 1.1. Descriptivas por nivel educativo (sin ponderar)
desc_educ <- enut_reg |>
  group_by(educacion_4cat) |>
  summarise(n = n(),
            media = round(mean(hs_domestico), 2),
            mediana = round(median(hs_domestico), 2),
            desvio = round(sd(hs_domestico), 2),
            .groups = "drop")
write_csv(desc_educ, file.path(out_tablas, "anova_descriptivas_educacion.csv"))

# 1.2. ANOVA clásico y tamaño del efecto (eta cuadrado)
modelo_anova <- aov(hs_domestico ~ educacion_4cat, data = enut_reg)
tabla_aov    <- summary(modelo_anova)[[1]]
eta2         <- tabla_aov$`Sum Sq`[1] / sum(tabla_aov$`Sum Sq`)

# 1.3. Supuestos: normalidad de residuos (QQ-plot) y varianzas iguales (Levene)
g_qq <- ggplot(data.frame(residuos = residuals(modelo_anova)), aes(sample = residuos)) +
  stat_qq(color = col_hist, alpha = 0.4, size = 0.8) +
  stat_qq_line(color = col_texto) +
  labs(title = "Normalidad de los residuos del ANOVA",
       subtitle = "Trabajo doméstico diario según nivel educativo. Si los puntos siguen la recta, hay normalidad.",
       x = "Cuantiles teóricos de la normal", y = "Residuos (horas por día)",
       caption = fuente_enut) +
  theme_tp()
guardar_grafico(g_qq, "qqplot_residuos_anova.png")

lev <- car::leveneTest(hs_domestico ~ educacion_4cat, data = enut_reg)
levene <- tibble(F = lev$`F value`[1], gl1 = lev$Df[1], gl2 = lev$Df[2], p = lev$`Pr(>F)`[1])
write_csv(levene, file.path(out_tablas, "anova_levene.csv"))
levene   # Levene rechaza varianzas iguales: corresponde Welch y Games-Howell

# 1.4. Welch (no supone varianzas iguales) y post-hoc Games-Howell
welch <- oneway.test(hs_domestico ~ educacion_4cat, data = enut_reg)

# En rstatix, estimate = media del group2 - media del group1. La columna "comparacion"                       
gh <- rstatix::games_howell_test(enut_reg, hs_domestico ~ educacion_4cat) |>
  mutate(comparacion = paste(group2, "-", group1)) |>
  select(comparacion, group1, group2, estimate, conf.low, conf.high, p.adj)
write_csv(gh, file.path(out_tablas, "post_hoc_games_howell.csv"))
gh

# 1.5. Kruskal-Wallis (alternativa no paramétrica), epsilon cuadrado y Wilcoxon con Bonferroni
kw   <- kruskal.test(hs_domestico ~ educacion_4cat, data = enut_reg)
eps2 <- unname(kw$statistic) / (nrow(enut_reg) - 1)

wilcox_pares <- broom::tidy(pairwise.wilcox.test(enut_reg$hs_domestico, enut_reg$educacion_4cat,
                                                 p.adjust.method = "bonferroni", exact = FALSE))
write_csv(wilcox_pares, file.path(out_tablas, "post_hoc_wilcoxon_bonferroni.csv"))

anova_resultados <- tibble(
  test = c("ANOVA clásico", "ANOVA de Welch", "Kruskal-Wallis"),
  estadistico = c(tabla_aov$`F value`[1], unname(welch$statistic), unname(kw$statistic)),
  gl = c(paste(tabla_aov$Df[1], tabla_aov$Df[2], sep = " y "),
         paste(round(welch$parameter[1], 0), round(welch$parameter[2], 0), sep = " y "),
         as.character(unname(kw$parameter))),
  p_valor = c(tabla_aov$`Pr(>F)`[1], welch$p.value, kw$p.value),
  tamano_efecto = c(eta2, NA, eps2),
  medida_efecto = c("eta cuadrado", NA, "epsilon cuadrado")
) |>
  mutate(across(where(is.numeric), ~ round(.x, 4)))
write_csv(anova_resultados, file.path(out_tablas, "anova_resultados.csv"))
anova_resultados

# 1.5b. Robustez, los mismos tests sin los casos atípicos de trabajo doméstico
enut_reg_sin <- enut_reg |> filter(hs_domestico <= umbral_dom)

welch_sin <- oneway.test(hs_domestico ~ educacion_4cat, data = enut_reg_sin)
kw_sin    <- kruskal.test(hs_domestico ~ educacion_4cat, data = enut_reg_sin)
eps2_sin  <- unname(kw_sin$statistic) / (nrow(enut_reg_sin) - 1)

anova_sin_outliers <- tibble(
  resultado = c("n", "Welch F", "Kruskal-Wallis H", "Kruskal-Wallis epsilon cuadrado"),
  con_atipicos = c(nrow(enut_reg), unname(welch$statistic), unname(kw$statistic), eps2),
  sin_atipicos = c(nrow(enut_reg_sin), unname(welch_sin$statistic), unname(kw_sin$statistic), eps2_sin)
) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))
write_csv(anova_sin_outliers, file.path(out_tablas, "anova_sin_outliers.csv"))
anova_sin_outliers

# 1.6. Composición de cada nivel educativo 
composicion <- enut_reg |>
  group_by(educacion_4cat) |>
  summarise(
    n                = n(),
    edad_media       = round(mean(edad_sel), 1),
    pct_con_ninos    = round(100 * mean(cant_personashasta13 > 0), 1),
    pct_ocupada      = round(100 * mean(estado_laboral == "Ocupada"), 1),
    pct_no_laborable = round(100 * mean(dia_tipo == "Día No Laborable"), 1),
    hs_domestico     = round(mean(hs_domestico), 2),
    .groups = "drop"
  )
write_csv(composicion, file.path(out_tablas, "composicion_educacion.csv"))
composicion

# ==============================================================================
# 2. Chi-cuadrado: nivel educativo x servicio doméstico pago
# H0: tener servicio pago es independiente del nivel educativo
# H1: la proporción con servicio pago varía con el nivel educativo
# ==============================================================================

tabla_chi <- table(enut_serv$educacion_4cat, enut_serv$servicio_pago)
prop_fila <- round(prop.table(tabla_chi, margin = 1) * 100, 1)

test_chi2 <- chisq.test(tabla_chi)
v_cramer  <- sqrt(unname(test_chi2$statistic) / (sum(tabla_chi) * (min(dim(tabla_chi)) - 1)))

chi2_resultados <- tibble(
  n = sum(tabla_chi),
  chi2 = round(unname(test_chi2$statistic), 2),
  gl = unname(test_chi2$parameter),
  p_valor = test_chi2$p.value,
  v_cramer = round(v_cramer, 3),
  esperada_minima = round(min(test_chi2$expected), 1)   # el supuesto pide que sea mayor a 5
)
write_csv(chi2_resultados, file.path(out_tablas, "chi2_resultados.csv"))
chi2_resultados

write_csv(as.data.frame.matrix(prop_fila) |> rownames_to_column("educacion_4cat"),
          file.path(out_tablas, "chi2_educacion_servicio.csv"))
write_csv(as.data.frame.matrix(tabla_chi) |> rownames_to_column("educacion_4cat"),
          file.path(out_tablas, "chi2_educacion_servicio_conteo.csv"))
write_csv(as.data.frame.matrix(round(test_chi2$stdres, 2)) |> rownames_to_column("educacion_4cat"),
          file.path(out_tablas, "chi2_residuos_estandarizados.csv"))

# Gráfico: % con servicio doméstico pago según nivel educativo
g_terc <- as.data.frame.matrix(prop_fila) |>
  rownames_to_column("educacion_4cat") |>
  mutate(nivel = factor(educacion_4cat, levels = niveles_educ, labels = etiquetas_educ)) |>
  ggplot(aes(x = nivel, y = `Con servicio pago`)) +
  geom_col(fill = col_con, width = 0.7) +
  geom_text(aes(label = paste0(format(`Con servicio pago`, decimal.mark = ","), " %")),
            vjust = -0.4, size = 3.6, color = col_texto) +
  scale_x_discrete(labels = function(x) str_wrap(x, 14)) +
  scale_y_continuous(limits = c(0, 20), labels = function(x) paste0(x, " %")) +
  labs(title = "Mujeres con servicio doméstico pago en el hogar, según nivel educativo",
       subtitle = "Mujeres de 18 a 64 años con dato de servicio pago. Porcentajes sin ponderar.",
       x = "Máximo nivel educativo alcanzado",
       y = "Porcentaje con servicio doméstico pago",
       caption = fuente_enut) +
  theme_tp()
guardar_grafico(g_terc, "barras_tercerizacion_educacion.png")

# ==============================================================================
# 3. Regresión MCO con errores robustos HC1: trabajo doméstico (horas por día)
# ==============================================================================

rhs <- "educacion_4cat + servicio_pago + cant_personashasta13 + estado_laboral + dia_tipo + edad_sel"

tabla_robusta <- function(modelo, nombre) {
  broom::tidy(lmtest::coeftest(modelo, vcov = sandwich::vcovHC(modelo, type = "HC1")),
              conf.int = TRUE) |>
    mutate(modelo = nombre, minutos_por_dia = round(estimate * 60), .before = 1)
}

modelo_dom <- lm(as.formula(paste("hs_domestico ~", rhs)), data = enut_serv)
write_csv(tabla_robusta(modelo_dom, "Trabajo doméstico"),
          file.path(out_tablas, "regresion_domestico_coeficientes.csv"))

res_dom <- summary(modelo_dom)
ajuste_dom <- tibble(
  n = nobs(modelo_dom),
  r2_ajustado = round(res_dom$adj.r.squared, 3),
  estadistico_F = round(res_dom$fstatistic[1], 1),
  gl1 = unname(res_dom$fstatistic[2]),
  gl2 = unname(res_dom$fstatistic[3])
)
write_csv(ajuste_dom, file.path(out_tablas, "regresion_domestico_ajuste.csv"))

# Supuestos: multicolinealidad (GVIF ajustado), heterocedasticidad (Breusch-Pagan), forma funcional (RESET)
# La 4ª columna es GVIF^(1/(2*Df)), el valor comparable entre variables (máximo 1,33).
vif_dom <- as.data.frame(car::vif(modelo_dom)) |> rownames_to_column("variable")
names(vif_dom)[4] <- "gvif_ajustado"
write_csv(vif_dom, file.path(out_tablas, "regresion_domestico_vif.csv"))

bp    <- lmtest::bptest(modelo_dom)
reset <- lmtest::resettest(modelo_dom)
supuestos_dom <- tibble(
  test = c("Breusch-Pagan (varianza constante)", "RESET (forma lineal)"),
  estadistico = c(unname(bp$statistic), unname(reset$statistic)),
  gl = c(paste(unname(bp$parameter)), paste(unname(reset$parameter), collapse = " y ")),
  p_valor = c(bp$p.value, reset$p.value)
) |>
  mutate(estadistico = round(estadistico, 2))
write_csv(supuestos_dom, file.path(out_tablas, "regresion_domestico_supuestos.csv"))
supuestos_dom

# Gráfico de coeficientes
nombres_coef <- c(
  "educacion_4cat2. Secundario Completo"        = "Secundario completo",
  "educacion_4cat3. Terciario/Univ. Incompleto" = "Terciario/univ. incompleto",
  "educacion_4cat4. Terciario/Univ. Completo"   = "Terciario/univ. completo",
  "servicio_pagoCon servicio pago"              = "Con servicio doméstico pago",
  "cant_personashasta13"                        = "Niños de 0 a 13 (por niño/a)",
  "estado_laboralOcupada"                       = "Ocupada",
  "dia_tipoDía No Laborable"                    = "Día no laborable",
  "edad_sel"                                    = "Edad (por año)"
)

g_coef <- modelsummary::modelplot(modelo_dom, vcov = "HC1", coef_omit = "Intercept",
                                  coef_rename = nombres_coef) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "firebrick") +
  labs(title = "Variables asociadas al trabajo doméstico no remunerado",
       subtitle = "Coeficientes e intervalos de confianza del 95 % (errores robustos HC1). Horas por día.",
       x = "Cambio asociado en horas diarias",
       y = NULL,
       caption = paste0(fuente_enut, "\nReferencias: hasta secundario incompleto, sin servicio pago, no ocupada, día laborable.")) +
  theme_tp()
guardar_grafico(g_coef, "modelplot_coeficientes.png", ancho = 22, alto = 13)

# Robustez: la misma regresión sin los casos atípicos de trabajo doméstico
enut_serv_sin  <- enut_serv |> filter(hs_domestico <= umbral_dom)
modelo_dom_sin <- lm(as.formula(paste("hs_domestico ~", rhs)), data = enut_serv_sin)
write_csv(tabla_robusta(modelo_dom_sin, "Trabajo doméstico, sin atípicos"),
          file.path(out_tablas, "regresion_domestico_sin_outliers.csv"))
# ajuste sin atípicos 
write_csv(glance(modelo_dom_sin) |> select(r.squared, adj.r.squared, nobs),
          file.path(out_tablas, "regresion_domestico_sin_outliers_ajuste.csv"))

# Brecha educativa sin controles y con controles 
modelo_bruto <- lm(hs_domestico ~ educacion_4cat, data = enut_serv)
comparacion_brecha <- bind_rows(
  tabla_robusta(modelo_bruto, "Sin controles"),
  tabla_robusta(modelo_dom,   "Con controles")
) |>
  filter(str_starts(term, "educacion_4cat")) |>
  select(modelo, term, estimate, conf.low, conf.high) |>
  mutate(across(where(is.numeric), ~ round(.x, 2)))
write_csv(comparacion_brecha, file.path(out_tablas, "brecha_educacion_sin_y_con_controles.csv"))
comparacion_brecha

# ================================================================
# 4. Cuidado directo: 61 % de ceros 
# ================================================================

# 4.1. Modelo completo (mismas variables)
modelo_cui <- lm(as.formula(paste("hs_cuidado ~", rhs)), data = enut_serv)

# 4.2. Solo mujeres con niños de 0 a 13 años
modelo_cui_ninos <- lm(as.formula(paste("hs_cuidado ~", rhs)),
                       data = filter(enut_serv, cant_personashasta13 > 0))

# 4.3. Dos partes: (1) probabilidad de cuidar algo (logit), (2) horas entre quienes cuidan
enut_serv <- enut_serv |> mutate(cuida_algo = as.integer(hs_cuidado > 0))
parte1 <- glm(as.formula(paste("cuida_algo ~", rhs)), family = binomial, data = enut_serv)
parte2 <- lm(as.formula(paste("hs_cuidado ~", rhs)), data = filter(enut_serv, hs_cuidado > 0))

coef_cuidado <- bind_rows(
  tabla_robusta(modelo_cui,       "Cuidado: modelo completo"),
  tabla_robusta(modelo_cui_ninos, "Cuidado: solo mujeres con niños"),
  broom::tidy(parte1, conf.int = FALSE) |> mutate(modelo = "Cuidado: parte 1 (logit de cuidar algo)", .before = 1),
  tabla_robusta(parte2,           "Cuidado: parte 2 (horas entre quienes cuidan)")
)
write_csv(coef_cuidado, file.path(out_tablas, "regresion_cuidado_coeficientes.csv"))

ajuste_cuidado <- tibble(
  modelo = c("Modelo completo", "Solo mujeres con niños", "Parte 2 (horas entre quienes cuidan)"),
  n = c(nobs(modelo_cui), nobs(modelo_cui_ninos), nobs(parte2)),
  r2_ajustado = round(c(summary(modelo_cui)$adj.r.squared,
                        summary(modelo_cui_ninos)$adj.r.squared,
                        summary(parte2)$adj.r.squared), 3)
)
write_csv(ajuste_cuidado, file.path(out_tablas, "regresion_cuidado_ajuste.csv"))
write_csv(glance(parte1), file.path(out_tablas, "regresion_cuidado_logit_ajuste.csv"))
ajuste_cuidado

cat("Script 04 completado: inferencia y regresiones exportadas.\n")
