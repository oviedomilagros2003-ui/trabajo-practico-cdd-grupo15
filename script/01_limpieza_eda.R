# ==============================================================================
# Script 01: Carga, diagnóstico y limpieza de la base
# Trabajo Práctico - Grupo 15 (ENUT 2021)
# Entradas : raw/enut2021_base.txt, auxiliar/codigos_nivel_educativo.csv
# Salidas  : input/enut_mujeres_18_64.csv (6.077, antes de excluir códigos)
#            input/enut_limpia.csv (6.046, muestra de análisis)
#            output/tablas/diccionario_variables.csv, diccionario_variables_derivadas.csv,
#            analisis_faltantes_inicial.csv, codigos_ns_nc_base_completa.csv,
#            codigos_no_respuesta.csv, tipos_variables.csv, resumen_filtrado_muestra.csv
# ==============================================================================

options(scipen = 999)
library(tidyverse)
library(janitor)

raw_dir    <- "raw"
aux_dir    <- "auxiliar"
input_dir  <- "input"
out_tablas <- file.path("output", "tablas")

dir.create(input_dir,  showWarnings = FALSE)
dir.create(out_tablas, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 1. Carga de la base cruda y chequeo de estructura
# ------------------------------------------------------------------------------

ruta_base <- file.path(raw_dir, "enut2021_base.txt")
if (!file.exists(ruta_base)) stop("No se encuentra ", ruta_base)

enut_raw <- read_delim(ruta_base, delim = "|", quote = "", show_col_types = FALSE) |>
  clean_names()

dim(enut_raw)   # 14.350 filas x 327 columnas

# Unidad de observación: una persona por hogar. Se verifica que el id no se repita.
if ("id" %in% names(enut_raw)) {
  cat("Ids duplicados:", sum(duplicated(enut_raw$id)), "\n")
} else {
  message("La base no trae la columna 'id': verificar la unicidad con el manual de la base.")
}

enut_sel <- enut_raw |>
  select(
    sexo_sel, edad_sel, nivel_educativo_sel,
    tcs_grupo_domestico, tcs_grupo_cuidado, tcs_grupo_trabajoocupacion,
    tcs_grupo_convivencia, tcs_grupo_usodemedios,
    cant_personashasta13, dia_laborable, t_domestico_pago,
    condicion_actividad_agrupada, wper
  )

glimpse(enut_sel)

tipos_variables <- tibble(
  variable = names(enut_sel),
  tipo_r   = sapply(enut_sel, function(x) class(x)[1])
)
write_csv(tipos_variables, file.path(out_tablas, "tipos_variables.csv"))

# ------------------------------------------------------------------------------
# 2. Faltantes en la base completa (NA explícitos y códigos 99)
# ------------------------------------------------------------------------------

vars_categoricas <- c("sexo_sel", "nivel_educativo_sel", "dia_laborable",
                      "t_domestico_pago", "condicion_actividad_agrupada")

faltantes <- enut_sel |>
  summarise(across(
    everything(),
    list(n = ~ sum(is.na(.x)), pct = ~ round(mean(is.na(.x)) * 100, 2)),
    .names = "{.col}__{.fn}"
  )) |>
  pivot_longer(everything(), names_to = c("variable", "medida"), names_sep = "__") |>
  pivot_wider(names_from = medida, values_from = value) |>
  rename(na_n = n, na_pct = pct)

codigos_99 <- enut_sel |>
  summarise(across(all_of(vars_categoricas), ~ sum(.x == 99, na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "ns_nc_99")

write_csv(faltantes,  file.path(out_tablas, "analisis_faltantes_inicial.csv"))
write_csv(codigos_99, file.path(out_tablas, "codigos_ns_nc_base_completa.csv"))

# ------------------------------------------------------------------------------
# 3. Población de estudio: mujeres de 18 a 64 años
# ------------------------------------------------------------------------------

n_base    <- nrow(enut_sel)
n_mujeres <- sum(enut_sel$sexo_sel == 1)

enut_mujeres <- enut_sel |>
  filter(sexo_sel == 1, edad_sel >= 18, edad_sel <= 64)

codigos_no_respuesta <- tibble(
  variable      = c("nivel_educativo_sel", "nivel_educativo_sel", "t_domestico_pago"),
  codigo        = c(99, 8, 99),
  significado   = c("Ns/Nc", "Educación especial", "Ns/Nc"),
  base_completa = c(sum(enut_sel$nivel_educativo_sel == 99, na.rm = TRUE),
                    sum(enut_sel$nivel_educativo_sel == 8,  na.rm = TRUE),
                    sum(enut_sel$t_domestico_pago == 99,    na.rm = TRUE)),
  mujeres_18_64 = c(sum(enut_mujeres$nivel_educativo_sel == 99, na.rm = TRUE),
                    sum(enut_mujeres$nivel_educativo_sel == 8,  na.rm = TRUE),
                    sum(enut_mujeres$t_domestico_pago == 99,    na.rm = TRUE))
) |>
  mutate(pct_mujeres_18_64 = round(100 * mujeres_18_64 / nrow(enut_mujeres), 2))

write_csv(codigos_no_respuesta, file.path(out_tablas, "codigos_no_respuesta.csv"))
codigos_no_respuesta

# ------------------------------------------------------------------------------
# 4. Mapeo educativo (left_join con la tabla auxiliar) y variables derivadas
# ------------------------------------------------------------------------------

codigos_educ <- read_csv(file.path(aux_dir, "codigos_nivel_educativo.csv"),
                         show_col_types = FALSE)

n_antes <- nrow(enut_mujeres)
enut_mujeres <- enut_mujeres |>
  left_join(codigos_educ, by = c("nivel_educativo_sel" = "codigo"))

# Controles del join: no se pierden ni se duplican filas y ningún código queda sin mapear
stopifnot(nrow(enut_mujeres) == n_antes, !anyNA(enut_mujeres$educacion_4cat))

enut_mujeres <- enut_mujeres |>
  mutate(
    hs_domestico   = tcs_grupo_domestico / 60,
    hs_cuidado     = tcs_grupo_cuidado / 60,
    hs_remunerado  = tcs_grupo_trabajoocupacion / 60,
    hs_convivencia = tcs_grupo_convivencia / 60,
    hs_medios      = tcs_grupo_usodemedios / 60,

    presencia_ninos = if_else(cant_personashasta13 > 0, "Con niños (0-13)", "Sin niños"),
    ninos_cat = case_when(
      cant_personashasta13 == 0 ~ "Sin niños",
      cant_personashasta13 == 1 ~ "1 niño/a",
      TRUE                      ~ "2 o más niños"
    ),
    dia_tipo = if_else(dia_laborable == 1, "Día Laborable", "Día No Laborable"),
    servicio_pago = case_when(
      t_domestico_pago == 1 ~ "Con servicio pago",
      t_domestico_pago == 2 ~ "Sin servicio pago",
      TRUE                  ~ NA_character_
    ),
    estado_laboral = if_else(condicion_actividad_agrupada == 1, "Ocupada", "No ocupada")
  )

# Se guarda la muestra de 6.077 antes de excluir (la usa el script 03 para el análisis de faltantes)
write_csv(enut_mujeres, file.path(input_dir, "enut_mujeres_18_64.csv"))

# Muestra de análisis: se excluyen educación especial y Ns/Nc
enut_clean <- enut_mujeres |>
  filter(incluir_analisis == "SI") |>
  select(-descripcion_original, -incluir_analisis)

# Controles tras recodificar
enut_clean |> count(educacion_4cat)
enut_clean |> count(servicio_pago)
enut_clean |> count(estado_laboral)
enut_clean |> count(ninos_cat)
enut_clean |> count(dia_tipo)

write_csv(enut_clean, file.path(input_dir, "enut_limpia.csv"))

# Flujo de la muestra
flujo <- tibble(
  paso   = c("Base completa", "Mujeres", "Mujeres de 18 a 64 años",
             "Nivel educativo válido", "Servicio pago válido"),
  casos  = c(n_base, n_mujeres, nrow(enut_mujeres), nrow(enut_clean),
             sum(!is.na(enut_clean$servicio_pago)))
) |>
  mutate(excluidos = lag(casos) - casos)

write_csv(flujo, file.path(out_tablas, "resumen_filtrado_muestra.csv"))
flujo

# ------------------------------------------------------------------------------
# 5. Diccionario de variables
# ------------------------------------------------------------------------------

diccionario <- tribble(
  ~variable,                      ~descripcion,                                               ~tipo,        ~categorias_rangos,                       ~funcion_estudio,
  "sexo_sel",                     "Sexo asignado al nacer",                                   "Nominal",    "1=Mujer, 2=Varón",                       "Filtro: mujeres",
  "edad_sel",                     "Edad en años cumplidos",                                   "Discreta",   "18 a 64 (filtro aplicado)",              "Filtro y control",
  "nivel_educativo_sel",          "Máximo nivel educativo alcanzado",                         "Ordinal",    "1 a 7; 8 y 99 excluidos",                "Variable explicativa",
  "tcs_grupo_domestico",          "Minutos diarios de trabajo doméstico no pago",             "Continua",   "minutos por día",                        "Variable dependiente",
  "tcs_grupo_cuidado",            "Minutos diarios de cuidado directo en el hogar",           "Continua",   "minutos por día",                        "Variable dependiente",
  "tcs_grupo_trabajoocupacion",   "Minutos diarios de trabajo remunerado",                    "Continua",   "minutos por día",                        "Actividad (PCA)",
  "tcs_grupo_convivencia",        "Minutos diarios de convivencia",                           "Continua",   "minutos por día",                        "Actividad (PCA)",
  "tcs_grupo_usodemedios",        "Minutos diarios de uso de medios",                         "Continua",   "minutos por día",                        "Actividad (PCA)",
  "cant_personashasta13",         "Niños de 0 a 13 años en el hogar",                         "Discreta",   "cantidad de personas",                   "Control",
  "dia_laborable",                "Tipo de día relevado en el diario",                        "Dicotómica", "1=Laborable, 2=No laborable",            "Control",
  "t_domestico_pago",             "Servicio doméstico pago en el hogar",                      "Dicotómica", "1=Sí, 2=No, 99=Ns/Nc",                  "Tercerización",
  "condicion_actividad_agrupada", "Condición de actividad de la mujer",                       "Dicotómica", "1=Ocupada, 2=No ocupada",                "Control",
  "wper",                         "Factor de expansión individual",                           "Discreta",   "enteros positivos",                      "Ponderador"
)

# Valores faltantes (NA + códigos sin respuesta) sobre las 6.077 mujeres de 18 a 64 años
n_na  <- sapply(diccionario$variable, function(v) sum(is.na(enut_mujeres[[v]])))
n_cod <- c(nivel_educativo_sel = sum(enut_mujeres$nivel_educativo_sel %in% c(8, 99)),
           t_domestico_pago    = sum(enut_mujeres$t_domestico_pago == 99, na.rm = TRUE))
diccionario$faltantes_n <- unname(n_na + ifelse(names(n_na) %in% names(n_cod),
                                                n_cod[names(n_na)], 0))

# Mínimo y máximo observados en las variables numéricas (muestra de 6.046)
vars_num_dic <- c("edad_sel", "tcs_grupo_domestico", "tcs_grupo_cuidado",
                  "tcs_grupo_trabajoocupacion", "tcs_grupo_convivencia",
                  "tcs_grupo_usodemedios", "cant_personashasta13", "wper")
rangos <- tibble(
  variable        = vars_num_dic,
  minimo_observado = sapply(vars_num_dic, function(v) min(enut_clean[[v]])),
  maximo_observado = sapply(vars_num_dic, function(v) max(enut_clean[[v]]))
)
diccionario <- diccionario |> left_join(rangos, by = "variable")

write_csv(diccionario, file.path(out_tablas, "diccionario_variables.csv"))

diccionario_derivadas <- tribble(
  ~variable,         ~como_se_calcula,
  "hs_domestico",    "tcs_grupo_domestico / 60 (horas por día)",
  "hs_cuidado",      "tcs_grupo_cuidado / 60 (horas por día)",
  "hs_remunerado",   "tcs_grupo_trabajoocupacion / 60 (horas por día)",
  "hs_convivencia",  "tcs_grupo_convivencia / 60 (horas por día)",
  "hs_medios",       "tcs_grupo_usodemedios / 60 (horas por día)",
  "educacion_4cat",  "Códigos 1-4 = Hasta sec. incompleto; 5 = Sec. completo; 6 = Terc./univ. incompleto; 7 = Terc./univ. completo (tabla auxiliar)",
  "servicio_pago",   "t_domestico_pago: 1 = Con servicio pago; 2 = Sin servicio pago; 99 = NA",
  "estado_laboral",  "condicion_actividad_agrupada: 1 = Ocupada; 2 = No ocupada",
  "presencia_ninos", "Con niños si cant_personashasta13 > 0",
  "ninos_cat",       "Sin niños / 1 niño/a / 2 o más niños",
  "dia_tipo",        "dia_laborable: 1 = Día Laborable; 2 = Día No Laborable"
)
write_csv(diccionario_derivadas, file.path(out_tablas, "diccionario_variables_derivadas.csv"))

cat("Script 01 completado. Muestra de análisis:", nrow(enut_clean), "mujeres.\n")
