# TRABAJO PRÁCTICO — GRUPO 15
# Script 01: Configuración Inicial, Carga y Limpieza de Datos
# Tema: Educación, tercerización y trabajo no remunerado (ENUT 2021, INDEC)

# REQUISITOS (Descomentar solo si corrés en Colab o por primera vez localmente)  
# install.packages(c("janitor", "skimr")) 

# 1. Configuración de opciones globales (evita notación científica) 
options(scipen = 999)
# 2. Carga de librerías de la cátedra
library(tidyverse)
library(janitor)
library(skimr)

# 3. Definición de la estructura de carpetas (estándar de la cátedra) 
raw_dir          <- "raw"                         # Entrada: base cruda INDEC 
input_dir        <- "input"                       # Salida: base limpia procesada 
out_dir          <- "output"                      # Salida general de resultados 
out_tablas       <- file.path(out_dir, "tablas")  # Tablas .csv 
out_graficos     <- file.path(out_dir, "graficos")# Gráficos .png / .jpg

# 4. Creación automática de directorios si no existen 
dir.create(raw_dir,    showWarnings = FALSE) 
dir.create(input_dir,  showWarnings = FALSE) 

# 1. CARGA DE BASE CRUDA Y SELECCIÓN DE VARIABLES 
# Pregunta guía: ¿Las mujeres de mayor nivel educativo reducen sus minutos diarios 
# de trabajo doméstico no remunerado propio mediante la tercerización paga, 
# y cómo responde el tiempo de cuidado directo?

# 1.1. Ruta al archivo original y control de existencia 
nombre_archivo <- "enut2021_base.txt" 
ruta_base      <- file.path(raw_dir, nombre_archivo)
if (!file.exists(ruta_base)) { 
  stop("No se encuentra '", ruta_base, "'. Por favor, colocá el archivo en la carpeta '", 
       raw_dir, "' y volvé a ejecutar el script.") 
}
# 1.2. Carga de la base original y normalización de nombres a snake_case 
enut_raw <- read_delim( 
  file = ruta_base, 
  delim = "|", 
  quote = "", 
  show_col_types = FALSE 
) |> clean_names()
# Verificación de dimensiones de la base cruda (esperado: 14.350 filas x 327 columnas) 
dim(enut_raw) 
 
# 1.3. Selección de las 13 variables clave para el análisis 
enut_sel <- enut_raw |> 
  select( 
    # Variables de identificación y filtros sociodemográficos 
    sexo_sel,                       # Filtro: sexo asignado (1 = Mujer) 
    edad_sel,                       # Filtro: edad activa (18 a 64 años) 
    nivel_educativo_sel,            # Variable explicativa (nivel educativo) 
# Variables dependientes de tiempo (minutos diarios) 
    tcs_grupo_domestico,            # Dependiente principal: trabajo doméstico no remunerado 
    tcs_grupo_cuidado,              # Dependiente principal: cuidado directo a miembros del hogar 
    tcs_grupo_trabajoocupacion,     # Actividad: trabajo remunerado / mercado 
    tcs_grupo_convivencia,          # Actividad: tiempo de convivencia y vida social 
    tcs_grupo_usodemedios,          # Actividad: uso de medios de comunicación 
 # Controles demográficos y de hogar 
    cant_personashasta13,           # Control: presencia/cantidad de niños (0 a 13 años) 
    dia_laborable,                  # Control: tipo de día relevado (semana / fin de semana) 
    t_domestico_pago,               # Mecanismo: contratación de servicio doméstico pago 
    condicion_actividad_agrupada,   # Control: inserción laboral (Ocupado/Desocupado/Inactivo) 
 
    # Ponderador muestral 
    wper                            # Factor de expansión individual
     ) 
 
# Verificación de la estructura seleccionada (esperado: 14.350 filas x 13 columnas) 
dim(enut_sel) 
 
# 1.4. DICCIONARIO DE VARIABLES 
# Objeto de R con las 13 variables seleccionadas. Se exporta a output/tablas/
diccionario <- tribble( 
  ~variable,                      ~descripcion,                                                 ~tipo,        ~categorias_rangos,                          ~funcion_estudio, 
  "sexo_sel",                     "Sexo asignado al nacer",                                     "Nominal",    "1=Mujer, 2=Varón",                          "Filtro: mujeres", 
  "edad_sel",                     "Edad en años cumplidos",                                     "Discreta",   "18 a 64 años (filtro aplicado)",            "Filtro: edad activa", 
  "nivel_educativo_sel",          "Máximo nivel educativo alcanzado",                           "Ordinal",    "1 a 7 (4 categorías); 8 y 99 excluidos",    "Variable explicativa", 
  "tcs_grupo_domestico",          "Minutos diarios de trabajo doméstico no pago",               "Continua",   "0 a 1170 min.",                             "Variable dependiente", 
  "tcs_grupo_cuidado",            "Minutos diarios de cuidado directo a miembros del hogar",    "Continua",   "0 a 1860 min.",                             "Variable dependiente", 
  "cant_personashasta13",         "Cantidad de niños de 0 a 13 años en el hogar",               "Discreta",   "0 a 6 niños (muestra final)",               "Variable de control", 
  "wper",                         "Factor de expansión poblacional individual",                 "Discreta",   "Enteros positivos",                         "Ponderador", 
  "dia_laborable",                "Tipo de día relevado en el diario de actividades",           "Dicotómica", "1=Laborable, 2=No laborable",               "Variable de control", 
  "t_domestico_pago",             "Presencia de servicio doméstico pago en el hogar",           "Dicotómica", "1=Sí, 2=No (99=Ns/Nc)",                    "Mecanismo directo", 
  "condicion_actividad_agrupada", "Inserción laboral de la persona seleccionada",               "Nominal",    "1=Ocupada, 2=No ocupada",                   "Variable de control", 
  "tcs_grupo_trabajoocupacion",   "Minutos diarios de trabajo remunerado",                        "Continua",   "0 a 1440 min.",                             "Actividad (PCA)", 
  "tcs_grupo_convivencia",        "Minutos diarios de convivencia",                               "Continua",   "0 a 1440 min.",                             "Actividad (PCA)",

    "tcs_grupo_usodemedios",        "Minutos diarios de uso de medios",                             "Continua",   "0 a 1440 min.",                             "Actividad (PCA)" 
) 
 
# Exportación del diccionario a la carpeta output/tablas 
write_csv(diccionario, file.path(out_tablas, "diccionario_variables.csv")) 
 
# Visualización en consola 
print(diccionario, n = Inf)
dir.create(out_tablas,   recursive = TRUE, showWarnings = FALSE) 

# SECCIÓN 2. DIAGNÓSTICO DE ESTRUCTURA, UNICIDAD Y TIPOS (Base inicial) 
# Objetivo: Primer contacto con los datos y checklist de higiene 
# 2.1. Dimensiones y estructura de la base seleccionada 
# Esperado: 14.350 filas x 13 columnas (incluyendo las variables para PCA) 
dim(enut_sel) 
glimpse(enut_sel)
# Verificación de dimensiones, tipos de datos R, rangos y unicidad de observación.

# 2.2. Unidad de observación y unicidad 
# Cada fila representa a una persona elegida en el hogar mediante Tabla de Kish. 
# Verificamos que el ID de persona no tenga duplicados (esperado: 0 duplicados). 
if ("id" %in% names(enut_raw)) { 
  duplicados_id <- sum(duplicated(enut_raw$id)) 
  message("Duplicados en la clave de ID: ", duplicados_id) 
} else { 
  message("Filas totales en la base: ", nrow(enut_raw), " | Filas únicas: ", n_distinct(enut_raw)) 
} 
# 2.3. Tipificación inicial que asignó R a cada variable 
tipos_iniciales <- tibble( 
  variable = names(enut_sel), 
  tipo_r   = map_chr(enut_sel, ~ class(.x)) 
) 
print(tipos_iniciales, n = Inf) 
 
# 2.4. Resumen estadístico inicial y detección de faltantes encubiertos (ej: 99) 
summary(enut_sel) 
skim(enut_sel)

# SECCIÓN 3. AUDITORÍA DE VALORES FALTANTES Y CÓDIGOS ENCUBIERTOS 
# Objetivo: Distinguir faltantes explícitos (NA) de encubiertos (99=Ns/Nc, 8=Educ. Especial) 
# Universo: Base seleccionada completa (14.350 casos) antes de aplicar filtros.
# 3.1. Faltantes explícitos (NA): cantidad y porcentaje por variable 
cat("\n===================================================\n") 
cat(" 3.1. AUDITORÍA DE FALTANTES EXPLÍCITOS (NA)\n") 
cat("===================================================\n")
faltantes <- enut_sel |> 
  summarise(across( 
    everything(), 
    list(n = ~ sum(is.na(.x)), pct = ~ round(mean(is.na(.x)) * 100, 2)), 
    .names = "{.col}__{.fn}" 
  )) |> 
  pivot_longer(everything(), names_to = c("variable", "medida"), names_sep = "__") |> 
  pivot_wider(names_from = medida, values_from = value) |> 
  rename(na_n = n, na_pct = pct) 
 
print(faltantes, n = Inf)
# 3.2. Categorías reales presentes en las variables categóricas 
cat("\n===================================================\n") 
cat(" 3.2. CATEGORÍAS REALES EN VARIABLES CATEGÓRICAS\n") 
cat("===================================================\n") 
 
vars_categoricas <- c("sexo_sel", "nivel_educativo_sel", "dia_laborable", 
                      "t_domestico_pago", "condicion_actividad_agrupada")

categorias_reales <- enut_sel |> 
  select(all_of(vars_categoricas)) |> 
  pivot_longer(everything(), names_to = "variable", values_to = "codigo") |> 
  count(variable, codigo) 
 
print(categorias_reales, n = Inf) 
 
# 3.3. Faltantes encubiertos: código 99 (Ns/Nc) y código 8 (Educación Especial) 
cat("\n===================================================\n") 
cat(" 3.3. AUDITORÍA DE FALTANTES ENCUBIERTOS (CÓDIGOS 99 Y 8)\n") 
cat("===================================================\n")

codigos_99 <- enut_sel |> 
  summarise(across(all_of(vars_categoricas), ~ sum(.x == 99, na.rm = TRUE))) |> 
  pivot_longer(everything(), names_to = "variable", values_to = "ns_nc_99") 
 
cat("Frecuencia de códigos 99 (Ns/Nc) por variable:\n") 
print(codigos_99) 
 
educ_especial_8 <- sum(enut_sel$nivel_educativo_sel == 8, na.rm = TRUE) 
cat("\nCasos totales con código 8 (Educación Especial):", educ_especial_8, "\n") 

# 3.4. INTEGRACIÓN Y EXPORTACIÓN DEL DICCIONARIO CON AUDITORÍA DE NULOS 
# ----------------------------------------------------------------------------- 
cat("\n===================================================\n") 
cat(" 3.4. INTEGRACIÓN DE NULOS AL DICCIONARIO Y EXPORTACIÓN\n") 
cat("===================================================\n") 
 
# Se completa el diccionario con las métricas de nulos (na_n y ns_nc_99) 
diccionario <- diccionario |> 
  left_join(select(faltantes, variable, na_n), by = "variable") |> 
  left_join(codigos_99, by = "variable")

# Exportación de tablas de auditoría a la carpeta output/tablas 
write_csv(faltantes,   file.path(out_tablas, "analisis_faltantes_inicial.csv")) 
write_csv(codigos_99,  file.path(out_tablas, "codigos_ns_nc_base_completa.csv")) 
write_csv(diccionario, file.path(out_tablas, "diccionario_variables.csv")) 
 
cat("Vistas de auditoría del diccionario actualizado:\n") 
print(diccionario |> select(variable, na_n, ns_nc_99), n = Inf) 
 
# ----------------------------------------------------------------------------- 
# 3.5. AUDITORÍA DE VARIABLES CONTINUAS DE ACTIVIDAD (TIEMPO EN MINUTOS) 
# ----------------------------------------------------------------------------- 
cat("\n===================================================\n") 
cat(" 3.5. AUDITORÍA DE VARIABLES DE TIEMPO (MINUTOS)\n")

cat("===================================================\n") 
 
# Lista de las 5 variables de tiempo en minutos (incluye activas para PCA) 
vars_min <- c("tcs_grupo_domestico", 
              "tcs_grupo_cuidado", 
              "tcs_grupo_trabajoocupacion", 
              "tcs_grupo_convivencia", 
              "tcs_grupo_usodemedios") 
 
reporte_minutos <- enut_sel |> 
  summarise(across( 
    all_of(vars_min), 
    list( 
      na       = ~ sum(is.na(.x)), 
      min      = ~ min(.x, na.rm = TRUE), 
      max      = ~ max(.x, na.rm = TRUE), 
      pct_cero = ~ round(100 * mean(.x == 0, na.rm = TRUE), 1) 
    ), 
    .names = "{.col}__{.fn}" 
  )) |> 
  pivot_longer(everything(), names_to = c("variable", "medida"), names_sep = "__") |> 
  pivot_wider(names_from = medida, values_from = value) 
 
print(reporte_minutos, width = Inf) 

# CELDA 4. SELECCIÓN DE LA MUESTRA Y AUDITORÍA DE EXCLUSIONES 
# Muestra objetivo: Mujeres de 18 a 64 años con nivel educativo informado.
# 4.1. Filtros de la muestra 
enut_mujeres  <- enut_sel |> filter(sexo_sel == 1) 
enut_activa   <- enut_mujeres |> filter(edad_sel >= 18, edad_sel <= 64) 
enut_filtrada <- enut_activa |> filter(!nivel_educativo_sel %in% c(8, 99))
# 4.2. Resumen de selección de la muestra 
tabla_filtrado <- tibble( 
  etapa = c("Base seleccionada inicial", 
            "Solo mujeres", 
            "Edad activa (18 a 64 años)", 
            "Nivel educativo informado (Muestra final)"), 
  casos_retenidos = c(nrow(enut_sel), nrow(enut_mujeres), nrow(enut_activa), nrow(enut_filtrada)) 
) |> 
  mutate(casos_excluidos = lag(casos_retenidos) - casos_retenidos) 
 
write_csv(tabla_filtrado, file.path(out_tablas, "resumen_filtrado_muestra.csv")) 
tabla_filtrado 
 
# 4.3. Perfil de casos excluidos vs. muestra de análisis 
perfil_faltantes <- enut_activa |> 
  mutate(grupo = case_when( 
    nivel_educativo_sel %in% c(8, 99) ~ "Excluidas por nivel educativo (8 o 99)", 
    t_domestico_pago == 99            ~ "Excluidas por servicio pago (99)", 
    TRUE                              ~ "Muestra final de análisis" 
  )) |> 
  group_by(grupo) |> 
  summarise( 
    casos       = n(), 
    edad_media  = round(mean(edad_sel), 1), 
    pct_ocupada = round(100 * mean(condicion_actividad_agrupada == 1), 1), 
    .groups     = "drop"

   ) 
 
write_csv(perfil_faltantes, file.path(out_tablas, "perfil_casos_excluidos.csv")) 
 
# 4.4. Comparación de estadísticas globales (antes vs. después del filtro) 
resumir_muestra <- function(df) { 
  df |> summarise( 
    casos       = n(), 
    edad_media  = round(mean(edad_sel), 1), 
    dom_media   = round(mean(tcs_grupo_domestico / 60), 2), 
    dom_mediana = round(median(tcs_grupo_domestico / 60), 2), 
    cui_media   = round(mean(tcs_grupo_cuidado / 60), 2), 
    pct_ocupada = round(100 * mean(condicion_actividad_agrupada == 1), 1) 
  ) 
} 
 
comparacion_muestras <- bind_rows( 
  "Antes de excluir (N=6.077)" = resumir_muestra(enut_activa), 
  "Muestra final (N=6.046)"    = resumir_muestra(enut_filtrada), 
  .id = "etapa" 
) 
 
write_csv(comparacion_muestras, file.path(out_tablas, "impacto_exclusion_estadisticas.csv")) 
comparacion_muestras

# CELDA 5. RECODIFICACIÓN DE VARIABLES Y GUARDADO DE BASE LIMPIA
# Recodificación y variables nuevas 
enut_clean <- enut_filtrada |> 
  mutate( 
    # Minutos a horas diarias 
    hs_domestico   = tcs_grupo_domestico / 60,
 hs_cuidado     = tcs_grupo_cuidado / 60, 
    hs_remunerado  = tcs_grupo_trabajoocupacion / 60, 
    hs_convivencia = tcs_grupo_convivencia / 60, 
    hs_medios      = tcs_grupo_usodemedios / 60, 
 
    # Nivel educativo en 4 categorías 
    educacion_4cat = case_when( 
      nivel_educativo_sel %in% c(1, 2, 3, 4) ~ "1. Hasta Sec. Incompleto", 
      nivel_educativo_sel == 5               ~ "2. Secundario Completo", 
      nivel_educativo_sel == 6               ~ "3. Terciario/Univ. Incompleto", 
      nivel_educativo_sel == 7               ~ "4. Terciario/Univ. Completo", 
      TRUE                                   ~ NA_character_ 
    ), 
 
    # Presencia de niños (0 a 13 años) 
    presencia_ninos = if_else(cant_personashasta13 > 0, "Con niños (0-13)", "Sin niños"), 
    ninos_cat = case_when( 
      cant_personashasta13 == 0 ~ "Sin niños", 
      cant_personashasta13 == 1 ~ "1 niño/a", 
      TRUE                      ~ "2 o más niños" 
    ), 
 
    # Tipo de día 
    dia_tipo = if_else(dia_laborable == 1, "Día Laborable", "Día No Laborable"), 
 
    # Servicio doméstico pago 
    servicio_pago = case_when( 
      t_domestico_pago == 1 ~ "Con servicio pago", 
      t_domestico_pago == 2 ~ "Sin servicio pago", 
      TRUE                  ~ NA_character_ 
    ), 
 
    # Condición de actividad 
    estado_laboral = if_else(condicion_actividad_agrupada == 1, "Ocupada", "No ocupada") 
  ) 
 
# Dimensiones de la muestra (esperado: 6.046 filas) 
dim(enut_clean) 
 
# Frecuencia del nivel educativo 
enut_clean |>

  count(educacion_4cat, name = "casos") |> 
  mutate( 
    pct  = round(100 * casos / sum(casos), 1), 
    acum = cumsum(pct) 
  ) 
 
# Distribución de variables categóricas 
enut_clean |> 
  select(presencia_ninos, ninos_cat, dia_tipo, estado_laboral, servicio_pago) |> 
  pivot_longer(everything(), names_to = "variable", values_to = "categoria") |> 
  count(variable, categoria) 
 
# Verificación de NAs y rango de edad 
colSums(is.na(enut_clean)) 
range(enut_clean$edad_sel) 
 
# Exportación de la base limpia 
write_csv(enut_clean, file.path(input_dir, "enut_limpia.csv")) 
 

dir.create(out_graficos, recursive = TRUE, showWarnings = FALSE)
