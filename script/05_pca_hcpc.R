# ==============================================================================
# Script 05: Análisis de componentes principales (PCA) y clustering (HCPC)
# Trabajo Práctico - Grupo 15 (ENUT 2021)
# Entrada : input/enut_limpia.csv
# Salidas : tablas en output/tablas/, gráficos en output/graficos/,
#           input/enut_con_clusters.csv
# PCA: 5 actividades activas; edad y niños (numéricas) y educación y servicio pago
#      (categóricas) suplementarias. 

options(scipen = 999)
set.seed(2026)

library(tidyverse)
library(FactoMineR)    # PCA y HCPC
library(factoextra)    # gráficos del PCA y de los clusters

source(file.path("utils", "tema_tp.R"))
source(file.path("utils", "guardar_grafico.R"))
source(file.path("utils", "declarar_tipos.R"))
source(file.path("utils", "limite_iqr.R"))

input_dir  <- "input"
out_tablas <- file.path("output", "tablas")
dir.create(out_tablas, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("output", "graficos"), recursive = TRUE, showWarnings = FALSE)   # CAMBIO: ggsave no crea carpetas

ruta_base <- file.path(input_dir, "enut_limpia.csv")
if (!file.exists(ruta_base)) {
  stop("No se encuentra '", ruta_base, "'. Ejecute primero script/01_limpieza_eda.R.")
}

enut_serv <- read_csv(ruta_base, show_col_types = FALSE) |>
  declarar_tipos() |>
  filter(!is.na(servicio_pago))                                   # 6.042

enut_serv_sin <- enut_serv |> filter(hs_domestico <= limite_iqr(enut_serv$hs_domestico))

# ------------------------------------------------------------------------------
# 1. Variables activas y suplementarias
# ------------------------------------------------------------------------------

vars_act     <- c("hs_remunerado", "hs_domestico", "hs_cuidado", "hs_convivencia", "hs_medios")
vars_sup_num <- c("edad_sel", "cant_personashasta13")
vars_sup_cat <- c("educacion_4cat", "servicio_pago")

df_pca <- enut_serv |>
  select(all_of(c(vars_act, vars_sup_num, vars_sup_cat))) |>
  as.data.frame()

stopifnot(!anyNA(df_pca))

# Correlaciones entre actividades (el PCA resume variables relacionadas)
cor_act <- round(cor(df_pca[, vars_act]), 2)
write_csv(as.data.frame(cor_act) |> rownames_to_column("variable"),
          file.path(out_tablas, "pca_correlaciones_actividades.csv"))

# % de ceros de cada actividad (un 0 es "no se realizó ese día")
ceros_act <- tibble(variable = vars_act,
                    pct_ceros = round(100 * sapply(df_pca[, vars_act], function(x) mean(x == 0)), 1))
write_csv(ceros_act, file.path(out_tablas, "pca_ceros_actividades.csv"))
ceros_act

# ------------------------------------------------------------------------------
# 2. Sensibilidad: sacar una actividad por vez, y prueba de incluir wper
# ------------------------------------------------------------------------------

leave_one_out <- bind_rows(lapply(vars_act, function(v) {
  p <- prcomp(df_pca[, setdiff(vars_act, v)], center = TRUE, scale. = TRUE)
  imp <- summary(p)$importance
  tibble(actividad_omitida = v,
         pct_pc1 = round(100 * imp[2, 1], 1),
         pct_pc2 = round(100 * imp[2, 2], 1),
         pct_pc1_pc2 = round(100 * imp[3, 2], 1))
}))
write_csv(leave_one_out, file.path(out_tablas, "pca_leave_one_out.csv"))
leave_one_out

# wper no mide nada de las mujeres: al incluirla aparece un componente que solo la capta
pca_wper <- prcomp(cbind(df_pca[, vars_act], wper = enut_serv$wper), center = TRUE, scale. = TRUE)
write_csv(as.data.frame(round(pca_wper$rotation, 2)) |> rownames_to_column("variable"),
          file.path(out_tablas, "pca_prueba_con_wper.csv"))

# ------------------------------------------------------------------------------
# 3. PCA con FactoMineR
# ------------------------------------------------------------------------------

n_act <- length(vars_act)
n_num <- length(vars_sup_num)

res_pca <- FactoMineR::PCA(
  df_pca,
  quanti.sup = n_act + seq_len(n_num),
  quali.sup  = n_act + n_num + seq_along(vars_sup_cat),
  scale.unit = TRUE,
  graph      = FALSE
)

# Se invierte la Dim 1 para que crezca con el
# trabajo remunerado. Se aplica en todas las tablas y gráficos.
res_pca$var$coord[, "Dim.1"]        <- -res_pca$var$coord[, "Dim.1"]
res_pca$var$cor[, "Dim.1"]          <- -res_pca$var$cor[, "Dim.1"]
res_pca$ind$coord[, "Dim.1"]        <- -res_pca$ind$coord[, "Dim.1"]
res_pca$quanti.sup$coord[, "Dim.1"] <- -res_pca$quanti.sup$coord[, "Dim.1"]
res_pca$quanti.sup$cor[, "Dim.1"]   <- -res_pca$quanti.sup$cor[, "Dim.1"]
res_pca$quali.sup$coord[, "Dim.1"]  <- -res_pca$quali.sup$coord[, "Dim.1"]
res_pca$quali.sup$v.test[, "Dim.1"] <- -res_pca$quali.sup$v.test[, "Dim.1"]

# 3.1. Autovalores y varianza explicada
autovalores <- as.data.frame(res_pca$eig) |>
  rownames_to_column("componente") |>
  rename(autovalor = eigenvalue, pct_varianza = `percentage of variance`,
         pct_acumulado = `cumulative percentage of variance`) |>
  mutate(across(where(is.numeric), ~ round(.x, 2)))
write_csv(autovalores, file.path(out_tablas, "pca_autovalores.csv"))
autovalores   # Kaiser (autovalor > 1) sugiere 3; se eligen 2 por interpretabilidad (ver README)

# 3.2. Correlación y contribución de cada actividad con los ejes 1 y 2
cargas <- tibble(
  variable   = rownames(res_pca$var$coord),
  cor_dim1   = round(res_pca$var$coord[, 1], 2),
  cor_dim2   = round(res_pca$var$coord[, 2], 2),
  contrib_d1 = round(res_pca$var$contrib[, 1], 1),
  contrib_d2 = round(res_pca$var$contrib[, 2], 1)
)
write_csv(cargas, file.path(out_tablas, "pca_cargas_actividades.csv"))
cargas

# 3.3. Variables suplementarias
sup_num <- tibble(
  variable = rownames(res_pca$quanti.sup$coord),
  cor_dim1 = round(res_pca$quanti.sup$coord[, 1], 2),
  cor_dim2 = round(res_pca$quanti.sup$coord[, 2], 2)
)
write_csv(sup_num, file.path(out_tablas, "pca_suplementarias_numericas.csv"))

# 3.4. Puntaje medio de cada grupo (educación y servicio pago) y v.test
puntajes_grupos <- tibble(
  grupo   = rownames(res_pca$quali.sup$coord),
  n       = c(as.integer(table(df_pca$educacion_4cat)), as.integer(table(df_pca$servicio_pago))),
  dim1    = round(res_pca$quali.sup$coord[, 1], 2),
  dim2    = round(res_pca$quali.sup$coord[, 2], 2),
  vtest_1 = round(res_pca$quali.sup$v.test[, 1], 1),
  vtest_2 = round(res_pca$quali.sup$v.test[, 2], 1)
)
write_csv(puntajes_grupos, file.path(out_tablas, "pca_puntajes_grupos.csv"))
puntajes_grupos

# Horas medias por grupo (respaldan la lectura del eje 1)
horas_por_grupo <- bind_rows(
  enut_serv |> mutate(grupo = as.character(educacion_4cat)),
  enut_serv |> mutate(grupo = as.character(servicio_pago))
) |>
  group_by(grupo) |>
  summarise(n = n(), edad_media = round(mean(edad_sel), 1),
            hs_remunerado = round(mean(hs_remunerado), 1),
            hs_domestico = round(mean(hs_domestico), 1), .groups = "drop")
write_csv(horas_por_grupo, file.path(out_tablas, "pca_horas_por_grupo.csv"))

# 3.5. Gráficos del PCA 
g_scree <- factoextra::fviz_screeplot(res_pca, addlabels = TRUE, barfill = col_hist, barcolor = col_hist) +
  labs(title = "Varianza explicada por componente",
       subtitle = "PCA de 5 actividades de uso del tiempo (mujeres de 18 a 64 años).",
       x = "Componente principal", y = "Porcentaje de la varianza",
       caption = fuente_enut) +
  theme_tp()
guardar_grafico(g_scree, "pca_screeplot.png", ancho = 18, alto = 12)

g_circulo <- factoextra::fviz_pca_var(res_pca, repel = TRUE, col.var = col_con, col.quanti.sup = "#4575b4") +
  labs(title = "Círculo de correlaciones: actividades de uso del tiempo",
       subtitle = "Rojo: actividades activas. Azul: edad y niños, suplementarias. Eje 1 invertido: crece con el trabajo remunerado.",
       caption = fuente_enut) +
  theme_tp()
guardar_grafico(g_circulo, "pca_circulo_correlacion.png", ancho = 18, alto = 16)

# Mapa de puntajes medios por grupo (educación y servicio pago) en el plano PC1-PC2
g_grupos <- ggplot(puntajes_grupos, aes(x = dim1, y = dim2, label = grupo)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(color = col_con, size = 3) +
  geom_text(vjust = -0.9, size = 3.6, color = col_texto) +
  labs(title = "Puntaje medio de cada grupo en los dos primeros componentes",
       subtitle = "Eje 1: trabajo remunerado contra no remunerado. Eje 2: convivencia contra trabajo.",
       x = "Componente 1", y = "Componente 2", caption = fuente_enut) +
  theme_tp()
guardar_grafico(g_grupos, "pca_puntajes_grupos.png", ancho = 20, alto = 14)

# ------------------------------------------------------------------------------
# 4. Clustering jerárquico sobre componentes principales (HCPC)
# ------------------------------------------------------------------------------

# 4.1. Ganancia de inercia y justificacion de k
hcpc_auto <- FactoMineR::HCPC(res_pca, nb.clust = -1, consol = FALSE, graph = FALSE)

ganancia <- tibble(
  n_grupos = 2:11,
  ganancia_inercia = round(hcpc_auto$call$t$inert.gain[1:10], 3)
)
write_csv(ganancia, file.path(out_tablas, "hcpc_ganancia_inercia.csv"))
cat("Grupos que propone el corte automático:", length(unique(hcpc_auto$data.clust$clust)), "\n")

g_inercia <- ggplot(ganancia, aes(x = n_grupos, y = ganancia_inercia)) +
  geom_line(color = col_con) +
  geom_point(color = col_con, size = 2) +
  scale_x_continuous(breaks = 2:11) +
  labs(title = "Ganancia de inercia al pasar a más grupos",
       subtitle = "HCPC (Ward) sobre los componentes del PCA. Se busca dónde la curva se aplana.",
       x = "Cantidad de grupos", y = "Ganancia de inercia", caption = fuente_enut) +
  theme_tp()
guardar_grafico(g_inercia, "hcpc_ganancia_inercia.png", ancho = 18, alto = 12)

# 4.2. Clustering final con k elegido.
# La ganancia de inercia no apoya k = 4 con claridad (se aplana recién después del 5.º grupo).
# Se elige k = 4 por interpretabilidad
k_clusters <- 4
res_hcpc <- FactoMineR::HCPC(res_pca, nb.clust = k_clusters, consol = TRUE, graph = FALSE)

enut_serv <- enut_serv |> mutate(cluster_hcpc = res_hcpc$data.clust$clust)

perfil_clusters <- enut_serv |>
  group_by(cluster_hcpc) |>
  summarise(
    n = n(),
    across(c(hs_remunerado, hs_domestico, hs_cuidado, hs_convivencia, hs_medios,
             edad_sel, cant_personashasta13), ~ round(mean(.x), 2)),
    .groups = "drop"
  )
write_csv(perfil_clusters, file.path(out_tablas, "perfil_clusters_hcpc.csv"))
perfil_clusters

# Composición por educación y servicio pago 
composicion_clusters <- enut_serv |>
  group_by(cluster_hcpc) |>
  summarise(
    n = n(),
    pct_univ_completa = round(100 * mean(educacion_4cat == "4. Terciario/Univ. Completo"), 1),
    pct_hasta_sec_inc = round(100 * mean(educacion_4cat == "1. Hasta Sec. Incompleto"), 1),
    pct_servicio_pago = round(100 * mean(servicio_pago == "Con servicio pago"), 1),
    .groups = "drop"
  )
write_csv(composicion_clusters, file.path(out_tablas, "composicion_clusters_hcpc.csv"))
composicion_clusters

g_cluster <- factoextra::fviz_cluster(res_hcpc, geom = "point", show.clust.cent = TRUE,
                                      ggtheme = theme_tp()) +
  labs(title = "Grupos de uso del tiempo (HCPC) en el plano de los dos primeros componentes",
       subtitle = "Mujeres de 18 a 64 años. Cada punto es una mujer.",
       caption = fuente_enut)
guardar_grafico(g_cluster, "hcpc_clusters.png", ancho = 20, alto = 15)

# 4.3. Perfiles con k alternativos (anexo), para respaldar la elección de k
perfiles_k <- bind_rows(lapply(3:5, function(k) {
  h <- FactoMineR::HCPC(res_pca, nb.clust = k, consol = TRUE, graph = FALSE)
  enut_serv |>
    mutate(grupo = h$data.clust$clust) |>
    group_by(grupo) |>
    summarise(n = n(),
              across(c(hs_remunerado, hs_domestico, hs_cuidado, hs_convivencia, hs_medios),
                     ~ round(mean(.x), 2)),
              .groups = "drop") |>
    mutate(k = k, .before = 1)
}))
write_csv(perfiles_k, file.path(out_tablas, "hcpc_perfiles_k_alternativos.csv"))

# Caracterización de cada grupo por v-test (|v| > 2: la variable lo distingue del promedio)
vtest_clusters <- bind_rows(
  lapply(Filter(Negate(is.null), res_hcpc$desc.var$quanti),
         function(m) as.data.frame(m) |> rownames_to_column("variable")),
  .id = "cluster")
write_csv(vtest_clusters, file.path(out_tablas, "hcpc_vtest_variables.csv"))

write_csv(enut_serv, file.path(input_dir, "enut_con_clusters.csv"))

# ------------------------------------------------------------------------------
# 5. Robustez del PCA: resultados con y sin los casos atípicos de trabajo doméstico
# ------------------------------------------------------------------------------

pca_act     <- prcomp(select(enut_serv,     all_of(vars_act)), center = TRUE, scale. = TRUE)
pca_act_sin <- prcomp(select(enut_serv_sin, all_of(vars_act)), center = TRUE, scale. = TRUE)

robustez <- tibble(
  resultado = c("Media trabajo doméstico (hs)",
                "PCA (5 actividades): % varianza PC1",
                "PCA (5 actividades): % varianza PC2"),
  con_atipicos = c(mean(enut_serv$hs_domestico),
                   100 * summary(pca_act)$importance[2, 1],
                   100 * summary(pca_act)$importance[2, 2]),
  sin_atipicos = c(mean(enut_serv_sin$hs_domestico),
                   100 * summary(pca_act_sin)$importance[2, 1],
                   100 * summary(pca_act_sin)$importance[2, 2])
) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

write_csv(robustez, file.path(out_tablas, "robustez_con_sin_outliers.csv"))
robustez

cat("Script 05 completado: PCA y HCPC exportados.\n")
