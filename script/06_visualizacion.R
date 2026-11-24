# ==============================================================================
# Script 06: Gráfico exploratorio y gráfico comunicativo (instancia 3)
# Trabajo Práctico - Grupo 15 (ENUT 2021)
# Entrada : input/enut_limpia.csv
# Salidas : output/graficos/grupo_15_exploratorio.jpg
#           output/graficos/grupo_15_comunicativo.jpg
# ==============================================================================

options(scipen = 999)
library(tidyverse)
library(scales)

source(file.path("utils", "tema_tp.R"))
source(file.path("utils", "fmt_n.R"))
source(file.path("utils", "fmt_dec.R"))
source(file.path("utils", "suavizar.R"))

dpi_jpg       <- 300
fuente_titulo <- "serif"
fuente_cuerpo <- "sans"

input_dir    <- "input"
out_graficos <- file.path("output", "graficos")
dir.create(out_graficos, recursive = TRUE, showWarnings = FALSE)

ruta_csv <- file.path(input_dir, "enut_limpia.csv")
if (!file.exists(ruta_csv)) {
  stop("No se encuentra '", ruta_csv, "'. Ejecute primero script/01_limpieza_eda.R.")
}

enut_muestra <- read_csv(ruta_csv, show_col_types = FALSE)

# ==============================================================================
# 1. GRÁFICO EXPLORATORIO: cuidado directo vs. trabajo doméstico (mujeres con niños/as)
# ==============================================================================

niveles_corto <- c("Hasta sec. incompleto", "Sec. completo",
                   "Terc./univ. incompleto", "Terc./univ. completo")

con_ninos <- enut_muestra |>
  filter(cant_personashasta13 > 0, !is.na(servicio_pago)) |>
  mutate(
    nivel         = factor(educacion_4cat, levels = niveles_educ, labels = niveles_corto),
    servicio_pago = factor(servicio_pago, levels = c("Sin servicio pago", "Con servicio pago")),
    ninos         = pmin(cant_personashasta13, 2)
  )

med_dom <- median(con_ninos$hs_domestico)
med_cui <- median(con_ninos$hs_cuidado)

etiquetas <- con_ninos |>
  group_by(nivel) |>
  summarise(n = n(), n_serv = sum(servicio_pago == "Con servicio pago"), .groups = "drop") |>
  mutate(panel = paste0(nivel, "\n(n = ", fmt_n(n), "; con servicio: ", n_serv, ")"))

con_ninos <- con_ninos |>
  left_join(etiquetas, by = "nivel") |>
  mutate(panel = factor(panel, levels = etiquetas$panel))

medias <- con_ninos |>
  group_by(panel, servicio_pago) |>
  summarise(dom = mean(hs_domestico), cui = mean(hs_cuidado), .groups = "drop")

n_fuera <- sum(con_ninos$hs_cuidado > 16 | con_ninos$hs_domestico > 12)

set.seed(123)
datos_plot <- con_ninos |>
  filter(hs_cuidado <= 16, hs_domestico <= 12) |>
  mutate(x = pmax(hs_cuidado   + runif(n(), -0.15, 0.15), 0),
         y = pmax(hs_domestico + runif(n(), -0.15, 0.15), 0)) |>
  arrange(servicio_pago)

g_expl <- ggplot() +
  geom_vline(xintercept = med_cui, linetype = "dashed", color = "gray40", linewidth = 0.4) +
  geom_hline(yintercept = med_dom, linetype = "dashed", color = "gray40", linewidth = 0.4) +
  geom_point(data = datos_plot, aes(x = x, y = y, fill = servicio_pago, size = ninos),
             shape = 21, color = col_fondo, stroke = 0.3, alpha = 0.65) +
  geom_point(data = medias, aes(x = cui, y = dom, fill = servicio_pago),
             shape = 23, size = 4, color = col_texto, stroke = 0.9, show.legend = FALSE) +
  facet_wrap(~ panel, nrow = 2) +
  scale_fill_manual(values = c("Sin servicio pago" = col_sin, "Con servicio pago" = col_con)) +
  scale_size_continuous(range = c(1.7, 3.4), breaks = 1:2, labels = c("1", "2 o más")) +
  scale_x_continuous(breaks = seq(0, 16, 4), labels = label_number(decimal.mark = ","),
                     expand = expansion(mult = 0.03)) +
  scale_y_continuous(breaks = seq(0, 12, 3), labels = label_number(decimal.mark = ","),
                     expand = expansion(mult = 0.03)) +
  coord_cartesian(xlim = c(0, 16), ylim = c(0, 12)) +
  guides(
    fill = guide_legend(order = 1, override.aes = list(shape = 21, size = 3.5, alpha = 1, color = col_fondo)),
    size = guide_legend(order = 2, override.aes = list(shape = 21, fill = "gray50", color = col_fondo, alpha = 1))
  ) +
  labs(
    title = str_wrap(paste0("Horas diarias de trabajo doméstico y de cuidado directo de mujeres ",
                            "con niños/as, según nivel educativo y servicio doméstico pago."), width = 66),
    subtitle = paste0(
      "Cada punto es una mujer de 18 a 64 años con niños/as de 0 a 13 años en el hogar (ENUT 2021). ",
      "Rombos: media de cada grupo.\n",
      "Líneas punteadas: mediana de todas las mujeres con niños/as (trabajo doméstico: ",
      fmt_dec(med_dom), " h; cuidado directo: ", fmt_dec(med_cui), " h)."),
    x = "Horas diarias de cuidado directo", y = "Horas diarias de trabajo doméstico",
    fill = NULL, size = "Niños/as de 0 a 13 años en el hogar:",
    caption = paste0(
      fuente_enut, " Muestra sin ponderar. Puntos con leve desplazamiento aleatorio.\n",
      "Se omiten ", n_fuera, " casos con más de 16 h de cuidado o más de 12 h de trabajo doméstico (eje recortado).\n",
      "Un 0 indica que no se realizó la actividad el día anterior.")
  ) +
  theme_tp(base_size = 11, base_family = fuente_cuerpo) +
  theme(
    plot.title    = element_text(family = fuente_titulo, face = "plain", size = 18, color = col_texto,
                                 lineheight = 1.05, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 10, color = "grey25", lineheight = 1.15, margin = margin(b = 10)),
    plot.caption  = element_text(size = 8, color = "grey35", hjust = 0, lineheight = 1.15, margin = margin(t = 12)),
    plot.margin   = margin(14, 16, 12, 16),
    panel.spacing = unit(1, "lines")
  )

ggsave(file.path(out_graficos, "grupo_15_exploratorio.jpg"), plot = g_expl,
       width = 25, height = 21, units = "cm", dpi = dpi_jpg)

# ==============================================================================
# 2. GRÁFICO COMUNICATIVO: trabajo no remunerado según edad y niños/as en el hogar
# ==============================================================================

mostrar_puntos <- TRUE
niveles_ninos  <- c("Sin niños/as", "1 niño/a", "2 o más niños/as")

base_com <- enut_muestra |>
  filter(!is.na(servicio_pago)) |>
  mutate(
    hs_total = hs_domestico + hs_cuidado,
    ninos = case_when(
      cant_personashasta13 == 0 ~ niveles_ninos[1],
      cant_personashasta13 == 1 ~ niveles_ninos[2],
      cant_personashasta13 >= 2 ~ niveles_ninos[3]
    ),
    ninos = factor(ninos, levels = niveles_ninos)
  )

curvas <- base_com |>
  group_by(ninos) |>
  group_modify(~ suavizar(.x)) |>
  ungroup()

# Etiquetas al final de cada curva (se separan si quedan muy juntas)
fin <- curvas |>
  group_by(ninos) |>
  slice_max(edad_sel, n = 1) |>
  ungroup() |>
  mutate(etiqueta = as.character(ninos)) |>
  arrange(fit)

sep_min <- 1.3
fin$y_lab <- fin$fit
for (i in seq_len(nrow(fin))[-1]) fin$y_lab[i] <- max(fin$fit[i], fin$y_lab[i - 1] + sep_min)

# Pico de la curva "2 o más" y brecha con "sin niños/as" a esa edad
pico        <- curvas |> filter(ninos == niveles_ninos[3]) |> slice_max(fit, n = 1)
f0_pico     <- curvas |> filter(ninos == niveles_ninos[1], edad_sel == pico$edad_sel) |> pull(fit)
brecha_pico <- pico$fit - f0_pico

# Brecha mínima entre ambas curvas (respalda "a cualquier edad" del título)
curvas_c3 <- curvas |> filter(ninos == niveles_ninos[3])
curvas_c1 <- curvas |> filter(ninos == niveles_ninos[1])
edades_comunes <- intersect(curvas_c3$edad_sel, curvas_c1$edad_sel)

brecha_min <- min(
  (curvas_c3 |> filter(edad_sel %in% edades_comunes) |> arrange(edad_sel) |> pull(fit)) -
  (curvas_c1 |> filter(edad_sel %in% edades_comunes) |> arrange(edad_sel) |> pull(fit))
)
cat("Brecha mínima entre '2 o más niños/as' y 'sin niños/as':", round(brecha_min, 2), "h\n")

colores  <- c("Sin niños/as" = "#9A9290", "1 niño/a" = "#6F6F6F", "2 o más niños/as" = col_con)
grosores <- c("Sin niños/as" = 0.9, "1 niño/a" = 0.9, "2 o más niños/as" = 1.5)

breaks_x <- seq(20, 60, by = 10)
breaks_x <- sort(c(breaks_x[abs(breaks_x - pico$edad_sel) >= 4], pico$edad_sel))
y_max <- 13

g_com <- ggplot()
if (mostrar_puntos) {
  g_com <- g_com + geom_point(data = filter(base_com, hs_total <= y_max),
                              aes(x = edad_sel, y = hs_total, color = ninos),
                              position = position_jitter(width = 0.4, height = 0, seed = 1),
                              size = 0.5, alpha = 0.07, stroke = 0)
}

g_com <- g_com +
  geom_line(data = curvas, aes(x = edad_sel, y = fit, color = ninos, linewidth = ninos), lineend = "round") +
  scale_color_manual(values = colores, guide = "none") +
  scale_linewidth_manual(values = grosores, guide = "none") +
  geom_text(data = fin, aes(x = edad_sel + 0.8, y = y_lab, label = etiqueta, color = ninos),
            hjust = 0, vjust = 0.5, fontface = "bold", size = 3.9, family = fuente_cuerpo) +
  annotate("segment", x = pico$edad_sel, xend = pico$edad_sel, y = 0, yend = f0_pico - 0.3,
           linetype = "dotted", linewidth = 0.5, color = col_con, alpha = 0.6) +
  annotate("segment", x = pico$edad_sel, xend = pico$edad_sel,
           y = f0_pico + 0.4, yend = pico$fit - 0.4,
           arrow = arrow(length = unit(0.14, "cm"), ends = "both"), linewidth = 0.45, color = col_texto) +
  annotate("point", x = pico$edad_sel, y = pico$fit, shape = 21, size = 3, stroke = 0.9,
           fill = col_con, color = col_fondo) +
  annotate("text", x = pico$edad_sel, y = pico$fit + 0.4, hjust = 0.5, vjust = 0,
           label = fmt_dec(pico$fit), color = col_con, size = 4.6, fontface = "bold", family = fuente_cuerpo) +
  annotate("point", x = pico$edad_sel, y = f0_pico, shape = 21, size = 3, stroke = 0.9,
           fill = colores[["Sin niños/as"]], color = col_fondo) +
  annotate("text", x = pico$edad_sel + 0.9, y = f0_pico - 0.2, hjust = 0, vjust = 1,
           label = fmt_dec(f0_pico), color = "#7D7573", size = 4.6, fontface = "bold", family = fuente_cuerpo) +
  annotate("text", x = pico$edad_sel + 0.8, y = f0_pico + 0.42 * (pico$fit - f0_pico),
           hjust = 0, vjust = 0.5, label = paste0(fmt_dec(brecha_pico), " h por día\nde diferencia"),
           color = col_texto, size = 3.7, fontface = "bold", lineheight = 0.95, family = fuente_cuerpo) +
  scale_x_continuous(breaks = breaks_x) +
  scale_y_continuous(breaks = seq(0, 12, by = 4)) +
  coord_cartesian(xlim = c(18, 79), ylim = c(0, y_max + 0.2), expand = FALSE) +
  labs(x = "Edad de la mujer (años)",
       y = "Horas por día de trabajo no remunerado\n(doméstico + cuidado directo)") +
  theme_tp(base_size = 11, base_family = fuente_cuerpo) +
  theme(panel.grid.major.x = element_blank(), plot.margin = margin(14, 16, 12, 16))

titulo <- str_wrap(paste0(
  "Las mujeres con 2 o más niños/as en el hogar dedican al menos ", floor(brecha_min),
  " horas diarias adicionales al trabajo no remunerado, a cualquier edad."), width = 62)

bajada_1 <- str_wrap(paste0(
  "Horas diarias de trabajo doméstico y de cuidado directo según la edad, ",
  "por cantidad de niños/as de 0 a 13 años en el hogar."), width = 115)
bajada_2 <- str_wrap(paste0(
  "A los ", pico$edad_sel, " años, las mujeres con 2 o más niños/as dedican ",
  fmt_dec(brecha_pico), " horas más por día que las que no viven con niños/as (",
  fmt_dec(pico$fit), " h contra ", fmt_dec(f0_pico), " h). ",
  "Mujeres de 18 a 64 años, de aglomerados urbanos de Argentina, 2021."), width = 115)
bajada <- paste0(bajada_1, "\n", bajada_2)

nota <- str_wrap(paste0(
  fuente_enut, " Muestra sin ponderar. Cada grupo se muestra en las edades que concentran su 95 % central. ",
  "Eje vertical cortado en ", y_max, " h."), width = 140)

grafico_comunicativo <- g_com +
  labs(title = titulo, subtitle = bajada, caption = nota) +
  theme(
    plot.title    = element_text(family = fuente_titulo, face = "plain", size = 18.5, color = col_con,
                                 lineheight = 1.05, margin = margin(b = 6)),
    plot.subtitle = element_text(family = fuente_cuerpo, size = 10, color = "grey25",
                                 lineheight = 1.15, margin = margin(b = 12)),
    plot.caption  = element_text(family = fuente_cuerpo, size = 8, color = "grey35",
                                 hjust = 0, lineheight = 1.15, margin = margin(t = 12))
  )

ggsave(file.path(out_graficos, "grupo_15_comunicativo.jpg"), plot = grafico_comunicativo,
       width = 24, height = 15.5, units = "cm", dpi = dpi_jpg, quality = 95)

cat("Script 06 completado: gráficos exploratorio y comunicativo exportados.\n")
