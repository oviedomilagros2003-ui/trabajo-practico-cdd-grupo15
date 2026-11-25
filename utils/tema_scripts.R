# ============================================================================== 
# Paleta de Colores y Tema Visual del Trabajo Práctico Scripts 02 al 05
# ============================================================================== 
 
library(ggplot2) 
 
col_con    <- "#960D3C" 
col_sin    <- "#9A9290" 
col_fondo  <- "#FBF6F4" 
col_grid   <- "#E7DEDB" 
col_texto  <- "#2E2E2E" 
col_hist   <- "#B0245A"

# Definición explícita de niveles educativos (había quedado en otro archivo de utils al separar temas)
  "Hasta Sec. Incompleto",
  "Secundario Completo",
  "Terciario/Univ. Incompleto",
  "Terciario/Univ. Completo"
)
fuente_enut <- "Fuente: elaboracion propia en base a la Encuesta Nacional de Uso del Tiempo 2021 (INDEC)"

cols_educ  <- c("#7B1650", "#C0566F", "#E8A1C4", "#D81B60") 
cols_dias  <- c("#9B1048", "#E0245E")
 
theme_tp <- function(base_size = 12, base_family = "sans") { 
  theme_minimal(base_size = base_size, base_family = base_family) + 
    theme( 
      plot.background       = element_rect(fill = col_fondo, color = NA), 
      panel.background      = element_rect(fill = col_fondo, color = NA), 
      plot.title.position   = "plot", 
      plot.caption.position = "plot",
      plot.title            = element_text(size = 16, face = "bold", color = col_texto, margin = margin(b = 6)), 
      plot.subtitle         = element_text(size = 11, color = "grey25", margin = margin(b = 10)), 
      plot.caption          = element_text(size = 8.5, color = "grey35", hjust = 0, margin = margin(t = 10)), 
      panel.grid.minor      = element_blank(), 
      panel.grid.major      = element_line(color = col_grid, linewidth = 0.4), 
      axis.title            = element_text(size = 10.5, color = col_texto), 
      axis.text             = element_text(size = 9.5, color = "grey30"), 
      strip.text            = element_text(face = "bold", size = 10.5, color = col_texto), 
      legend.position       = "top", 
      legend.justification  = "left" 
    ) 
}

  
