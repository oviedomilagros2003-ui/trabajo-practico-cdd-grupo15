# Trabajo Práctico - Ciencia de Datos  | Análisis del uso del tiempo y trabajo no remunerado a partir de la ENUT 2021.

## Integrantes

- Bartoli, Pilar (registro: 917779)
- Oviedo, Milagros (registro: 916005)
- Rosado, Florencia (registro: 918304)

## Objetivo e hipótesis de trabajo

El trabajo describe cómo se relacionan el nivel educativo, la contratación de servicio doméstico pago y la carga de trabajo no remunerado (doméstico y de cuidado) de las mujeres de 18 a 64 años.

**Hipótesis:** el nivel educativo de las mujeres de 18 a 64 años se asocia con una mayor tercerización del trabajo doméstico (medida como presencia de servicio doméstico pago en el hogar), y quienes tercerizan tienen menor carga de trabajo doméstico no remunerado.

La hipótesis se ajustó respecto de la primera entrega: la tercerización se mide solo como servicio doméstico pago.

## Base de datos

- **Nombre:** Encuesta Nacional de Uso del Tiempo (ENUT) 2021.
- **Fuente:** INDEC. Enlace: https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos-8
- **Archivo:** `raw/enut2021_base.txt` (texto separado por `|`).
- **Período:** relevamiento 2021.
- **Unidad de observación:** persona de 14 años o más, una por hogar (selección con tabla de Kish).
- **Tamaño:** 14.350 filas × 327 columnas. Se trabaja con 13 variables.
- **Muestra de análisis:** mujeres de 18 a 64 años.

| Paso | Casos |
|---|---|
| Base completa | 14.350 |
| Mujeres | 8.055 |
| Mujeres de 18 a 64 años | 6.077 |
| Con nivel educativo válido | 6.046 |
| Con dato de servicio pago (chi-cuadrado, regresión, PCA) | 6.042 |

**Limitaciones:** el diario cubre un solo día por persona; la base no trae ingreso del hogar; los tests, la regresión y el PCA no usan el ponderador `wper` ni el diseño muestral (solo las descriptivas lo usan). Un 0 en una actividad significa que no se realizó ese día, no un dato faltante.

**Base auxiliar:** `auxiliar/codigos_nivel_educativo.csv`, que traduce el código de nivel educativo a su categoría (se une con `left_join`, sin pérdida de observaciones).

## Descripción del análisis

1. **Limpieza:** se seleccionan 13 variables, se filtra la población (mujeres de 18 a 64 años), se recodifican las variables y se pasan los minutos diarios a horas.
2. **Exploración:** descriptivas, tablas ponderadas con `wper`, cruces por nivel educativo, tipo de día, ocupación y niños, percentiles y correlaciones.
3. **Faltantes:** no hay NA explícitos, los faltantes vienen como códigos (99 y educación especial). Se excluyen 35 casos (0,58 %) y se documenta su perfil.
4. **Outliers:** regla del rango intercuartílico sobre trabajo doméstico (umbral 11,08 h por día). Son 73 casos (1,21 %), se consideran valores genuinos y se mantienen, repitiendo cada método sin ellos.
5. **Inferencia:** ANOVA, Welch, Games-Howell y Kruskal-Wallis (trabajo doméstico según educación, con y sin atípicos); chi-cuadrado (educación y servicio pago); regresión MCO con errores robustos HC1 (trabajo doméstico y cuidado directo, este último en tres versiones por su 61 % de ceros).
6. **PCA y clustering:** PCA de cinco actividades de uso del tiempo (edad, niños, educación y servicio pago como variables suplementarias). El criterio de Kaiser sugiere 3 componentes y se eligen 2 porque se interpretan mejor. Luego, clustering jerárquico sobre componentes principales (HCPC) con 4 grupos: la ganancia de inercia no marca un número claro, por eso se elige por interpretabilidad y se guardan los perfiles con 3 y 5 grupos.
7. **Gráficos:** gráficos de cada etapa, más un gráfico exploratorio y uno comunicativo.

## Estructura del repositorio

```
trabajo-practico-cdd-grupo15/
├── auxiliar/   Tabla de códigos de nivel educativo
├── input/      Bases procesadas generadas por los scripts (CSV)
├── output/
│   ├── graficos/  Gráficos (PNG y JPG)
│   └── tablas/    Tablas de resultados (CSV)
├── raw/        Datos originales de la ENUT 2021 (no se modifican)
├── script/     Código R, numerado según el orden de ejecución
├── utils/      Funciones y estilos propios (un script por función)
└── README.md
```

## Instrucciones de reproducción

### Paquetes necesarios

`tidyverse`, `janitor`, `skimr`, `car`, `rstatix`, `lmtest`, `sandwich`, `modelsummary`, `broom`, `FactoMineR`, `factoextra`, `scales`. Opcionales, para la tipografía de los gráficos: `showtext` y `sysfonts` (si no están, se usan las fuentes del sistema).

### Cómo correr el proyecto en Google Colab (entorno R)

Todos los pasos se hacen en celdas de Colab; los scripts no instalan paquetes ni fijan carpetas de trabajo.

1. **Celda 1.** Instalar los paquetes (una sola vez por sesión; `factoextra` tarda varios minutos):

   ```r
   install.packages(c("tidyverse", "janitor", "skimr", "car", "rstatix", "lmtest", "sandwich",
                      "modelsummary", "broom", "FactoMineR", "factoextra", "scales",
                      "showtext", "sysfonts"))
   ```

2. **Celda 2.** Descargar el repositorio y fijar su carpeta como directorio de trabajo:

   ```r
   system("git clone https://github.com/oviedomilagros2003-ui/trabajo-practico-cdd-grupo15.git")
   setwd("/content/trabajo-practico-cdd-grupo15")
   getwd()
   ```

3. **Celdas siguientes.** Ejecutar los scripts en orden numérico (la carpeta `output/` se crea sola si no existe):

   ```r
   source("script/01_limpieza_eda.R")
   source("script/02_exploratorio.R")
   source("script/03_faltantes_outliers.R")
   source("script/04_inferencia.R")
   source("script/05_pca_hcpc.R")
   source("script/06_visualizacion.R")
   ```

Cada script lee sus entradas desde archivos guardados, así que puede correrse por separado en una sesión nueva. Se usa `set.seed(2026)` para que los resultados sean reproducibles. Los archivos intermedios y de resultados son CSV (UTF-8, separador coma).

### Qué hace cada script

| Script | Qué hace | Lee | Genera |
|---|---|---|---|
| `01_limpieza_eda.R` | Carga la base cruda, revisa su estructura, filtra mujeres de 18 a 64 años, une la tabla de niveles educativos, recodifica variables y arma los diccionarios | `raw/enut2021_base.txt`, `auxiliar/codigos_nivel_educativo.csv` | `input/enut_mujeres_18_64.csv` (6.077), `input/enut_limpia.csv` (6.046); `output/tablas/`: `diccionario_variables*.csv`, `analisis_faltantes_inicial.csv`, `codigos_*.csv`, `tipos_variables.csv`, `resumen_filtrado_muestra.csv` |
| `02_eda.R` | Análisis exploratorio: descriptivas, frecuencias, tablas ponderadas con `wper`, cruces por educación, tipo de día, ocupación y niños, percentiles, correlaciones y comparación ponderado contra sin ponderar | `input/enut_limpia.csv` | `output/tablas/`: `descriptivas_*.csv`, `tabla_*.csv`, `perfil_por_nivel_educativo.csv`, `ratios_percentiles.csv`, `matriz_correlaciones_*.csv`, `comparacion_ponderado_vs_simple.csv`, entre otras; `output/graficos/`: `histograma_trabajo_domestico.png`, `boxplot_domestico_educacion.png`, `barras_cuidado_ocupadas_ninos.png`, `barras_domestico_tipo_dia.png`, `matriz_correlaciones.png` |
| `03_faltantes_outliers.R` | Diagnóstico de faltantes (perfil de los excluidos y efecto de excluirlos) y de outliers (regla del IQR, perfil de los atípicos, descriptivas con y sin ellos) | `input/enut_mujeres_18_64.csv`, `input/enut_limpia.csv`, `output/tablas/codigos_no_respuesta.csv` | `output/tablas/`: `perfil_casos_excluidos.csv`, `impacto_exclusion_estadisticas.csv`, `criterio_outliers_domestico.csv`, `perfil_outliers.csv`, `descriptivas_con_sin_outliers.csv`, `limites_iqr_actividades.csv`; `output/graficos/`: `casos_excluidos.png`, `histograma_atipicos_domestico.png` |
| `04_inferencia.R` | ANOVA, Welch, Games-Howell, Kruskal-Wallis (con y sin atípicos), chi-cuadrado, regresión de trabajo doméstico y de cuidado | `input/enut_limpia.csv` | `output/tablas/`: `anova_*.csv`, `post_hoc_*.csv`, `composicion_educacion.csv`, `chi2_*.csv`, `regresion_*.csv`, `brecha_educacion_sin_y_con_controles.csv`; `output/graficos/`: `qqplot_residuos_anova.png`, `barras_tercerizacion_educacion.png`, `modelplot_coeficientes.png` |
| `05_pca_clustering.R` | PCA, pruebas de sensibilidad, HCPC con justificación del número de grupos y robustez con y sin atípicos | `input/enut_limpia.csv` | `input/enut_con_clusters.csv`; `output/tablas/`: `pca_*.csv`, `hcpc_*.csv`, `perfil_clusters_hcpc.csv`, `composicion_clusters_hcpc.csv`, `robustez_con_sin_outliers.csv`; `output/graficos/`: `pca_*.png`, `hcpc_*.png` |
| `06_graficos.R` | Gráfico exploratorio y gráfico comunicativo | `input/enut_limpia.csv` | `output/graficos/grupo_15_exploratorio.jpg`, `output/graficos/grupo_15_comunicativo.jpg` |

### Funciones y estilos de `utils/`

Se cargan con `source()` desde los scripts.

| Archivo | Para qué sirve |
|---|---|
| `tema_scripts.R` | Colores, etiquetas y tema de los gráficos de los scripts 02 a 05 |
| `tema_tp.R` | Colores, tipografía y tema de los gráficos del script 06 |
| `guardar_grafico.R` | Guarda un gráfico en `output/graficos/` |
| `declarar_tipos.R` | Vuelve a declarar los tipos de variable al leer los CSV |
| `limite_iqr.R` | Calcula el umbral de atípicos (regla del rango intercuartílico) |
| `cuantil_pond.R` | Cuantiles ponderados con `wper` |
| `resumir_dom.R` | Resumen del trabajo doméstico para comparar con y sin atípicos |
| `fmt_n.R`, `fmt_dec.R` | Formato de números con separador de miles y coma decimal |
| `suavizar.R` | Curva suavizada de horas por edad para el gráfico comunicativo |

## Conclusiones principales

La hipótesis se apoya en parte y se matiza:

- **Educación y tercerización van juntas:** con servicio doméstico pago tienen el 1,2 % de las mujeres de nivel hasta secundario incompleto y el 14,7 % de las universitarias completas (17,0 % ponderado). Chi-cuadrado = 318,44; V de Cramér = 0,23.
- **Tercerización y carga:** el servicio pago se asocia con unos 49 minutos menos de trabajo doméstico por día, pero no con menos cuidado.
- **Educación y carga:** la relación es débil y no lineal (η² = 0,043). Con controles se reduce a cerca de un tercio, y parte de la brecha del terciario incompleto se explica por su composición (es el grupo más joven y con menos niños).
- **Lo que más pesa:** los niños en el hogar y el día no laborable.
- **Uso del tiempo (PCA):** se ordena en trabajo remunerado contra no remunerado y en convivencia contra trabajo. Los puntajes suben con la educación y con el servicio pago, sobre todo por el trabajo remunerado.
- **Robustez:** ninguna conclusión cambia al excluir los 73 casos atípicos.
