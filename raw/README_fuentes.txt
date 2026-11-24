FUENTE DE DATOS: ENUT 2021 (INDEC) 
 
1. FUENTE OFICIAL 
- Encuesta: Encuesta Nacional de Uso del Tiempo 2021 (ENUT 2021). 
- Institución: Instituto Nacional de Estadística y Censos (INDEC), Argentina. 
- Enlace oficial: https://www.indec.gob.ar/indec/web/Nivel4-Tema-4-31-166 
 
2. ARCHIVO CRUDO DE DATOS 
- Archivo: raw/enut2021_base.txt (texto delimitado por "|"). 
- Nota: El archivo crudo no se modifica. Todas las transformaciones se hacen directamente con los scripts de R. 
- Unidad de observación: Persona de 14 años o más, una por hogar (tabla de Kish). 
- Dimensiones: 14.350 filas y 327 columnas. 
 
3. DOCUMENTOS DE REFERENCIA 
- Manual de uso de la base, cuestionario y nota técnica. 
- No están en el repositorio; se descargan directamente del sitio oficial del INDEC. 
 
4. FILTROS APLICADOS EN EL TRABAJO (script/01_limpieza_eda.R) 
1. Selección de mujeres (sexo_sel == 1). 
2. Edad de 18 a 64 años. 
3. Se excluyen nivel educativo 8 (educación especial) y 99 (Ns/Nc). 
- Muestra inicial: 6.077 personas -> 6.046 mujeres tras la limpieza. 
- Muestra final con dato de servicio doméstico pago: 6.042 casos. 
  
