# Libraria para pasar pdf
library(pagedown)

# cargar scrit del reporte
source("reporte_backtesting_simple_con_explicaciones.R")

# ingresar parámetros en la funcion 
resultado <- generar_reporte_html(
  archivo_datos = "pred.xlsx",
  carpeta_salida = "reporte_backtesting_feb2025_v4"
)
7# Limpiar memoria
gc()

# Ruta de la carpeta donde vive la salida del archivo
ruta <- "C:/Users/mfsierra/fincomun.com.mx/Riesgos - Área de trabajo modelos de riesgo/2. Proveedores/1. AIS/a. Modelo de originación T-Conecta/pipeline_tc_v2/pipeline_origenTC/"

# ruta completa
carpeta_salida <- paste0(ruta,"reporte_backtesting_feb2025_v4")

# Convertir HTML a PDF
pagedown::chrome_print(
  input = file.path(carpeta_salida, "reporte_backtesting.html"),
  output = file.path(carpeta_salida, "reporte_backtesting.pdf")
)