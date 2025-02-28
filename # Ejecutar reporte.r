# Libraria para pasar pdf
library(pagedown)

# cargar scrit del reporte
source("# Reporte Simplificado de Backtesting.r")

# ingresar parámetros en la funcion 
resultado <- generar_reporte_html(
  archivo_datos = "pred.xlsx",
  carpeta_salida = "reporte_backtesting_feb2025_v4"
)
7# Limpiar memoria
gc()

# Ruta de la carpeta donde vive la salida del archivo
ruta <- getwd()

# ruta completa
carpeta_salida <- paste0(ruta,"/","reporte_backtesting_feb2025_v4")

# Convertir HTML a PDF
pagedown::chrome_print(
  input = file.path(carpeta_salida, "reporte_backtesting.html"),
  output = file.path(carpeta_salida, "reporte_backtesting.pdf")
)