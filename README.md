# ML_Report
Reporte de backtesting

Este escript da un reporte automatizado del backtesting para modelos de machine learning supervizados en R sin usar Markdow. Usa las siguientes métricas:

- Gini
- KS
- ROC
- Precisión
- Sensibilidad
- Especificidad

Igualmente da gráficos para analizar la calidad del modelo y Alertas de posibles desviaciones. Se compone de 2 script:

- Ejecutar reporte.r --> manda llamar la funciones, ejecuta los cáculos, genera un html y convierte a pdf
- Reporte Simplificado de Backtesting.r --> genera todos los cálculos, gráficos y html principal

Se coloca base de ejemplo con las entradas necesarias para generar el reporte en el archivo pred.xlsx, unicanmente son 3 datos:

- ID
- Variable objetivo (y)
- predicciones del modelo (y estimada)