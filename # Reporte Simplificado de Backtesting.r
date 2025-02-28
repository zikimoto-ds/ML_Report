# Reporte Simplificado de Backtesting
# ------------------------------------------------------

# Función para calcular el coeficiente Gini (relacionado con AUC)
calcular_gini <- function(auc) {
  return(2 * auc - 1)
}

# Función para generar la interpretación automatizada del coeficiente Gini
interpretar_gini <- function(gini) {
  interpretacion <- "El coeficiente Gini mide la desigualdad en la distribución de predicciones entre buenos y malos clientes. "
  
  if (gini >= 0.6) {
    interpretacion <- paste0(interpretacion, "Con un valor de ", round(gini, 3), 
                             ", el modelo muestra una excelente capacidad discriminativa. Esto indica una fuerte separación entre los grupos de alto y bajo riesgo.")
  } else if (gini >= 0.4) {
    interpretacion <- paste0(interpretacion, "Con un valor de ", round(gini, 3), 
                             ", el modelo muestra una buena capacidad discriminativa. La separación entre grupos de riesgo es adecuada para aplicaciones de crédito.")
  } else if (gini >= 0.2) {
    interpretacion <- paste0(interpretacion, "Con un valor de ", round(gini, 3), 
                             ", el modelo muestra una capacidad discriminativa moderada. Podría ser suficiente para algunas aplicaciones, pero hay margen de mejora.")
  } else {
    interpretacion <- paste0(interpretacion, "Con un valor de ", round(gini, 3), 
                             ", el modelo muestra una capacidad discriminativa limitada. Se recomienda revisar las variables o considerar técnicas de modelado alternativas.")
  }
  
  return(interpretacion)
}

# Función para interpretar la tabla de contingencia
interpretar_tabla_contingencia <- function(conf_matrix) {
  VP <- conf_matrix$table[2, 2]  # Verdaderos Positivos
  FP <- conf_matrix$table[1, 2]  # Falsos Positivos
  VN <- conf_matrix$table[1, 1]  # Verdaderos Negativos
  FN <- conf_matrix$table[2, 1]  # Falsos Negativos
  
  total <- sum(conf_matrix$table)
  
  interpretacion <- paste0(
    "La tabla de contingencia muestra que de un total de ", total, " casos: ",
    VP, " clientes de alto riesgo fueron correctamente identificados (Verdaderos Positivos), ",
    FN, " clientes de alto riesgo no fueron detectados (Falsos Negativos), ",
    VN, " clientes de bajo riesgo fueron correctamente clasificados (Verdaderos Negativos), y ",
    FP, " clientes de bajo riesgo fueron clasificados incorrectamente como de alto riesgo (Falsos Positivos)."
  )
  
  # Análisis de consecuencias de negocio
  tasa_deteccion <- VP / (VP + FN)
  tasa_falsa_alarma <- FP / (FP + VN)
  
  interpretacion <- paste0(interpretacion,
                           " Desde una perspectiva de negocio, esto significa que el modelo detecta el ", round(tasa_deteccion * 100, 1), 
                           "% de todos los clientes de alto riesgo, mientras que de todos los clientes realmente buenos, el ", 
                           round(tasa_falsa_alarma * 100, 1), "% son rechazados incorrectamente."
  )
  
  # Recomendaciones basadas en el balance
  if (tasa_deteccion < 0.6) {
    interpretacion <- paste0(interpretacion, 
                             " La baja tasa de detección de clientes de alto riesgo sugiere que se debe revisar el punto de corte o mejorar el modelo para aumentar la sensibilidad."
    )
  } else if (tasa_falsa_alarma > 0.3) {
    interpretacion <- paste0(interpretacion, 
                             " La alta tasa de rechazos incorrectos de buenos clientes sugiere ajustar el punto de corte para mejorar la especificidad, o enriquecer el modelo para mejor discriminación."
    )
  } else {
    interpretacion <- paste0(interpretacion, 
                             " El balance entre detección de riesgo y aprobación de buenos clientes parece adecuado, pero siempre debe evaluarse según las prioridades específicas del negocio."
    )
  }
  
  return(interpretacion)
}

# Función para interpretar el gráfico de distribución de predicciones
interpretar_distribucion <- function(datos) {
  # Análisis de la forma de la distribución
  hist_info <- hist(datos$preds, breaks = 50, plot = FALSE)
  bins <- hist_info$counts
  
  # Detectar si hay bimodalidad (indicador de buena separación)
  es_bimodal <- FALSE
  peaks <- which(diff(sign(diff(c(0, bins, 0)))) == -2)
  if (length(peaks) >= 2) {
    es_bimodal <- TRUE
  }
  
  # Calcular la curtosis como medida de concentración
  media <- mean(datos$preds)
  sd_preds <- sd(datos$preds)
  curtosis <- mean(((datos$preds - media)/sd_preds)^4) - 3
  
  interpretacion <- "La distribución de probabilidades predichas "
  
  if (es_bimodal) {
    interpretacion <- paste0(interpretacion, 
                             "muestra múltiples picos, lo que sugiere una buena separación entre grupos de riesgo. "
    )
  } else {
    if (curtosis > 3) {
      interpretacion <- paste0(interpretacion, 
                               "tiene una concentración muy alta en ciertos valores (alta curtosis), lo que podría indicar una limitada capacidad discriminativa o sobreajuste del modelo. "
      )
    } else if (curtosis > 0) {
      interpretacion <- paste0(interpretacion, 
                               "muestra una concentración moderada alrededor de ciertos valores. "
      )
    } else {
      interpretacion <- paste0(interpretacion, 
                               "tiene una dispersión amplia sin concentraciones extremas. "
      )
    }
  }
  
  # Análisis de la distribución por grupo
  media_buenos <- mean(datos$preds[datos$Malo_A == 0])
  media_malos <- mean(datos$preds[datos$Malo_A == 1])
  diferencia_medias <- media_malos - media_buenos
  
  if (diferencia_medias > 0.3) {
    interpretacion <- paste0(interpretacion, 
                             "La diferencia entre las medias de probabilidad para clientes buenos (", round(media_buenos, 3), 
                             ") y malos (", round(media_malos, 3), ") es significativa, indicando una buena capacidad discriminativa."
    )
  } else if (diferencia_medias > 0.15) {
    interpretacion <- paste0(interpretacion, 
                             "La diferencia entre las medias de probabilidad para clientes buenos (", round(media_buenos, 3), 
                             ") y malos (", round(media_malos, 3), ") es moderada, sugiriendo una capacidad discriminativa aceptable."
    )
  } else {
    interpretacion <- paste0(interpretacion, 
                             "La diferencia entre las medias de probabilidad para clientes buenos (", round(media_buenos, 3), 
                             ") y malos (", round(media_malos, 3), ") es pequeña, lo que podría indicar una limitada capacidad discriminativa del modelo."
    )
  }
  
  return(interpretacion)
}

# Función para interpretar la curva ROC
interpretar_curva_roc <- function(auc_valor) {
  interpretacion <- "La curva ROC ilustra el equilibrio entre sensibilidad (tasa de verdaderos positivos) y 1-especificidad (tasa de falsos positivos) a diferentes puntos de corte. "
  
  if (auc_valor >= 0.8) {
    interpretacion <- paste0(interpretacion, 
                             "Con un AUC de ", round(auc_valor, 3), 
                             ", el modelo muestra un excelente poder discriminativo. Esto significa que el modelo tiene una alta capacidad para distinguir entre clientes de alto y bajo riesgo. En aplicaciones crediticias, este nivel de AUC es considerado muy bueno y sugiere que el modelo puede ser efectivo para decisiones de aprobación y pricing basados en riesgo."
    )
  } else if (auc_valor >= 0.7) {
    interpretacion <- paste0(interpretacion, 
                             "Con un AUC de ", round(auc_valor, 3), 
                             ", el modelo muestra un buen poder discriminativo. El modelo tiene una capacidad adecuada para distinguir entre clientes de alto y bajo riesgo, suficiente para la mayoría de aplicaciones crediticias estándar. Se podría considerar su uso para decisiones de aprobación y asignación de límites."
    )
  } else if (auc_valor >= 0.6) {
    interpretacion <- paste0(interpretacion, 
                             "Con un AUC de ", round(auc_valor, 3), 
                             ", el modelo muestra un poder discriminativo moderado. Aunque está por encima del nivel aleatorio (0.5), hay margen para mejorar la capacidad del modelo para distinguir entre clientes de alto y bajo riesgo. Podría ser suficiente para algunas aplicaciones menos críticas o como complemento a otros criterios de decisión."
    )
  } else {
    interpretacion <- paste0(interpretacion, 
                             "Con un AUC de ", round(auc_valor, 3), 
                             ", el modelo muestra un poder discriminativo limitado. Su capacidad para distinguir entre clientes de alto y bajo riesgo es apenas superior a una clasificación aleatoria. Se recomienda revisar las variables utilizadas o considerar técnicas de modelado alternativas antes de usar el modelo en un entorno de producción."
    )
  }
  
  return(interpretacion)
}

# Función para interpretar el boxplot por grupo
interpretar_boxplot <- function(datos) {
  # Calcular estadísticas para buenos y malos
  stats_buenos <- summary(datos$preds[datos$Malo_A == 0])
  stats_malos <- summary(datos$preds[datos$Malo_A == 1])
  
  # Calcular superposición entre cuartiles
  q1_buenos <- stats_buenos["1st Qu."]
  q3_buenos <- stats_buenos["3rd Qu."]
  q1_malos <- stats_malos["1st Qu."]
  q3_malos <- stats_malos["3rd Qu."]
  
  # Diferencia entre medianas
  med_buenos <- stats_buenos["Median"]
  med_malos <- stats_malos["Median"]
  dif_medianas <- med_malos - med_buenos
  
  # Comprobar si hay superposición entre los rangos intercuartílicos
  superposicion <- (q1_malos <= q3_buenos && q3_malos >= q1_buenos)
  grado_superposicion <- 0
  
  if (superposicion) {
    # Calcular porcentaje de superposición
    rango_superposicion <- min(q3_buenos, q3_malos) - max(q1_buenos, q1_malos)
    rango_total <- max(q3_buenos, q3_malos) - min(q1_buenos, q1_malos)
    grado_superposicion <- rango_superposicion / rango_total
  }
  
  # Generar interpretación
  interpretacion <- "El boxplot muestra la distribución de probabilidades predichas para clientes buenos (0) y malos (1). "
  
  if (dif_medianas > 0.3) {
    interpretacion <- paste0(interpretacion, 
                             "La diferencia entre las medianas es sustancial (", round(dif_medianas, 3), 
                             "), lo que indica una buena capacidad del modelo para discriminar entre buenos y malos clientes. "
    )
  } else if (dif_medianas > 0.15) {
    interpretacion <- paste0(interpretacion, 
                             "La diferencia entre las medianas es moderada (", round(dif_medianas, 3), 
                             "), indicando una capacidad de discriminación aceptable del modelo. "
    )
  } else {
    interpretacion <- paste0(interpretacion, 
                             "La diferencia entre las medianas es pequeña (", round(dif_medianas, 3), 
                             "), sugiriendo una capacidad de discriminación limitada del modelo. "
    )
  }
  
  if (!superposicion) {
    interpretacion <- paste0(interpretacion, 
                             "No hay superposición entre los rangos intercuartílicos de ambos grupos, lo que es ideal para la discriminación. "
    )
  } else if (grado_superposicion < 0.3) {
    interpretacion <- paste0(interpretacion, 
                             "Hay una superposición limitada entre los rangos intercuartílicos, lo que indica buena separación entre grupos. "
    )
  } else if (grado_superposicion < 0.6) {
    interpretacion <- paste0(interpretacion, 
                             "Existe una superposición moderada entre los rangos intercuartílicos, indicando que algunos clientes podrían ser difíciles de clasificar correctamente. "
    )
  } else {
    interpretacion <- paste0(interpretacion, 
                             "Hay una superposición significativa entre los rangos intercuartílicos, lo que sugiere dificultad para discriminar entre buenos y malos clientes en algunos segmentos. "
    )
  }
  
  return(interpretacion)
}

# Función para interpretar la curva de ganancia
interpretar_ganancia <- function(stats_deciles) {
  # Calcular captura de malos en primeros deciles
  captura_20pct <- stats_deciles$pct_malos_acum[2] * 100  # Primer 20% (deciles 1-2)
  captura_30pct <- stats_deciles$pct_malos_acum[3] * 100  # Primer 30% (deciles 1-3)
  captura_50pct <- stats_deciles$pct_malos_acum[5] * 100  # Primer 50% (deciles 1-5)
  
  # Generar interpretación
  interpretacion <- "La curva de ganancia muestra la eficiencia del modelo para identificar clientes de alto riesgo cuando se ordena la población por riesgo predicho. "
  
  if (captura_30pct >= 70) {
    interpretacion <- paste0(interpretacion, 
                             "El modelo muestra una excelente concentración de riesgo, capturando el ", round(captura_30pct, 1), 
                             "% de todos los clientes malos en apenas el 30% de la población con mayor riesgo predicho. "
    )
  } else if (captura_30pct >= 55) {
    interpretacion <- paste0(interpretacion, 
                             "El modelo muestra una buena concentración de riesgo, capturando el ", round(captura_30pct, 1), 
                             "% de todos los clientes malos en el 30% de la población con mayor riesgo predicho. "
    )
  } else {
    interpretacion <- paste0(interpretacion, 
                             "El modelo muestra una concentración de riesgo moderada, capturando el ", round(captura_30pct, 1), 
                             "% de todos los clientes malos en el 30% de la población con mayor riesgo predicho. "
    )
  }
  
  # Añadir información sobre diferentes segmentos
  interpretacion <- paste0(interpretacion, 
                           "Específicamente, el modelo identifica el ", round(captura_20pct, 1), 
                           "% de todos los clientes malos en el primer 20% de la población, y el ", 
                           round(captura_50pct, 1), "% en el primer 50%. "
  )
  
  # Recomendaciones de uso
  if (captura_20pct >= 50) {
    interpretacion <- paste0(interpretacion, 
                             "Esta alta concentración en los primeros segmentos permite implementar estrategias de rechazo selectivo o requisitos adicionales para los clientes de mayor riesgo, maximizando la eficiencia en la gestión de riesgo."
    )
  } else {
    interpretacion <- paste0(interpretacion, 
                             "Para mejorar la eficiencia en la gestión de riesgo, podría considerarse complementar este modelo con variables o técnicas adicionales que ayuden a concentrar mejor los clientes de alto riesgo en los primeros segmentos."
    )
  }
  
  return(interpretacion)
}

# Función para interpretar la calibración
interpretar_calibracion <- function(cal_data, max_desviacion_cal) {
  # Calcular la tendencia general (sobreestimación vs subestimación)
  n_sobre <- sum(cal_data$avg_pred > cal_data$obs_rate)
  n_sub <- sum(cal_data$avg_pred < cal_data$obs_rate)
  
  # Identificar deciles con mayor desviación
  cal_data <- cal_data %>%
    mutate(desviacion_abs = abs(avg_pred - obs_rate))
  
  decil_max_desv <- cal_data$decil[which.max(cal_data$desviacion_abs)]
  max_desv_valor <- max(cal_data$desviacion_abs)
  es_sobre <- cal_data$avg_pred[which.max(cal_data$desviacion_abs)] > cal_data$obs_rate[which.max(cal_data$desviacion_abs)]
  
  # Interpretar tendencia general
  interpretacion <- "El gráfico de calibración compara las probabilidades predichas por el modelo con las tasas reales observadas. "
  
  if (max_desviacion_cal <= 0.05) {
    interpretacion <- paste0(interpretacion, 
                             "El modelo muestra una excelente calibración, con una desviación máxima de solo ", 
                             round(max_desviacion_cal * 100, 1), "% entre valores predichos y observados. "
    )
  } else if (max_desviacion_cal <= 0.1) {
    interpretacion <- paste0(interpretacion, 
                             "El modelo muestra una calibración aceptable, con una desviación máxima de ", 
                             round(max_desviacion_cal * 100, 1), "% entre valores predichos y observados. "
    )
  } else {
    interpretacion <- paste0(interpretacion, 
                             "El modelo muestra problemas de calibración, con una desviación máxima de ", 
                             round(max_desviacion_cal * 100, 1), "% entre valores predichos y observados. "
    )
  }
  
  # Detallar tendencia
  if (n_sobre > n_sub * 2) {
    interpretacion <- paste0(interpretacion, 
                             "Existe una clara tendencia a sobreestimar el riesgo en la mayoría de los deciles, lo que podría llevar a rechazos innecesarios de buenos clientes. "
    )
  } else if (n_sub > n_sobre * 2) {
    interpretacion <- paste0(interpretacion, 
                             "Existe una clara tendencia a subestimar el riesgo en la mayoría de los deciles, lo que podría aumentar la exposición a clientes de alto riesgo. "
    )
  } else {
    interpretacion <- paste0(interpretacion, 
                             "No hay una tendencia sistemática clara de sobreestimación o subestimación del riesgo. "
    )
  }
  
  # Especificar el decil con mayor desviación
  if (es_sobre) {
    interpretacion <- paste0(interpretacion, 
                             "La mayor desviación se observa en el decil ", decil_max_desv, 
                             ", donde el modelo sobreestima el riesgo en ", round(max_desv_valor * 100, 1), "%. "
    )
  } else {
    interpretacion <- paste0(interpretacion, 
                             "La mayor desviación se observa en el decil ", decil_max_desv, 
                             ", donde el modelo subestima el riesgo en ", round(max_desv_valor * 100, 1), "%. "
    )
  }
  
  # Recomendaciones basadas en la calibración
  if (max_desviacion_cal > 0.1) {
    interpretacion <- paste0(interpretacion, 
                             "Se recomienda considerar una recalibración del modelo, especialmente para los deciles con mayores desviaciones, para mejorar la precisión de las probabilidades predichas."
    )
  } else {
    interpretacion <- paste0(interpretacion, 
                             "La calibración es suficiente para la mayoría de las aplicaciones prácticas, aunque siempre es recomendable monitorear la estabilidad de la calibración con el tiempo."
    )
  }
  
  return(interpretacion)
}

# Función para interpretar el gráfico KS
interpretar_ks <- function(ks_valor, ks_cutoff) {
  interpretacion <- "El gráfico KS muestra la separación entre las distribuciones acumulativas de clientes buenos y malos. "
  
  if (ks_valor >= 0.4) {
    interpretacion <- paste0(interpretacion, 
                             "Con un valor KS de ", round(ks_valor, 3), 
                             ", el modelo muestra una excelente capacidad para separar clientes buenos y malos. Este nivel de separación es ideal para aplicaciones crediticias, permitiendo una clara distinción entre los diferentes perfiles de riesgo."
    )
  } else if (ks_valor >= 0.3) {
    interpretacion <- paste0(interpretacion, 
                             "Con un valor KS de ", round(ks_valor, 3), 
                             ", el modelo muestra una buena capacidad para separar clientes buenos y malos. Esta separación es adecuada para la mayoría de aplicaciones crediticias estándar."
    )
  } else if (ks_valor >= 0.2) {
    interpretacion <- paste0(interpretacion, 
                             "Con un valor KS de ", round(ks_valor, 3), 
                             ", el modelo muestra una capacidad moderada para separar clientes buenos y malos. Aunque aceptable, hay margen para mejorar la capacidad discriminativa del modelo."
    )
  } else {
    interpretacion <- paste0(interpretacion, 
                             "Con un valor KS de ", round(ks_valor, 3), 
                             ", el modelo muestra una capacidad limitada para separar clientes buenos y malos. Se recomienda revisar las variables o técnicas de modelado para mejorar la separación."
    )
  }
  
  interpretacion <- paste0(interpretacion, 
                           " El punto de corte óptimo según KS es ", round(ks_cutoff, 4), 
                           ", donde se maximiza la diferencia entre las distribuciones acumulativas de buenos y malos clientes."
  )
  
  return(interpretacion)
}

# Función para interpretar la tasa de malos por decil
interpretar_tasa_malos_decil <- function(stats_deciles) {
  # Calcular la razón entre el decil de mayor y menor riesgo
  ratio_deciles <- stats_deciles$tasa_malos[1] / stats_deciles$tasa_malos[10]
  
  # Verificar si la tendencia es consistentemente decreciente
  tendencia_decreciente <- all(diff(stats_deciles$tasa_malos) <= 0.01) # Permitir pequeñas fluctuaciones
  
  # Calcular la tasa de malos promedio
  tasa_promedio <- mean(stats_deciles$tasa_malos)
  
  # Generar interpretación
  interpretacion <- "El gráfico de tasa de malos por decil muestra la concentración de riesgo a lo largo de los deciles ordenados por probabilidad predicha. "
  
  if (ratio_deciles >= 5) {
    interpretacion <- paste0(interpretacion, 
                             "La ratio entre el decil de mayor riesgo (", round(stats_deciles$tasa_malos[1] * 100, 1), 
                             "%) y el de menor riesgo (", round(stats_deciles$tasa_malos[10] * 100, 1), 
                             "%) es de ", round(ratio_deciles, 1), 
                             "x, lo que indica una excelente capacidad discriminativa del modelo. "
    )
  } else if (ratio_deciles >= 3) {
    interpretacion <- paste0(interpretacion, 
                             "La ratio entre el decil de mayor riesgo (", round(stats_deciles$tasa_malos[1] * 100, 1), 
                             "%) y el de menor riesgo (", round(stats_deciles$tasa_malos[10] * 100, 1), 
                             "%) es de ", round(ratio_deciles, 1), 
                             "x, lo que indica una buena capacidad discriminativa del modelo. "
    )
  } else {
    interpretacion <- paste0(interpretacion, 
                             "La ratio entre el decil de mayor riesgo (", round(stats_deciles$tasa_malos[1] * 100, 1), 
                             "%) y el de menor riesgo (", round(stats_deciles$tasa_malos[10] * 100, 1), 
                             "%) es de solo ", round(ratio_deciles, 1), 
                             "x, lo que podría indicar una capacidad discriminativa limitada. "
    )
  }
  
  if (tendencia_decreciente) {
    interpretacion <- paste0(interpretacion, 
                             "La tendencia de la tasa de malos es consistentemente decreciente a lo largo de los deciles, lo que confirma que el ordenamiento por riesgo predicho es efectivo. "
    )
  } else {
    interpretacion <- paste0(interpretacion, 
                             "Se observan algunas fluctuaciones en la tendencia decreciente de la tasa de malos, lo que podría indicar oportunidades para mejorar la calibración en ciertos segmentos. "
    )
  }
  
  # Análisis de concentración
  interpretacion <- paste0(interpretacion, 
                           "El decil de mayor riesgo tiene una tasa de incumplimiento de ", round(stats_deciles$tasa_malos[1] * 100, 1), 
                           "%, que es ", round(stats_deciles$tasa_malos[1] / tasa_promedio, 1), 
                           " veces la tasa promedio (", round(tasa_promedio * 100, 1), "%). "
  )
  
  # Recomendaciones estratégicas
  if (ratio_deciles >= 4) {
    interpretacion <- paste0(interpretacion, 
                             "Esta marcada diferenciación entre deciles permite implementar estrategias comerciales y de riesgo altamente segmentadas, con políticas más estrictas para los primeros deciles y ofreciendo condiciones preferenciales a los deciles de menor riesgo."
    )
  } else if (ratio_deciles >= 2.5) {
    interpretacion <- paste0(interpretacion, 
                             "Esta diferenciación entre deciles permite implementar algunas estrategias segmentadas, aunque la separación entre los segmentos intermedios podría no ser lo suficientemente pronunciada para políticas muy diferenciadas."
    )
  } else {
    interpretacion <- paste0(interpretacion, 
                             "La limitada diferenciación entre deciles sugiere que podría ser difícil implementar estrategias comerciales y de riesgo altamente segmentadas. Se recomienda evaluar la inclusión de variables adicionales que ayuden a mejorar la separación entre segmentos."
    )
  }
  
  return(interpretacion)
}

# Función para interpretar métricas por punto de corte
interpretar_metricas_punto_corte <- function(resultados_cortes, ks_cutoff) {
  # Identificar el punto de corte donde se cruzan sensibilidad y especificidad
  diffs <- abs(resultados_cortes$sensitivity - resultados_cortes$specificity)
  idx_equilibrio <- which.min(diffs)
  punto_equilibrio <- resultados_cortes$punto_corte[idx_equilibrio]
  
  # Identificar el punto que maximiza accuracy
  idx_max_accuracy <- which.max(resultados_cortes$accuracy)
  punto_max_accuracy <- resultados_cortes$punto_corte[idx_max_accuracy]
  
  # Identificar el punto que maximiza F1
  idx_max_f1 <- which.max(resultados_cortes$f1_score)
  punto_max_f1 <- resultados_cortes$punto_corte[idx_max_f1]
  
  # Generar interpretación
  interpretacion <- "El gráfico de métricas por punto de corte muestra cómo varían las principales métricas de rendimiento al cambiar el umbral de clasificación. "
  
  interpretacion <- paste0(interpretacion, 
                           "El punto de corte óptimo según KS es ", round(ks_cutoff, 3), 
                           ", pero existen otros puntos relevantes: "
  )
  
  interpretacion <- paste0(interpretacion, 
                           "el punto ", round(punto_equilibrio, 3), 
                           " ofrece el mejor equilibrio entre sensibilidad y especificidad (ambas cerca de ", 
                           round(resultados_cortes$sensitivity[idx_equilibrio] * 100, 1), "%); "
  )
  
  interpretacion <- paste0(interpretacion, 
                           "el punto ", round(punto_max_accuracy, 3), 
                           " maximiza la accuracy global (", 
                           round(resultados_cortes$accuracy[idx_max_accuracy] * 100, 1), "%); "
  )
  
  interpretacion <- paste0(interpretacion, 
                           "y el punto ", round(punto_max_f1, 3), 
                           " maximiza el F1-score (", 
                           round(resultados_cortes$f1_score[idx_max_f1] * 100, 1), "%), que es una media armónica entre precisión y sensibilidad. "
  )
  
  # Recomendaciones según objetivos
  interpretacion <- paste0(interpretacion, 
                           "La elección del punto de corte debería basarse en los objetivos específicos del negocio: "
  )
  
  interpretacion <- paste0(interpretacion, 
                           "si la prioridad es maximizar la detección de clientes de alto riesgo, se recomienda un punto más bajo (mayor sensibilidad); "
  )
  
  interpretacion <- paste0(interpretacion, 
                           "si la prioridad es minimizar los rechazos incorrectos de buenos clientes, se recomienda un punto más alto (mayor especificidad); "
  )
  
  interpretacion <- paste0(interpretacion, 
                           "y si se busca un balance entre ambos objetivos, el punto ", round(punto_equilibrio, 3), 
                           " o el punto ", round(punto_max_f1, 3), " serían opciones adecuadas."
  )
  
  return(interpretacion)
}

# Función para interpretar impacto en tasa de aprobación y bad rate
interpretar_tasas_aprobacion <- function(resultados_cortes) {
  # Identificar algunos puntos clave para el análisis
  puntos_corte <- resultados_cortes$punto_corte
  
  # Buscar punto donde bad rate es aproximadamente 5%
  idx_target_br <- which.min(abs(resultados_cortes$bad_rate_approved - 0.05))
  punto_target_br <- resultados_cortes$punto_corte[idx_target_br]
  br_en_punto <- resultados_cortes$bad_rate_approved[idx_target_br]
  apr_en_punto <- resultados_cortes$approval_rate[idx_target_br]
  
  # Buscar punto donde la aprobación es aproximadamente 70%
  idx_target_apr <- which.min(abs(resultados_cortes$approval_rate - 0.7))
  punto_target_apr <- resultados_cortes$punto_corte[idx_target_apr]
  br_en_punto_apr <- resultados_cortes$bad_rate_approved[idx_target_apr]
  apr_en_punto_apr <- resultados_cortes$approval_rate[idx_target_apr]
  
  # Generar interpretación
  interpretacion <- "El gráfico de tasas de aprobación muestra la relación entre el punto de corte, la tasa de aprobación y el bad rate resultante, ilustrando el trade-off fundamental entre volumen y riesgo. "
  
  # Análisis de la relación general
  interpretacion <- paste0(interpretacion, 
                           "Como es de esperar, a medida que se reduce el punto de corte, aumenta la tasa de aprobación pero también aumenta el bad rate entre los aprobados. "
  )
  
  # Análisis para punto de corte con bad rate objetivo
  interpretacion <- paste0(interpretacion, 
                           "Si se establece un objetivo de bad rate cercano al 5%, el punto de corte apropiado sería aproximadamente ", 
                           round(punto_target_br, 3), ", resultando en una tasa de aprobación del ", 
                           round(apr_en_punto * 100, 1), "% y un bad rate exacto del ", 
                           round(br_en_punto * 100, 1), "%. "
  )
  
  # Análisis para punto de corte con aprobación objetivo
  interpretacion <- paste0(interpretacion, 
                           "Por otro lado, si el objetivo de negocio es mantener una tasa de aprobación del 70%, el punto de corte debería ser cercano a ", 
                           round(punto_target_apr, 3), ", lo que resultaría en un bad rate del ", 
                           round(br_en_punto_apr * 100, 1), "%. "
  )
  
  # Recomendaciones generales
  interpretacion <- paste0(interpretacion, 
                           "La elección final del punto de corte dependerá del apetito de riesgo de la organización y de los objetivos comerciales. "
  )
  
  interpretacion <- paste0(interpretacion, 
                           "Un enfoque conservador priorizaría mantener el bad rate por debajo de un umbral específico, mientras que un enfoque orientado al crecimiento podría priorizar alcanzar una determinada tasa de aprobación, aceptando el incremento correspondiente en el riesgo. "
  )
  
  interpretacion <- paste0(interpretacion, 
                           "También es importante considerar que estos resultados se basan en datos históricos, y la relación entre punto de corte, aprobaciones y bad rate podría variar con el tiempo o en diferentes segmentos de clientes."
  )
  
  return(interpretacion)
}

# Función para interpretar estadísticas por decil
interpretar_estadisticas_decil <- function(stats_deciles) {
  # Encontrar deciles con valores extremos o interesantes
  decil_max_tasa <- which.max(stats_deciles$tasa_malos)
  decil_min_tasa <- which.min(stats_deciles$tasa_malos)
  
  # Calcular la concentración de malos en primeros deciles
  malos_top3 <- sum(stats_deciles$n_malos[1:3]) / sum(stats_deciles$n_malos)
  
  # Generar interpretación
  interpretacion <- "La tabla de estadísticas por decil permite analizar en detalle el rendimiento del modelo en cada segmento de riesgo. "
  
  # Análisis de concentración
  interpretacion <- paste0(interpretacion, 
                           "Los primeros 3 deciles (30% de la población con mayor riesgo predicho) concentran el ", 
                           round(malos_top3 * 100, 1), "% del total de clientes malos. "
  )
  
  # Análisis de deciles extremos
  interpretacion <- paste0(interpretacion, 
                           "El decil ", decil_max_tasa, " muestra la mayor tasa de malos con ", 
                           round(stats_deciles$tasa_malos[decil_max_tasa] * 100, 1), 
                           "%, mientras que el decil ", decil_min_tasa, " tiene la menor tasa con apenas ", 
                           round(stats_deciles$tasa_malos[decil_min_tasa] * 100, 1), "%. "
  )
  
  # Análisis de la capacidad predictiva por rangos
  interpretacion <- paste0(interpretacion, 
                           "El rango de probabilidades predichas va desde ", 
                           round(min(stats_deciles$min_pred), 4), " hasta ", 
                           round(max(stats_deciles$max_pred), 4), ", con variaciones significativas entre deciles. "
  )
  
  # Recomendaciones estratégicas
  interpretacion <- paste0(interpretacion, 
                           "Esta segmentación permite diseñar estrategias específicas para cada nivel de riesgo: por ejemplo, implementar requisitos adicionales o garantías para los deciles 1-3, condiciones estándar para los deciles 4-7, y posibles beneficios como tasas preferenciales o límites más altos para los deciles 8-10. "
  )
  
  interpretacion <- paste0(interpretacion, 
                           "También se puede utilizar esta información para ajustar los niveles de aprovisionamiento según el perfil de riesgo, o para estrategias de pricing basado en riesgo."
  )
  
  return(interpretacion)
}

# Función para interpretar evaluación de diferentes puntos de corte
interpretar_diferentes_puntos_corte <- function(resultados_cortes) {
  # Ordenar los resultados por accuracy
  idx_ordenados <- order(resultados_cortes$accuracy, decreasing = TRUE)
  mejores_accuracy <- resultados_cortes[idx_ordenados[1:3], ]
  
  # Ordenar por F1 score
  idx_ordenados_f1 <- order(resultados_cortes$f1_score, decreasing = TRUE)
  mejores_f1 <- resultados_cortes[idx_ordenados_f1[1:3], ]
  
  # Generar interpretación
  interpretacion <- "La tabla de evaluación de diferentes puntos de corte proporciona una visión detallada de cómo varían las métricas de rendimiento con distintos umbrales de clasificación. "
  
  # Mejores puntos por accuracy
  interpretacion <- paste0(interpretacion, 
                           "Los puntos de corte con mejor accuracy son: ", 
                           paste(sprintf("%.3f (%.1f%%)", 
                                         mejores_accuracy$punto_corte[1:3], 
                                         mejores_accuracy$accuracy[1:3] * 100), 
                                 collapse = ", "), ". "
  )
  
  # Mejores puntos por F1
  interpretacion <- paste0(interpretacion, 
                           "Los puntos con mejor balance entre precisión y sensibilidad (F1 score) son: ", 
                           paste(sprintf("%.3f (%.1f%%)", 
                                         mejores_f1$punto_corte[1:3], 
                                         mejores_f1$f1_score[1:3] * 100), 
                                 collapse = ", "), ". "
  )
  
  # Análisis de tasa de aprobación vs bad rate
  interpretacion <- paste0(interpretacion, 
                           "La tabla muestra claramente cómo varía el trade-off entre tasa de aprobación y bad rate: por ejemplo, con un punto de corte de ", 
                           round(resultados_cortes$punto_corte[1], 3), 
                           ", la tasa de aprobación es del ", 
                           round(resultados_cortes$approval_rate[1] * 100, 1), 
                           "% con un bad rate del ", 
                           round(resultados_cortes$bad_rate_approved[1] * 100, 1), 
                           "%, mientras que con ", 
                           round(resultados_cortes$punto_corte[nrow(resultados_cortes)], 3), 
                           ", la aprobación aumenta al ", 
                           round(resultados_cortes$approval_rate[nrow(resultados_cortes)] * 100, 1), 
                           "% pero el bad rate sube al ", 
                           round(resultados_cortes$bad_rate_approved[nrow(resultados_cortes)] * 100, 1), "%."
  )
  
  # Recomendación de uso
  interpretacion <- paste0(interpretacion, 
                           " La información de esta tabla es especialmente útil para realizar análisis de sensibilidad y planificación de escenarios, permitiendo a los equipos de negocio y riesgo evaluar el impacto de diferentes políticas de aprobación."
  )
  
  return(interpretacion)
}


# Función para generar un reporte completo sin depender de RMarkdown
generar_reporte_html <- function(archivo_datos, carpeta_salida = "reporte_backtesting") {
  # Cargar librerías necesarias
  library(readxl)       # Para leer archivos Excel
  library(dplyr)        # Para manipulación de datos
  library(ggplot2)      # Para visualizaciones
  library(pROC)         # Para curva ROC y AUC
  library(scales)       # Para formateo de ejes
  library(caret)        # Para matriz de confusión
  library(e1071)        # Para cálculos estadísticos (curtosis, etc.)
  
  # Crear carpeta de salida si no existe
  if (!dir.exists(carpeta_salida)) {
    dir.create(carpeta_salida)
  }
  
  # 1. Cargar los datos desde el archivo Excel
  cat("Cargando datos desde:", archivo_datos, "\n")
  datos <- read_excel(archivo_datos)
  
  # Verificar la estructura de los datos
  cat("Estructura de los datos:\n")
  print(str(datos))
  cat("\nResumen de los datos:\n")
  print(summary(datos))
  
  # 2. Preparación de los datos
  # Asegurarse que Malo_A sea factor
  datos$Malo_A <- as.factor(datos$Malo_A)
  
  # 3. Análisis general de la distribución de predicciones
  cat("Generando gráficos de distribución...\n")
  
  # ----------------------------------------
  # Histograma de Probabilidades Predichas
  # ----------------------------------------
  
  # Guardar histograma
  png(file.path(carpeta_salida, "histograma_predicciones.png"), width = 800, height = 600)
  hist_pred <- hist(datos$preds, breaks = 50, 
                    main = "Distribución de Probabilidades Predichas",
                    xlab = "Probabilidad Predicha", 
                    ylab = "Frecuencia",
                    col = "skyblue")
  dev.off()
  gc()
  # ----------------------------------------
  # Boxplot por Grupo (Buenos vs Malos)
  # ----------------------------------------
  
  # Boxplot por grupo
  png(file.path(carpeta_salida, "boxplot_por_grupo.png"), width = 800, height = 600)
  boxplot(preds ~ Malo_A, data = datos, 
          main = "Distribución de Predicciones por Grupo",
          xlab = "Estado del Cliente (0=Bueno, 1=Malo)", 
          ylab = "Probabilidad Predicha",
          col = c("green3", "red3"))
  dev.off()
  
  # 4. Métricas de desempeño
  cat("Calculando métricas de desempeño...\n")
  # Calcular AUC (Area Under Curve)
  roc_obj <- roc(datos$Malo_A, datos$preds)
  auc_valor <- auc(roc_obj)
  
  # Calcular Gini basado en AUC
  gini_valor <- 2 * auc_valor - 1
  gc()
  # ----------------------------------------
  # Curva ROC
  # ----------------------------------------
  
  # Crear gráfico ROC
  png(file.path(carpeta_salida, "curva_roc.png"), width = 800, height = 600)
  plot(roc_obj, main = paste("Curva ROC (AUC =", round(auc_valor, 4), ")"),
       xlab = "Tasa de Falsos Positivos (1-Especificidad)",
       ylab = "Tasa de Verdaderos Positivos (Sensibilidad)",
       col = "blue", lwd = 2)
  abline(a = 0, b = 1, lty = 2, col = "gray")
  legend("bottomright", legend = paste("AUC =", round(auc_valor, 4)), 
         col = "blue", lwd = 2)
  dev.off()
  
  # 5. Cálculo del estadístico KS (Kolmogorov-Smirnov)
  cat("Calculando estadístico KS...\n")
  # Crear dataframe para el cálculo
  ks_data <- data.frame(
    pred = datos$preds,
    target = as.numeric(as.character(datos$Malo_A))
  )
  
  # Ordenar por predicción
  ks_data <- ks_data %>%
    arrange(pred)
  
  # Calcular distribuciones acumulativas
  ks_data <- ks_data %>%
    mutate(
      rank = row_number(),
      cum_bads = cumsum(target == 1) / sum(target == 1),
      cum_goods = cumsum(target == 0) / sum(target == 0),
      diff = abs(cum_bads - cum_goods)
    )
  
  # Encontrar el valor máximo de la diferencia (KS)
  ks_valor <- max(ks_data$diff)
  ks_cutoff <- ks_data$pred[which.max(ks_data$diff)]
  gc()
  # ----------------------------------------
  # Gráfico KS (Kolmogorov-Smirnov)
  # ----------------------------------------
  
  # Gráfico KS
  png(file.path(carpeta_salida, "grafico_ks.png"), width = 800, height = 600)
  plot(ks_data$pred, ks_data$cum_bads, type = "l", col = "red", lwd = 2,
       xlab = "Punto de Corte (Score)", ylab = "Tasa Acumulativa",
       main = paste("Gráfico KS (Valor KS =", round(ks_valor, 4), ")"))
  lines(ks_data$pred, ks_data$cum_goods, col = "green", lwd = 2)
  abline(v = ks_cutoff, lty = 2)
  text(ks_cutoff + 0.05, 0.5, paste("KS =", round(ks_valor, 4)), col = "blue", cex = 1.2)
  legend("topleft", legend = c("Malos", "Buenos"), 
         col = c("red", "green"), lwd = 2)
  dev.off()
  
  # 6. Análisis por deciles
  cat("Analizando rendimiento por deciles...\n")
  # Crear deciles basados en la predicción (descendente para que decil 1 sea el de mayor riesgo)
  datos <- datos %>%
    mutate(decil = ntile(desc(preds), 10))
  
  # Calcular estadísticas por decil
  stats_deciles <- datos %>%
    group_by(decil) %>%
    summarise(
      n = n(),
      n_malos = sum(as.numeric(as.character(Malo_A)) == 1),
      n_buenos = sum(as.numeric(as.character(Malo_A)) == 0),
      tasa_malos = n_malos / n,
      min_pred = min(preds),
      max_pred = max(preds),
      avg_pred = mean(preds)
    ) %>%
    mutate(
      pct_total = n / sum(n),
      pct_malos = n_malos / sum(datos$Malo_A == 1),
      pct_buenos = n_buenos / sum(datos$Malo_A == 0),
      pct_malos_acum = cumsum(pct_malos),
      pct_buenos_acum = cumsum(pct_buenos),
      odds = ifelse(n_buenos > 0, n_malos / n_buenos, Inf),
      lift = tasa_malos / mean(as.numeric(as.character(datos$Malo_A)))
    )
  gc()
  # Guardar tabla de deciles
  write.csv(stats_deciles, file.path(carpeta_salida, "estadisticas_deciles.csv"), row.names = FALSE)
  
  # ----------------------------------------
  # Tasa de Malos por Decil
  # ----------------------------------------
  
  # Crear gráfico de concentración de malos por decil
  png(file.path(carpeta_salida, "tasa_malos_por_decil.png"), width = 800, height = 600)
  barplot(stats_deciles$tasa_malos, names.arg = stats_deciles$decil,
          main = "Tasa de Malos por Decil (1 = Mayor Riesgo Predicho)",
          xlab = "Decil", ylab = "Tasa de Malos",
          col = "steelblue")
  # Añadir etiquetas de porcentaje
  text(x = seq(1.5, 20, by = 2), y = stats_deciles$tasa_malos + 0.02,
       labels = paste0(round(stats_deciles$tasa_malos * 100, 1), "%"))
  dev.off()
  gc()
  # ----------------------------------------
  # Curva de Ganancia Acumulada
  # ----------------------------------------
  
  # Curva de ganancia acumulada
  png(file.path(carpeta_salida, "curva_ganancia.png"), width = 800, height = 600)
  plot(cumsum(stats_deciles$pct_total), stats_deciles$pct_malos_acum,
       type = "l", col = "red", lwd = 2,
       xlab = "% Acumulado de Población", ylab = "% Acumulado de Malos",
       main = "Curva de Ganancia Acumulada")
  abline(a = 0, b = 1, lty = 2, col = "gray")
  dev.off()
  
  # 7. Calcular métricas de rendimiento adicionales
  cat("Calculando métricas de rendimiento adicionales...\n")
  # Information Value (IV)
  stats_deciles <- stats_deciles %>%
    mutate(
      woe = log(pct_buenos / pct_malos),
      iv_component = (pct_buenos - pct_malos) * woe
    )
  
  iv_total <- sum(stats_deciles$iv_component, na.rm = TRUE)
  
  # Precisión general del modelo usando punto de corte óptimo (KS)
  datos$prediccion_binaria <- as.factor(ifelse(datos$preds >= ks_cutoff, 1, 0))
  conf_matrix <- confusionMatrix(datos$prediccion_binaria, datos$Malo_A, positive = "1")
  
  # 8. Análisis de calibración
  cat("Analizando calibración del modelo...\n")
  # Comparar predicción vs realidad por deciles
  cal_data <- datos %>%
    group_by(decil) %>%
    summarise(
      avg_pred = mean(preds),
      obs_rate = mean(as.numeric(as.character(Malo_A)))
    )
  
  # Calcular la máxima desviación en calibración
  cal_data <- cal_data %>%
    mutate(desviacion = abs(avg_pred - obs_rate))
  
  max_desviacion_cal <- max(cal_data$desviacion)
  gc()
  # ----------------------------------------
  # Gráfico de Calibración
  # ----------------------------------------
  
  # Gráfico de calibración
  png(file.path(carpeta_salida, "calibracion.png"), width = 800, height = 600)
  plot(cal_data$avg_pred, cal_data$obs_rate, 
       xlab = "Probabilidad Predicha Promedio", ylab = "Tasa Observada Real",
       main = "Calibración del Modelo por Decil",
       pch = 19, col = "blue", cex = 1.5)
  abline(a = 0, b = 1, lty = 2, col = "red")
  text(cal_data$avg_pred, cal_data$obs_rate + 0.02, labels = cal_data$decil)
  dev.off()
  
  # 9. Análisis de puntos de corte
  cat("Analizando diferentes puntos de corte...\n")
  # Crear tabla de efectividad con diferentes puntos de corte
  cortes <- seq(0.3, 0.7, by = 0.05)
  resultados_cortes <- data.frame()
  
  for(corte in cortes) {
    datos$pred_corte <- as.factor(ifelse(datos$preds >= corte, 1, 0))
    cm <- confusionMatrix(datos$pred_corte, datos$Malo_A, positive = "1")
    
    temp <- data.frame(
      punto_corte = corte,
      accuracy = cm$overall["Accuracy"],
      sensitivity = cm$byClass["Sensitivity"],
      specificity = cm$byClass["Specificity"],
      precision = cm$byClass["Pos Pred Value"],
      f1_score = cm$byClass["F1"],
      approval_rate = mean(datos$preds < corte),
      bad_rate_approved = sum(as.numeric(as.character(datos$Malo_A)) == 1 & 
                                datos$preds < corte) / sum(datos$preds < corte)
    )
    
    resultados_cortes <- rbind(resultados_cortes, temp)
  }
  
  # Guardar tabla de resultados por punto de corte
  write.csv(resultados_cortes, file.path(carpeta_salida, "analisis_puntos_corte.csv"), row.names = FALSE)
  gc()
  # ----------------------------------------
  # Gráfico de Métricas por Punto de Corte
  # ----------------------------------------
  
  # Gráfico de métricas por punto de corte
  png(file.path(carpeta_salida, "metricas_por_punto_corte.png"), width = 800, height = 600)
  matplot(cortes, resultados_cortes[, c("accuracy", "sensitivity", "specificity", "f1_score")],
          type = "l", lty = 1, lwd = 2, 
          col = c("black", "red", "green", "blue"),
          xlab = "Punto de Corte", ylab = "Valor",
          main = "Métricas por Punto de Corte")
  legend("topleft", 
         legend = c("Accuracy", "Sensitivity", "Specificity", "F1 Score"),
         col = c("black", "red", "green", "blue"), 
         lty = 1, lwd = 2)
  dev.off()
  gc()
  # ----------------------------------------
  # Gráfico de Tasas de Aprobación
  # ----------------------------------------
  
  # Gráfico de tasas de aprobación y bad rate de aprobados
  png(file.path(carpeta_salida, "tasas_aprobacion.png"), width = 800, height = 600)
  par(mar = c(5, 4, 4, 4) + 0.1)
  plot(cortes, resultados_cortes$approval_rate, 
       type = "l", col = "blue", lwd = 2,
       xlab = "Punto de Corte", ylab = "Tasa de Aprobación",
       main = "Impacto del Punto de Corte en Aprobaciones")
  par(new = TRUE)
  plot(cortes, resultados_cortes$bad_rate_approved, 
       type = "l", col = "red", lwd = 2,
       xlab = "", ylab = "", axes = FALSE)
  axis(side = 4)
  mtext("Bad Rate en Aprobados", side = 4, line = 2)
  legend("topright", 
         legend = c("Tasa de Aprobación", "Bad Rate en Aprobados"),
         col = c("blue", "red"), 
         lty = 1, lwd = 2)
  dev.off()
  gc()
  # 10. Generar gráficos y análisis adicionales
  # --- NUEVOS ELEMENTOS ---
  
  # Generar gráfico Gini (basado en la curva de Lorenz)
  png(file.path(carpeta_salida, "grafico_gini.png"), width = 800, height = 600)
  # Crear estructura para la curva de Lorenz
  lorenz_data <- ks_data %>%
    arrange(pred) %>%
    mutate(
      cum_pop_pct = row_number() / n(),
      cum_target_pct = cumsum(target) / sum(target)
    )
  
  # Gráfico Gini/Lorenz
  plot(lorenz_data$cum_pop_pct, lorenz_data$cum_target_pct, 
       type = "l", col = "blue", lwd = 2,
       xlab = "% Acumulado de Población", 
       ylab = "% Acumulado de Malos",
       main = paste("Curva de Lorenz (Gini =", round(gini_valor, 4), ")"))
  abline(a = 0, b = 1, lty = 2, col = "gray")
  # Añadir área sombreada para visualizar Gini
  polygon(
    c(0, lorenz_data$cum_pop_pct, 1),
    c(0, lorenz_data$cum_target_pct, 0),
    col = rgb(0, 0, 1, 0.2)
  )
  text(0.7, 0.3, paste("Gini =", round(gini_valor, 4)), col = "darkblue", cex = 1.2)
  dev.off()
  
  # Generar tabla de contingencia más visual
  png(file.path(carpeta_salida, "tabla_contingencia.png"), width = 800, height = 600)
  # Extraer valores
  VP <- conf_matrix$table[2, 2]  # Verdaderos Positivos
  FP <- conf_matrix$table[1, 2]  # Falsos Positivos
  VN <- conf_matrix$table[1, 1]  # Verdaderos Negativos
  FN <- conf_matrix$table[2, 1]  # Falsos Negativos
  total <- sum(conf_matrix$table)
  
  # Crear layout para la visualización
  layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
  
  # Título
  par(mar = c(0,0,2,0))
  plot.new()
  title(main = "Matriz de Confusión", line = 0)
  
  # Matriz de confusión
  par(mar = c(5,5,2,1))
  plot.new()
  x_range <- c(0, 100)
  y_range <- c(0, 100)
  plot.window(xlim = x_range, ylim = y_range)
  
  # Dibujar recuadros
  rect(0, 50, 50, 100, col = rgb(0, 0.8, 0, 0.5))  # VN
  rect(0, 0, 50, 50, col = rgb(1, 0, 0, 0.5))      # FN
  rect(50, 0, 100, 50, col = rgb(0, 0.8, 0, 0.5))  # VP
  rect(50, 50, 100, 100, col = rgb(1, 0, 0, 0.5))  # FP
  
  # Añadir etiquetas
  text(25, 75, paste("VN:", VN, "\n", round(VN/total*100, 1), "%"), cex = 1.5)
  text(25, 25, paste("FN:", FN, "\n", round(FN/total*100, 1), "%"), cex = 1.5)
  text(75, 25, paste("VP:", VP, "\n", round(VP/total*100, 1), "%"), cex = 1.5)
  text(75, 75, paste("FP:", FP, "\n", round(FP/total*100, 1), "%"), cex = 1.5)
  
  # Añadir ejes
  axis(1, at = c(0, 50, 100), labels = c("", "Predicción", ""), tick = FALSE)
  axis(2, at = c(0, 50, 100), labels = c("", "Real", ""), tick = FALSE)
  text(25, 105, "Negativo")
  text(75, 105, "Positivo")
  text(-5, 25, "Positivo", srt = 90)
  text(-5, 75, "Negativo", srt = 90)
  
  # Métricas derivadas
  par(mar = c(5,5,2,1))
  plot.new()
  plot.window(xlim = c(0, 100), ylim = c(0, 100))
  text(50, 90, "Métricas Derivadas", cex = 1.5, font = 2)
  text(50, 75, paste("Accuracy:", round(conf_matrix$overall["Accuracy"]*100, 1), "%"), cex = 1.2, adj = 0.5)
  text(50, 65, paste("Sensibilidad:", round(conf_matrix$byClass["Sensitivity"]*100, 1), "%"), cex = 1.2, adj = 0.5)
  text(50, 55, paste("Especificidad:", round(conf_matrix$byClass["Specificity"]*100, 1), "%"), cex = 1.2, adj = 0.5)
  text(50, 45, paste("Precisión:", round(conf_matrix$byClass["Pos Pred Value"]*100, 1), "%"), cex = 1.2, adj = 0.5)
  text(50, 35, paste("F1 Score:", round(conf_matrix$byClass["F1"]*100, 1), "%"), cex = 1.2, adj = 0.5)
  text(50, 25, paste("Punto de Corte:", round(ks_cutoff, 4)), cex = 1.2, adj = 0.5)
  text(50, 15, paste("Clasificados Como Riesgo:", round((FP+VP)/total*100, 1), "%"), cex = 1.2, adj = 0.5)
  
  dev.off()
  gc()
  # Generar interpretaciones automatizadas para los gráficos
  
  # Interpretación para Gini
  interpretacion_gini <- interpretar_gini(gini_valor)
  
  # Interpretación para la tabla de contingencia
  interpretacion_tabla_contingencia <- interpretar_tabla_contingencia(conf_matrix)
  
  # Interpretación para la distribución de predicciones
  interpretacion_distribucion <- interpretar_distribucion(datos)
  
  # Interpretación para la curva ROC
  interpretacion_curva_roc <- interpretar_curva_roc(auc_valor)
  
  # Interpretación para KS
  interpretacion_ks <- interpretar_ks(ks_valor, ks_cutoff)
  
  # Interpretación para tasa de malos por decil
  interpretacion_tasa_malos <- interpretar_tasa_malos_decil(stats_deciles)
  
  # Interpretación para métricas por punto de corte
  interpretacion_metricas_corte <- interpretar_metricas_punto_corte(resultados_cortes, ks_cutoff)
  
  # Interpretación para tasas de aprobación
  interpretacion_tasas_aprobacion <- interpretar_tasas_aprobacion(resultados_cortes)
  
  # Interpretación para estadísticas por decil
  interpretacion_stats_deciles <- interpretar_estadisticas_decil(stats_deciles)
  
  # Interpretación para diferentes puntos de corte
  interpretacion_diferentes_puntos <- interpretar_diferentes_puntos_corte(resultados_cortes) 
  
  # Interpretación para boxplot por grupo
  interpretacion_boxplot <- interpretar_boxplot(datos)
  
  # Interpretación para curva de ganancia
  interpretacion_ganancia <- interpretar_ganancia(stats_deciles)
  
  # Interpretación para calibración
  interpretacion_calibracion <- interpretar_calibracion(cal_data, max_desviacion_cal)
  
  # 11. Generar reporte HTML
  cat("Generando reporte HTML...\n")
  gc()
  # Determinar niveles de alerta basados en métricas
  nivel_auc <- ifelse(auc_valor >= 0.75, "bajo", 
                      ifelse(auc_valor >= 0.65, "medio", "alto"))
  
  nivel_ks <- ifelse(ks_valor >= 0.3, "bajo", 
                     ifelse(ks_valor >= 0.2, "medio", "alto"))
  
  nivel_iv <- ifelse(iv_total >= 0.5, "alto", 
                     ifelse(iv_total >= 0.3, "bajo", 
                            ifelse(iv_total >= 0.1, "medio", "alto")))
  
  diferencia_sens_spec <- abs(conf_matrix$byClass["Sensitivity"] - conf_matrix$byClass["Specificity"])
  nivel_balance <- ifelse(diferencia_sens_spec <= 0.1, "bajo",
                          ifelse(diferencia_sens_spec <= 0.2, "medio", "alto"))
  
  nivel_calibracion <- ifelse(max_desviacion_cal <= 0.05, "bajo",
                              ifelse(max_desviacion_cal <= 0.1, "medio", "alto"))
  gc()
  # Crear mensajes de alerta
  mensaje_auc <- ifelse(nivel_auc == "alto",
                        paste("AUC bajo (", round(auc_valor, 4), "). El modelo tiene poder discriminativo limitado."),
                        ifelse(nivel_auc == "medio",
                               paste("AUC moderado (", round(auc_valor, 4), "). El modelo tiene poder discriminativo aceptable."),
                               paste("AUC bueno (", round(auc_valor, 4), "). El modelo tiene buen poder discriminativo.")))
  
  mensaje_ks <- ifelse(nivel_ks == "alto",
                       paste("KS bajo (", round(ks_valor, 4), "). Separación insuficiente entre buenos y malos clientes."),
                       ifelse(nivel_ks == "medio",
                              paste("KS moderado (", round(ks_valor, 4), "). Separación moderada entre buenos y malos clientes."),
                              paste("KS bueno (", round(ks_valor, 4), "). Buena separación entre buenos y malos clientes.")))
  
  mensaje_iv <- ifelse(nivel_iv == "alto" && iv_total >= 0.6,
                       paste("IV muy alto (", round(iv_total, 4), "). Posible sobreajuste del modelo."),
                       ifelse(nivel_iv == "alto",
                              paste("IV alto (", round(iv_total, 4), "). Muy buen poder predictivo, verificar si hay sobreajuste."),
                              ifelse(nivel_iv == "bajo",
                                     paste("IV bueno (", round(iv_total, 4), "). Buen poder predictivo."),
                                     paste("IV moderado (", round(iv_total, 4), "). Poder predictivo moderado."))))
  
  mensaje_balance <- ifelse(nivel_balance == "alto",
                            paste("Desbalance significativo entre sensibilidad (", 
                                  round(conf_matrix$byClass["Sensitivity"], 4), ") y especificidad (", 
                                  round(conf_matrix$byClass["Specificity"], 4), "). Considerar ajustar el punto de corte."),
                            ifelse(nivel_balance == "medio",
                                   paste("Diferencia moderada entre sensibilidad (", 
                                         round(conf_matrix$byClass["Sensitivity"], 4), ") y especificidad (", 
                                         round(conf_matrix$byClass["Specificity"], 4), ")."),
                                   paste("Buen balance entre sensibilidad (", 
                                         round(conf_matrix$byClass["Sensitivity"], 4), ") y especificidad (", 
                                         round(conf_matrix$byClass["Specificity"], 4), ").")))
  mensaje_calibracion <- ifelse(nivel_calibracion == "alto",
                                paste("Problemas de calibración. Desviación máxima de ", 
                                      round(max_desviacion_cal * 100, 1), 
                                      "% entre probabilidades predichas y observadas."),
                                ifelse(nivel_calibracion == "medio",
                                       paste("Calibración moderada. Desviación máxima de ", 
                                             round(max_desviacion_cal * 100, 1), 
                                             "% entre probabilidades predichas y observadas."),
                                       paste("Buena calibración. Desviación máxima de ", 
                                             round(max_desviacion_cal * 100, 1), 
                                             "% entre probabilidades predichas y observadas.")))
  gc()
  # Generar comentarios estratégicos
  comentario_desempeno <- ""
  if (auc_valor > 0.75 && ks_valor > 0.3) {
    comentario_desempeno <- "El modelo muestra un desempeño fuerte con buena capacidad discriminativa. Es adecuado para aplicaciones de decisión crediticia."
  } else if (auc_valor > 0.65 && ks_valor > 0.2) {
    comentario_desempeno <- "El modelo muestra un desempeño moderado con capacidad discriminativa aceptable. Puede ser utilizado para decisiones crediticias con monitoreo continuo."
  } else {
    comentario_desempeno <- "El modelo muestra un desempeño limitado. Se recomienda revisión o recalibración antes de usar para decisiones crediticias importantes."
  }
  
  comentario_balance <- ""
  if (conf_matrix$byClass["Sensitivity"] > 0.7 && conf_matrix$byClass["Specificity"] < 0.6) {
    comentario_balance <- "El modelo está optimizado para detectar clientes de alto riesgo (alta sensibilidad) a costa de rechazar algunos buenos clientes. Esto es apropiado en contextos de alto riesgo o exposiciones grandes."
  } else if (conf_matrix$byClass["Sensitivity"] < 0.6 && conf_matrix$byClass["Specificity"] > 0.7) {
    comentario_balance <- "El modelo está optimizado para aprobar buenos clientes (alta especificidad) a costa de aceptar algunos clientes de alto riesgo. Esto puede ser apropiado en contextos de bajo riesgo o para productos con alta rentabilidad."
  } else if (conf_matrix$byClass["Sensitivity"] > 0.65 && conf_matrix$byClass["Specificity"] > 0.65) {
    comentario_balance <- "El modelo muestra un buen balance entre detectar clientes de alto riesgo y aprobar buenos clientes. Es adecuado para una estrategia equilibrada de riesgo-rentabilidad."
  } else {
    comentario_balance <- "El modelo muestra limitaciones tanto en detectar clientes de alto riesgo como en identificar buenos clientes. Se recomienda revisión de variables o técnicas de modelado."
  }
  
  comentario_punto_corte <- paste0(
    "El punto de corte óptimo según KS es ", round(ks_cutoff, 4), 
    ". Sin embargo, dependiendo de los objetivos de negocio, se podría considerar:",
    "<br>- Un punto de corte más alto para reducir el riesgo (mayor capacidad para identificar clientes de alto riesgo, menor tasa de aprobación)",
    "<br>- Un punto de corte más bajo para aumentar volumen (mayor tasa de aprobación a costa de incluir algunos clientes con mayor riesgo)"
  )
  
  max_ratio_deciles <- stats_deciles$tasa_malos[1] / stats_deciles$tasa_malos[10]
  comentario_segmentacion <- ""
  if (max_ratio_deciles > 2.5) {
    comentario_segmentacion <- paste0(
      "El ratio entre la tasa de malos del decil de mayor riesgo y el de menor riesgo es ", 
      round(max_ratio_deciles, 2), 
      "x. Se recomienda implementar estrategias diferenciadas por nivel de riesgo:",
      "<br>- Deciles 1-3 (mayor riesgo): Mayor supervisión, garantías adicionales o tasas más altas para compensar el riesgo",
      "<br>- Deciles 4-7 (riesgo medio): Condiciones estándar",
      "<br>- Deciles 8-10 (menor riesgo): Posibles beneficios, tasas preferenciales o mayor línea de crédito"
    )
  } else {
    comentario_segmentacion <- paste0(
      "El ratio entre la tasa de malos del decil de mayor riesgo y el de menor riesgo es sólo ", 
      round(max_ratio_deciles, 2), 
      "x. La diferenciación entre segmentos de riesgo es limitada, se recomienda revisar la capacidad predictiva de las variables para mejorar la separación entre clientes de alto y bajo riesgo."
    )
  }
  
  comentario_sobreajuste <- ""
  if (iv_total > 0.6) {
    comentario_sobreajuste <- paste0(
      "El valor de IV (", round(iv_total, 4), 
      ") es muy alto, lo que sugiere posible sobreajuste. Se recomienda:",
      "<br>- Validar el modelo con datos más recientes o de otra población",
      "<br>- Evaluar la simplificación del modelo reduciendo el número de variables",
      "<br>- Aplicar técnicas de regularización para controlar el sobreajuste",
      "<br>- Verificar si el modelo mantiene su capacidad predictiva en diferentes segmentos"
    )
  } else if (iv_total < 0.1) {
    comentario_sobreajuste <- paste0(
      "El valor de IV (", round(iv_total, 4), 
      ") es bajo, lo que indica una capacidad predictiva limitada. Se recomienda:",
      "<br>- Explorar variables adicionales que puedan capturar mejor el riesgo",
      "<br>- Considerar técnicas de modelado más avanzadas",
      "<br>- Evaluar si existen segmentos donde el modelo funcione mejor"
    )
  }
  gc()
  # Generar el HTML
  html_content <- paste0('
<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>Reporte de Backtesting del Modelo de Originacion</title>
  <style>
    body {
      font-family: Arial, sans-serif;
      line-height: 1.6;
      margin: 0;
      padding: 20px;
      color: #333;
      text-align: center;
    }
    h1, h2, h3, h4 {
      color: #2c3e50;
    }
    .container {
      max-width: 1200px;
      margin: 0 auto;
      text-align: left;
    }
    /* Contenedor que agrupa el encabezado y el primer contenido para evitar saltos de página entre ellos */
    .header-container {
      page-break-inside: avoid;
      break-inside: avoid;
    }
    .header-title {
      margin-bottom: 20px;
      /* Aseguramos que no se forcen saltos después del encabezado */
      page-break-after: avoid;
      break-after: avoid;
    }
    .section {
      margin-bottom: 30px;
      padding: 20px;
      background: white;
      border-radius: 5px;
      box-shadow: 0 2px 5px rgba(0,0,0,0.1);
    }
    /* Evitar saltos de página al imprimir en secciones */
    @media print {
      .section {
        page-break-inside: avoid;
        break-inside: avoid;
        orphans: 3;
        widows: 3;
      }
    }
    .dashboard {
      display: flex;
      flex-wrap: wrap;
      gap: 15px;
      margin-bottom: 20px;
    }
    .metric-box {
      flex: 1;
      min-width: 150px;
      padding: 10px;
      background-color: #f8f9fa;
      border-radius: 5px;
      box-shadow: 0 2px 5px rgba(0,0,0,0.1);
      text-align: center;
    }
    .metric-value {
      font-size: 24px;
      font-weight: bold;
      margin: 10px 0;
    }
    .metric-label {
      font-size: 14px;
      color: #666;
    }
    .alert-alto {
      color: white;
      background-color: #d9534f;
      border-color: #d43f3a;
      padding: 10px;
      border-radius: 5px;
      margin-bottom: 10px;
    }
    .alert-medio {
      color: #8a6d3b;
      background-color: #fcf8e3;
      border-color: #faebcc;
      padding: 10px;
      border-radius: 5px;
      margin-bottom: 10px;
    }
    .alert-bajo {
      color: #3c763d;
      background-color: #dff0d8;
      border-color: #d6e9c6;
      padding: 10px;
      border-radius: 5px;
      margin-bottom: 10px;
    }
    .chart-container {
      margin: 20px 0;
      text-align: center;
    }
    .chart {
      max-width: 100%;
      height: auto;
      border: 1px solid #ddd;
      border-radius: 5px;
    }
    .chart-explanation {
      background-color: #f9f9f9;
      border-left: 4px solid #007bff;
      padding: 10px 15px;
      margin: 15px 0;
      font-size: 0.9em;
    }
    .chart-interpretation {
      background-color: #e8f4ff;
      border-left: 4px solid #4a86e8;
      padding: 10px 15px;
      margin: 15px 0;
      font-size: 0.95em;
    }
    /* Nueva clase para agrupar el título y el contenido del gráfico */
    .chart-group {
      page-break-inside: avoid;
      break-inside: avoid;
      display: flex;
      flex-direction: column;
      align-items: center;
    }
    table {
      width: 100%;
      border-collapse: collapse;
      margin: 20px 0;
    }
    th, td {
      padding: 12px 15px;
      text-align: left;
      border-bottom: 1px solid #ddd;
    }
    th {
      background-color: #f2f2f2;
    }
    tr:hover {
      background-color: #f5f5f5;
    }
    .footer {
      margin-top: 40px;
      text-align: center;
      color: #777;
      font-size: 14px;
    }
  </style>
</head>
<body>
  <div class="container">
      <!-- Primer contenido: Resumen Ejecutivo -->
      <div class="section">
        <!-- Agrupamos el encabezado y el primer bloque para que se impriman juntos -->
        <div class="header-container">
          <div class="header-title">
            <h1>Reporte de Backtesting del Modelo de Originacion</h1>
            <p>Fecha de análisis: ', format(Sys.Date(), "%d de %B de %Y"), '</p>
            <p>Linea de negocio: T-Conecta</p>
          </div>
        <h2>Resumen Ejecutivo</h2>
        
        <div class="dashboard">
          <div class="metric-box" style="background-color: ', 
                         ifelse(nivel_auc == "bajo", "#dff0d8", 
                                ifelse(nivel_auc == "medio", "#fcf8e3", "#f2dede")), ';">
            <div class="metric-label">AUC</div>
            <div class="metric-value">', round(auc_valor * 100, 1), '%</div>
            <div>Discriminación</div>
          </div>
          
          <div class="metric-box" style="background-color: ', 
                         ifelse(nivel_ks == "bajo", "#dff0d8", 
                                ifelse(nivel_ks == "medio", "#fcf8e3", "#f2dede")), ';">
            <div class="metric-label">KS</div>
            <div class="metric-value">', round(ks_valor * 100, 1), '%</div>
            <div>Separación</div>
          </div>
          
          <div class="metric-box">
            <div class="metric-label">Accuracy</div>
            <div class="metric-value">', round(conf_matrix$overall["Accuracy"] * 100, 1), '%</div>
            <div>Precisión Global</div>
          </div>
          
          <div class="metric-box">
            <div class="metric-label">Punto de Corte</div>
            <div class="metric-value">', round(ks_cutoff, 3), '</div>
            <div>Óptimo (KS)</div>
          </div>
          
          <div class="metric-box" style="background-color: ', 
                         ifelse(gini_valor >= 0.6, "#dff0d8", 
                                ifelse(gini_valor >= 0.4, "#fcf8e3", "#f2dede")), ';">
            <div class="metric-label">Gini</div>
            <div class="metric-value">', round(gini_valor * 100, 1), '%</div>
            <div>Discriminación</div>
          </div>
        </div>
        
        <h3>Alertas</h3>
        <div class="alert-', nivel_auc, '">', mensaje_auc, '</div>
        <div class="alert-', nivel_ks, '">', mensaje_ks, '</div>
        <div class="alert-', nivel_iv, '">', mensaje_iv, '</div>
        <div class="alert-', nivel_balance, '">', mensaje_balance, '</div>
        <div class="alert-', nivel_calibracion, '">', mensaje_calibracion, '</div>
        
        <h3>Evaluación Estratégica</h3>
        <h4>Desempeño General</h4>
        <p>', comentario_desempeno, '</p>
        
        <h4>Balance del Modelo</h4>
        <p>', comentario_balance, '</p>
        
        <h4>Recomendaciones sobre Punto de Corte</h4>
        <p>', comentario_punto_corte, '</p>
        
        <h4>Recomendaciones sobre Segmentación</h4>
        <p>', comentario_segmentacion, '</p>
        
        ', ifelse(iv_total > 0.6 || iv_total < 0.1, 
                  paste0('<h4>Consideraciones sobre Poder Predictivo</h4><p>', comentario_sobreajuste, '</p>'), 
                  ''), '
      </div>
    </div>
    
    <!-- Resto del contenido -->
    <div class="section">
      <div class="row">
        <div class="chart-group">
          <div class="container">
            <div class="header-title">
              <h1>Métricas Detalladas</h1>
            </div>
          <h3>Curva ROC y Capacidad Discriminativa</h3>
          <div class="chart-container">
            <img class="chart" src="curva_roc.png" alt="Curva ROC">
            <div class="chart-explanation">
              <strong>Cómo interpretar:</strong> La curva ROC muestra la relación entre la Tasa de Verdaderos Positivos y la Tasa de Falsos Positivos para distintos puntos de corte. Un AUC mayor indica mejor discriminación.
            </div>
            <div class="chart-interpretation">
              <strong>Interpretación para este modelo:</strong> ', interpretacion_curva_roc, '
            </div>
          </div>
        </div>
        
        <div class="chart-group">
          <h3>Gráfico KS (Máxima Separación)</h3>
          <div class="chart-container">
            <img class="chart" src="grafico_ks.png" alt="Gráfico KS">
            <div class="chart-explanation">
              <strong>Cómo interpretar:</strong> El gráfico KS muestra las distribuciones acumulativas de clientes buenos y malos. La separación máxima (KS) indica el punto óptimo.
            </div>
            <div class="chart-interpretation">
              <strong>Interpretación para este modelo:</strong> ', interpretacion_ks, '
            </div>
          </div>
        </div>
      </div>
      
      <div class="row">
        <div class="chart-group">
          <h3>Distribución de Predicciones</h3>
          <div class="chart-container">
            <img class="chart" src="histograma_predicciones.png" alt="Distribución de Predicciones">
            <div class="chart-explanation">
              <strong>Cómo interpretar:</strong> Este histograma muestra la distribución de probabilidades predichas. Una distribución uniforme indica buen comportamiento, mientras que picos podrían sugerir problemas.
            </div>
            <div class="chart-interpretation">
              <strong>Interpretación para este modelo:</strong> ', interpretacion_distribucion, '
            </div>
          </div>
        </div>
        
        <div class="chart-group">
          <h3>Predicciones por Grupo (Buenos vs Malos)</h3>
          <div class="chart-container">
            <img class="chart" src="boxplot_por_grupo.png" alt="Boxplot por Grupo">
            <div class="chart-explanation">
              <strong>Cómo interpretar:</strong> Compara las distribuciones de probabilidades para clientes buenos y malos. Una separación clara indica buena discriminación.
            </div>
            <div class="chart-interpretation">
              <strong>Interpretación para este modelo:</strong> ', interpretacion_boxplot, '
            </div>
          </div>
        </div>
      </div>
      
      <div class="chart-group">
        <h3>Calibración del Modelo por Decil</h3>
        <div class="chart-container">
          <img class="chart" src="calibracion.png" alt="Calibración del Modelo">
          <div class="chart-explanation">
            <strong>Cómo interpretar:</strong> Compara la probabilidad predicha con la tasa real observada para cada decil. La cercanía a la diagonal indica buena calibración.
          </div>
          <div class="chart-interpretation">
            <strong>Interpretación para este modelo:</strong> ', interpretacion_calibracion, '
          </div>
        </div>
      </div>
    </div>
    
    <div class="section">
        <div class="chart-group">
          <div class="container">
            <div class="header-title">
              <h1>Matriz de Confusión y Análisis de Clasificación</h1>
            </div>
          <h3>Matriz de Confusión</h3>
          <div class="chart-container">
            <img class="chart" src="tabla_contingencia.png" alt="Matriz de Confusión">
            <div class="chart-explanation">
              <strong>Cómo interpretar:</strong> La matriz muestra la distribución de predicciones versus valores reales. Los cuadrantes indican VN, FP, FN y VP.
            </div>
            <div class="chart-interpretation">
              <strong>Interpretación para este modelo:</strong> ', interpretacion_tabla_contingencia, '
            </div>
          </div>
        </div>
        
        <div class="chart-group">
          <h3>Coeficiente Gini y Curva de Lorenz</h3>
          <div class="chart-container">
            <img class="chart" src="grafico_gini.png" alt="Curva de Lorenz y Gini">
            <div class="chart-explanation">
              <strong>Cómo interpretar:</strong> La curva de Lorenz y el coeficiente Gini muestran la desigualdad en la distribución de predicciones. Valores cercanos a 1 indican mejor discriminación.
            </div>
            <div class="chart-interpretation">
              <strong>Interpretación para este modelo:</strong> ', interpretacion_gini, '
            </div>
          </div>
        </div>
      </div>
    </div>
    
    <div class="section">
        <div class="chart-group">
          <div class="container">
            <div class="header-title">
              <h1>Análisis por Deciles</h1>
            </div>
          <h3>Tasa de Malos por Decil</h3>
          <div class="chart-container">
            <img class="chart" src="tasa_malos_por_decil.png" alt="Tasa de Malos por Decil">
            <div class="chart-explanation">
              <strong>Cómo interpretar:</strong> Muestra la tasa de incumplimiento por cada decil de riesgo. Se espera que disminuya de decil 1 a 10.
            </div>
            <div class="chart-interpretation">
              <strong>Interpretación para este modelo:</strong> ', interpretacion_tasa_malos, '
            </div>
          </div>
        </div>
        
        <div class="chart-group">
          <h3>Curva de Ganancia Acumulada</h3>
          <div class="chart-container">
            <img class="chart" src="curva_ganancia.png" alt="Curva de Ganancia Acumulada">
            <div class="chart-explanation">
              <strong>Cómo interpretar:</strong> Esta curva muestra el porcentaje de incumplimientos capturados al seleccionar un porcentaje de la población ordenada por riesgo.
            </div>
            <div class="chart-interpretation">
              <strong>Interpretación para este modelo:</strong> ', interpretacion_ganancia, '
            </div>
          </div>
        </div>
        
        <h3>Estadísticas por Decil</h3>
        <p>Se ha generado un archivo CSV con las estadísticas detalladas por decil (estadisticas_deciles.csv)</p>
        <div class="chart-interpretation">
          <strong>Interpretación de las estadísticas:</strong> ', interpretacion_stats_deciles, '
        </div>
        <table>
          <tr>
            <th>Decil</th>
            <th>N</th>
            <th>N Malos</th>
            <th>Tasa Malos</th>
            <th>Min Pred</th>
            <th>Max Pred</th>
            <th>Avg Pred</th>
          </tr>
          ', paste(apply(as.data.frame(stats_deciles), 1, function(row) {
            paste0(
              "<tr>",
              "<td>", row["decil"], "</td>",
              "<td>", row["n"], "</td>",
              "<td>", row["n_malos"], "</td>",
              "<td>", sprintf("%.2f%%", 100*as.numeric(row["tasa_malos"])), "</td>",
              "<td>", sprintf("%.4f", as.numeric(row["min_pred"])), "</td>",
              "<td>", sprintf("%.4f", as.numeric(row["max_pred"])), "</td>",
              "<td>", sprintf("%.4f", as.numeric(row["avg_pred"])), "</td>",
              "</tr>"
            )
          }), collapse = "" ), '
        </table>
      </div>
    </div>
    
    <div class="section">
        <div class="chart-group">
          <div class="container">
            <div class="header-title">
              <h1>Análisis de Puntos de Corte</h1>
            </div>
          <h3>Métricas por Punto de Corte</h3>
          <div class="chart-container">
            <img class="chart" src="metricas_por_punto_corte.png" alt="Métricas por Punto de Corte">
            <div class="chart-explanation">
              <strong>Cómo interpretar:</strong> Este gráfico muestra la variación de accuracy, sensitivity, specificity y F1 score al cambiar el punto de corte.
            </div>
            <div class="chart-interpretation">
              <strong>Interpretación para este modelo:</strong> ', interpretacion_metricas_corte, '
            </div>
          </div>
        </div>
        
        <div class="chart-group">
          <h3>Impacto en Tasa de Aprobación y Bad Rate</h3>
          <div class="chart-container">
            <img class="chart" src="tasas_aprobacion.png" alt="Tasas de Aprobación">
            <div class="chart-explanation">
              <strong>Cómo interpretar:</strong> Muestra la relación entre el punto de corte, la tasa de aprobación y el bad rate en los aprobados.
            </div>
            <div class="chart-interpretation">
              <strong>Interpretación para este modelo:</strong> ', interpretacion_tasas_aprobacion, '
            </div>
          </div>
        </div>
        
        <h3>Evaluación de Diferentes Puntos de Corte</h3>
        <p>Se ha generado un archivo CSV con los resultados detallados para diferentes puntos de corte (analisis_puntos_corte.csv)</p>
        <div class="chart-interpretation">
          <strong>Interpretación de los resultados:</strong> ', interpretacion_diferentes_puntos, '
        </div>
        <table>
          <tr>
            <th>Punto de Corte</th>
            <th>Accuracy</th>
            <th>Sensitivity</th>
            <th>Specificity</th>
            <th>Tasa de Aprobación</th>
            <th>Bad Rate en Aprobados</th>
          </tr>
          ', paste(apply(as.data.frame(resultados_cortes), 1, function(row) {
            paste0(
              "<tr>",
              "<td>", sprintf("%.2f", as.numeric(row["punto_corte"])), "</td>",
              "<td>", sprintf("%.2f%%", 100*as.numeric(row["accuracy"])), "</td>",
              "<td>", sprintf("%.2f%%", 100*as.numeric(row["sensitivity"])), "</td>",
              "<td>", sprintf("%.2f%%", 100*as.numeric(row["specificity"])), "</td>",
              "<td>", sprintf("%.2f%%", 100*as.numeric(row["approval_rate"])), "</td>",
              "<td>", sprintf("%.2f%%", 100*as.numeric(row["bad_rate_approved"])), "</td>",
              "</tr>"
            )
          }), collapse = "" ), '
        </table>
      </div>
    </div>
    
    <div class="section">
      <h2>Glosario y Guía de Interpretación</h2>
      <p>A continuación, se presenta una breve guía para la interpretación correcta de las métricas y gráficos incluidos en este reporte:</p>
      
      <h3>Métricas Principales</h3>
      <ul>
        <li><strong>AUC (Area Under Curve):</strong> Mide la capacidad discriminativa del modelo. Varía de 0.5 (modelo aleatorio) a 1 (modelo perfecto). AUC &gt; 0.75 se considera bueno para modelos de riesgo crediticio.</li>
        <li><strong>KS (Kolmogorov-Smirnov):</strong> Mide la máxima separación entre clientes buenos y malos. KS &gt; 0.3 indica buena separación.</li>
        <li><strong>Gini:</strong> Relacionado con el AUC (Gini = 2*AUC-1). Mide la desigualdad en la distribución de predicciones. Valores más altos indican mejor discriminación.</li>
        <li><strong>Accuracy:</strong> Porcentaje total de clasificaciones correctas. Debe interpretarse junto con sensibilidad y especificidad.</li>
        <li><strong>Sensibilidad:</strong> Porcentaje de clientes de alto riesgo correctamente identificados.</li>
        <li><strong>Especificidad:</strong> Porcentaje de clientes de bajo riesgo correctamente identificados.</li>
      </ul>
      
      <h3>Interpretación de Probabilidades</h3>
      <p>En este modelo, una mayor probabilidad indica mayor riesgo de incumplimiento:</p>
      <ul>
        <li>Probabilidades cercanas a 1: Alto riesgo de incumplimiento</li>
        <li>Probabilidades cercanas a 0: Bajo riesgo de incumplimiento</li>
      </ul>
      
      <h3>Aspectos Clave de la Evaluación</h3>
      <ul>
        <li><strong>Poder Discriminativo:</strong> Capacidad del modelo para separar clientes de alto y bajo riesgo (medido por AUC, KS, Gini).</li>
        <li><strong>Calibración:</strong> Precisión de las probabilidades predichas respecto a las tasas reales observadas.</li>
        <li><strong>Balance del Modelo:</strong> Equilibrio entre sensibilidad y especificidad.</li>
        <li><strong>Estabilidad por Segmentos:</strong> Consistencia del rendimiento en diferentes grupos o deciles.</li>
      </ul>
      
      <p>Para interpretaciones más detalladas de cada gráfico, consulte las explicaciones incluidas junto a cada visualización en este reporte.</p>
    </div>
    
    <div class="footer">
      <p>Reporte generado el ', format(Sys.Date(), "%d/%m/%Y"), '.</p>
      <p>Para consultas o análisis adicionales, contacte al equipo de Riesgo.</p>
    </div>
  </div>
</body>
</html>
')
  
  # Escribir el HTML al archivo
  writeLines(html_content, file.path(carpeta_salida, "reporte_backtesting.html"))
  
  # Crear archivo de resumen con métricas principales
  summary_content <- paste0(
    "=== RESUMEN DE MÉTRICAS DE DESEMPEÑO ===\n",
    "AUC: ", round(auc_valor, 4), "\n",
    "KS: ", round(ks_valor, 4), " (en punto de corte: ", round(ks_cutoff, 4), ")\n",
    "Gini: ", round(gini_valor, 4), "\n",
    "IV: ", round(iv_total, 4), "\n",
    "Accuracy: ", round(conf_matrix$overall["Accuracy"], 4), "\n",
    "Sensitivity: ", round(conf_matrix$byClass["Sensitivity"], 4), "\n",
    "Specificity: ", round(conf_matrix$byClass["Specificity"], 4), "\n",
    "Ratio de tasa de malos decil 1 / decil 10: ", round(max_ratio_deciles, 2), "\n",
    "Máxima desviación en calibración: ", round(max_desviacion_cal * 100, 2), "%\n"
  )
  
  writeLines(summary_content, file.path(carpeta_salida, "metricas_resumen.txt"))
  
  # Crear un glosario de términos para ayudar a la interpretación
  glosario_content <- paste0(
    "=== GLOSARIO DE TÉRMINOS Y MÉTRICAS ===\n\n",
    "AUC (Area Under Curve): Mide la capacidad discriminativa del modelo. Valores cercanos a 1 indican mejor discriminación. AUC > 0.75 generalmente se considera bueno.\n\n",
    "KS (Kolmogorov-Smirnov): Mide la máxima separación entre las distribuciones de buenos y malos clientes. Valores más altos indican mejor separación. KS > 0.3 se considera bueno.\n\n",
    "Gini: Coeficiente que mide la desigualdad en la distribución de predicciones. Se relaciona con el AUC mediante la fórmula Gini = 2*AUC-1. Valores más altos indican mejor discriminación.\n\n",
    "Punto de Corte: Umbral de probabilidad a partir del cual un cliente se clasifica como de alto riesgo. Clientes con probabilidad igual o mayor se consideran de alto riesgo.\n\n",
    "Sensibilidad: Porcentaje de clientes malos correctamente identificados como de alto riesgo. También llamada tasa de verdaderos positivos.\n\n",
    "Especificidad: Porcentaje de clientes buenos correctamente identificados como de bajo riesgo. También llamada tasa de verdaderos negativos.\n\n",
    "Accuracy: Porcentaje total de clasificaciones correctas (tanto buenos como malos).\n\n",
    "Tasa de Aprobación: Porcentaje de clientes que serían aprobados con un determinado punto de corte.\n\n",
    "Bad Rate: Porcentaje de clientes malos dentro de un grupo determinado. El bad rate en aprobados es crítico para la gestión de riesgo.\n\n",
    "IV (Information Value): Mide el poder predictivo global del modelo. Valores entre 0.3 y 0.5 se consideran buenos, pero valores muy altos (>0.6) pueden indicar sobreajuste.\n\n",
    "Calibración: Grado de coincidencia entre las probabilidades predichas y las tasas de incumplimiento observadas.\n\n",
    "Deciles: División de la población en 10 grupos según su nivel de riesgo predicho (decil 1 = mayor riesgo, decil 10 = menor riesgo).\n\n",
    "Matriz de Confusión: Tabla que muestra las predicciones correctas e incorrectas del modelo.\n",
    " - Verdaderos Positivos (VP): Clientes clasificados como malos que realmente son malos.\n",
    " - Falsos Positivos (FP): Clientes clasificados como malos que realmente son buenos.\n",
    " - Verdaderos Negativos (VN): Clientes clasificados como buenos que realmente son buenos.\n",
    " - Falsos Negativos (FN): Clientes clasificados como buenos que realmente son malos.\n\n",
    "F1 Score: Media armónica entre la precisión y la sensibilidad. Útil cuando se busca un balance entre ambas métricas.\n\n",
    "Precisión (Precision): Porcentaje de clientes correctamente clasificados como malos entre todos los clasificados como malos.\n\n"
  )
  
  writeLines(glosario_content, file.path(carpeta_salida, "glosario_terminos.txt"))
  
  cat("\nReporte generado exitosamente en la carpeta:", carpeta_salida, "\n")
  cat("Abre el archivo 'reporte_backtesting.html' para ver los resultados.\n")
  
  return(list(
    status = "success",
    carpeta = carpeta_salida,
    metricas = list(
      auc = auc_valor,
      ks = ks_valor,
      gini = gini_valor,
      cutoff = ks_cutoff,
      iv = iv_total,
      accuracy = conf_matrix$overall["Accuracy"],
      sensitivity = conf_matrix$byClass["Sensitivity"],
      specificity = conf_matrix$byClass["Specificity"],
      max_ratio_deciles = max_ratio_deciles,
      max_calibration_deviation = max_desviacion_cal
    )
  ))
}