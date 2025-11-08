# ============================================================================
# ANÁLISIS ESPACIAL BAYESIANO JERÁRQUICO ESPACIO-TEMPORAL
# Eventos Climáticos Extremos y Agricultura - Sur del Perú
# CON AUTOCORRELACIÓN ESPACIAL, HOTSPOTS Y PANEL DATA 2023-2024
# VERSIÓN CORREGIDA - COMPLETAMENTE FUNCIONAL
# ============================================================================

# Cargar librerías
library(haven)
library(dplyr)
library(ggplot2)
library(brms)
library(bayesplot)
library(tidyr)
library(viridis)
library(gridExtra)
library(RColorBrewer)

# LIBRERÍAS ESPACIALES
library(spdep)      # Análisis espacial
library(sf)         # Geometrías espaciales
library(leaflet)    # Mapas interactivos
library(leaflet.extras)  # Extensiones leaflet
library(htmlwidgets)     # Guardar mapas HTML
library(CARBayes)   # Modelos CAR alternativos

# RESOLVER CONFLICTOS DE NOMBRES (CARBayes carga MASS que sobreescribe select)
select <- dplyr::select
filter <- dplyr::filter
summarise <- dplyr::summarise
mutate <- dplyr::mutate

cat("=== LIBRERÍAS CARGADAS (incluidas espaciales y temporales) ===\n\n")

# ============================================================================
# PASO 1: CARGA DE ARCHIVOS
# ============================================================================

cargar_archivos_ena <- function(año = 2024) {
  
  cat("=== CARGANDO ARCHIVOS ENA", año, "===\n")
  
  archivos <- c("CARATULA.sav", "CAP200AB.sav", "CAP200B_1.sav", "CAP300AB.sav")
  
  for(archivo in archivos) {
    ruta <- paste0("ENA_", año, "/", archivo)
    if(!file.exists(ruta)) {
      stop("Archivo no encontrado: ", ruta)
    }
  }
  
  caratula <- read_sav(paste0("ENA_", año, "/CARATULA.sav"))
  produccion <- read_sav(paste0("ENA_", año, "/CAP200AB.sav"))
  perdidas <- read_sav(paste0("ENA_", año, "/CAP200B_1.sav"))
  practicas <- read_sav(paste0("ENA_", año, "/CAP300AB.sav"))
  
  cat("Archivos cargados:\n")
  cat("- Carátula:", nrow(caratula), "registros\n")
  cat("- Producción:", nrow(produccion), "registros\n")
  cat("- Pérdidas:", nrow(perdidas), "registros\n")
  cat("- Prácticas:", nrow(practicas), "registros\n\n")
  
  return(list(
    caratula = caratula,
    produccion = produccion,
    perdidas = perdidas,
    practicas = practicas,
    año = año
  ))
}

# ============================================================================
# PASO 2: CONSOLIDACIÓN DE DATOS
# ============================================================================

consolidar_datos <- function(archivos_ena) {
  
  cat("=== CONSOLIDANDO DATOS PARA AÑO", archivos_ena$año, "===\n")
  
  produccion <- archivos_ena$produccion
  perdidas <- archivos_ena$perdidas
  practicas <- archivos_ena$practicas
  año <- archivos_ena$año
  
  regiones_sur <- c("PUNO", "CUSCO", "AREQUIPA", "TACNA", "MOQUEGUA")
  cultivos_interes <- c("PAPA BLANCA", "MAIZ AMILACEO")
  
  prod_filtrada <- produccion %>%
    filter(NOMBREDD %in% regiones_sur,
           P204_NOM %in% cultivos_interes,
           !is.na(P217_SUP_ha), P217_SUP_ha > 0,
           !is.na(P219_EQUIV_KG), P219_EQUIV_KG > 0) %>%
    mutate(
      clave_productor = paste(ANIO, CCDD, CCPP, CCDI, ID_PROD, UA, sep="_"),
      rendimiento = P219_EQUIV_KG / P217_SUP_ha,
      año = año
    ) %>%
    filter(rendimiento > 100, rendimiento < 100000)
  
  perdidas_unicas <- perdidas %>%
    mutate(clave_productor = paste(ANIO, CCDD, CCPP, CCDI, ID_PROD, UA, sep="_")) %>%
    group_by(clave_productor) %>%
    summarise(
      heladas = max(P224E_4, na.rm = TRUE),
      sequias = max(P224E_6, na.rm = TRUE),
      granizadas = max(P224E_5, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      heladas = ifelse(heladas == 1, 1, 0),
      sequias = ifelse(sequias == 1, 1, 0),
      granizadas = ifelse(granizadas == 1, 1, 0)
    )
  
  practicas_unicas <- practicas %>%
    mutate(clave_productor = paste(ANIO, CCDD, CCPP, CCDI, ID_PROD, UA, sep="_")) %>%
    group_by(clave_productor) %>%
    summarise(
      materia_organica = max(P301A_2, na.rm = TRUE),
      rotacion_cultivos = max(P301A_3, na.rm = TRUE),
      terrazas = max(P301A_4, na.rm = TRUE),
      manejo_agua = max(P301A_9, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      materia_organica = ifelse(materia_organica == 1, 1, 0),
      rotacion_cultivos = ifelse(rotacion_cultivos == 1, 1, 0),
      terrazas = ifelse(terrazas == 1, 1, 0),
      manejo_agua = ifelse(manejo_agua == 1, 1, 0)
    )
  
  datos_consolidados <- prod_filtrada %>%
    left_join(perdidas_unicas, by = "clave_productor") %>%
    left_join(practicas_unicas, by = "clave_productor") %>%
    filter(!is.na(heladas), !is.na(materia_organica)) %>%
    mutate(
      semilla_certificada = ifelse(P214 == 1, 1, 0),
      departamento = NOMBREDD,
      provincia = NOMBREPV,
      distrito = NOMBREDI,
      cultivo = P204_NOM
    ) %>%
    select(clave_productor, año, departamento, provincia, distrito, cultivo, rendimiento,
           heladas, sequias, granizadas, materia_organica, rotacion_cultivos,
           terrazas, manejo_agua, semilla_certificada)
  
  cat("Datos consolidados año", año, ":", nrow(datos_consolidados), "registros\n\n")
  
  return(datos_consolidados)
}

# ============================================================================
# PASO 3: CREAR PANEL DATA 2023-2024
# ============================================================================

crear_panel_data <- function() {
  
  cat("\n==========================================================\n")
  cat("CREANDO PANEL DATA 2023-2024\n")
  cat("==========================================================\n\n")
  
  # Cargar y consolidar 2023
  cat("--- Procesando datos 2023 ---\n")
  archivos_2023 <- cargar_archivos_ena(2023)
  datos_2023 <- consolidar_datos(archivos_2023)
  
  # Cargar y consolidar 2024
  cat("--- Procesando datos 2024 ---\n")
  archivos_2024 <- cargar_archivos_ena(2024)
  datos_2024 <- consolidar_datos(archivos_2024)
  
  # Combinar ambos años
  panel_data <- bind_rows(datos_2023, datos_2024) %>%
    mutate(
      año_factor = as.factor(año),
      año_num = año - 2022
    )
  
  cat("\n=== PANEL DATA CREADO ===\n")
  cat("Total observaciones:", nrow(panel_data), "\n")
  cat("Año 2023:", sum(panel_data$año == 2023), "obs\n")
  cat("Año 2024:", sum(panel_data$año == 2024), "obs\n")
  cat("Provincias únicas:", n_distinct(panel_data$provincia), "\n")
  cat("Departamentos únicos:", n_distinct(panel_data$departamento), "\n\n")
  
  return(list(
    panel = panel_data,
    caratula_2023 = archivos_2023$caratula,
    caratula_2024 = archivos_2024$caratula
  ))
}

# ============================================================================
# PASO 4: FILTRADO DE PROVINCIAS VIABLES
# ============================================================================

filtrar_provincias_viables <- function(panel_data) {
  
  cat("=== FILTRANDO PROVINCIAS VIABLES (PANEL 2023-2024) ===\n")
  
  # Contar observaciones por provincia POR AÑO
  provincias_por_año <- panel_data %>%
    group_by(departamento, provincia, año) %>%
    summarise(n_obs = n(), .groups = "drop") %>%
    filter(n_obs >= 10)
  
  # Provincias que tienen datos en AMBOS años
  provincias_ambos_años <- provincias_por_año %>%
    group_by(departamento, provincia) %>%
    summarise(n_años = n_distinct(año), .groups = "drop") %>%
    filter(n_años == 2)
  
  cat("Provincias con datos en ambos años:", nrow(provincias_ambos_años), "\n")
  
  # Filtrar panel data
  datos_modelo <- panel_data %>%
    semi_join(provincias_ambos_años, by = c("departamento", "provincia")) %>%
    mutate(
      departamento = as.factor(departamento),
      provincia = as.factor(provincia),
      cultivo = as.factor(cultivo),
      año_factor = as.factor(año)
    )
  
  cat("\nDatos finales del panel:\n")
  cat("- Observaciones totales:", nrow(datos_modelo), "\n")
  cat("- Provincias:", n_distinct(datos_modelo$provincia), "\n")
  cat("- Departamentos:", n_distinct(datos_modelo$departamento), "\n")
  cat("- Obs 2023:", sum(datos_modelo$año == 2023), "\n")
  cat("- Obs 2024:", sum(datos_modelo$año == 2024), "\n\n")
  
  return(datos_modelo)
}

# ============================================================================
# PASO 5: EXTRAER COORDENADAS
# ============================================================================

extraer_coordenadas <- function(caratula_2023, caratula_2024, datos_modelo) {
  
  cat("=== EXTRAYENDO COORDENADAS GEOGRÁFICAS (2023-2024) ===\n")
  
  # Combinar carátulas de ambos años
  caratula_combinada <- bind_rows(
    caratula_2023 %>% mutate(año = 2023),
    caratula_2024 %>% mutate(año = 2024)
  )
  
  # Extraer coordenadas promedio por provincia
  coord_provincias <- caratula_combinada %>%
    filter(NOMBREDD %in% unique(datos_modelo$departamento),
           NOMBREPV %in% unique(datos_modelo$provincia)) %>%
    select(NOMBREDD, NOMBREPV, LATITUD, LONGITUD) %>%
    filter(!is.na(LATITUD), !is.na(LONGITUD)) %>%
    group_by(NOMBREDD, NOMBREPV) %>%
    summarise(
      latitud = mean(LATITUD, na.rm = TRUE),
      longitud = mean(LONGITUD, na.rm = TRUE),
      n_puntos = n(),
      .groups = "drop"
    )
  
  cat("Coordenadas extraídas para", nrow(coord_provincias), "provincias\n")
  
  # Verificar provincias sin coordenadas
  provincias_sin_coord <- setdiff(
    unique(datos_modelo$provincia),
    coord_provincias$NOMBREPV
  )
  
  if(length(provincias_sin_coord) > 0) {
    cat("ADVERTENCIA: Sin coordenadas para:", 
        paste(provincias_sin_coord, collapse = ", "), "\n")
  }
  
  # Unir coordenadas
  datos_con_coord <- datos_modelo %>%
    left_join(coord_provincias, 
              by = c("departamento" = "NOMBREDD", "provincia" = "NOMBREPV")) %>%
    filter(!is.na(latitud), !is.na(longitud))
  
  cat("Datos finales con coordenadas:", nrow(datos_con_coord), "registros\n\n")
  
  return(list(
    datos = datos_con_coord,
    coordenadas = coord_provincias
  ))
}

# ============================================================================
# PASO 6: CREAR MATRIZ DE PESOS ESPACIALES
# ============================================================================

crear_matriz_espacial <- function(coordenadas) {
  
  cat("=== CREANDO MATRIZ DE PESOS ESPACIALES ===\n")
  
  coords_df <- coordenadas %>%
    arrange(NOMBREPV) %>%
    select(provincia = NOMBREPV, longitud, latitud)
  
  coords_matrix <- as.matrix(coords_df[, c("longitud", "latitud")])
  rownames(coords_matrix) <- coords_df$provincia
  
  knn5 <- knearneigh(coords_matrix, k = 5, longlat = TRUE)
  nb_knn <- knn2nb(knn5)
  W_knn <- nb2listw(nb_knn, style = "W")
  
  cat("Matriz espacial creada:\n")
  cat("- Número de provincias:", nrow(coords_matrix), "\n")
  cat("- Vecinos promedio:", round(mean(card(nb_knn)), 2), "\n")
  cat("- Mín-Máx vecinos:", min(card(nb_knn)), "-", max(card(nb_knn)), "\n\n")
  
  pdf("mapa_vecindad.pdf", width = 10, height = 8)
  plot(nb_knn, coords_matrix, 
       main = "Estructura de Vecindad Espacial (K=5 vecinos más cercanos)",
       pch = 19, col = "darkblue", cex = 1.5)
  text(coords_matrix, labels = rownames(coords_matrix), 
       pos = 3, cex = 0.6, col = "red")
  dev.off()
  
  cat("Mapa de vecindad guardado: mapa_vecindad.pdf\n\n")
  
  return(list(
    W = W_knn,
    nb = nb_knn,
    coords = coords_matrix,
    coords_df = coords_df
  ))
}

# ============================================================================
# PASO 7: TESTS DE AUTOCORRELACIÓN ESPACIAL
# ============================================================================

test_autocorrelacion_espacial <- function(datos, matriz_espacial) {
  
  cat("\n=== TESTS DE AUTOCORRELACIÓN ESPACIAL ===\n")
  
  rend_prov <- datos %>%
    group_by(provincia) %>%
    summarise(
      rendimiento_medio = mean(rendimiento, na.rm = TRUE),
      n_obs = n(),
      .groups = "drop"
    ) %>%
    arrange(provincia)
  
  rend_prov <- rend_prov %>%
    filter(provincia %in% rownames(matriz_espacial$coords))
  
  rend_prov <- rend_prov[match(rownames(matriz_espacial$coords), 
                               rend_prov$provincia), ]
  
  moran_test <- moran.test(rend_prov$rendimiento_medio, matriz_espacial$W)
  
  cat("\n** ÍNDICE DE MORAN GLOBAL **\n")
  cat("Moran's I:", round(moran_test$estimate[1], 4), "\n")
  cat("Valor esperado:", round(moran_test$estimate[2], 4), "\n")
  cat("Varianza:", round(moran_test$estimate[3], 6), "\n")
  cat("Estadístico z:", round(moran_test$statistic, 3), "\n")
  cat("P-value:", format.pval(moran_test$p.value, digits = 4), "\n")
  
  if(moran_test$p.value < 0.05) {
    if(moran_test$estimate[1] > 0) {
      cat("\n✓ AUTOCORRELACIÓN ESPACIAL POSITIVA SIGNIFICATIVA\n")
      cat("  → Provincias con rendimientos similares están agrupadas\n")
      cat("  → SE RECOMIENDA usar modelo espacial CAR\n")
    } else {
      cat("\n✓ AUTOCORRELACIÓN ESPACIAL NEGATIVA SIGNIFICATIVA\n")
    }
  } else {
    cat("\n✗ No hay autocorrelación espacial significativa\n")
  }
  
  geary_test <- geary.test(rend_prov$rendimiento_medio, matriz_espacial$W)
  
  cat("\n** ÍNDICE DE GEARY **\n")
  cat("Geary's C:", round(geary_test$estimate[1], 4), "\n")
  cat("P-value:", format.pval(geary_test$p.value, digits = 4), "\n")
  
  local_moran <- localmoran(rend_prov$rendimiento_medio, matriz_espacial$W)
  
  rend_prov$moran_local <- local_moran[, "Ii"]
  rend_prov$moran_z <- local_moran[, "Z.Ii"]
  rend_prov$moran_pval <- local_moran[, "Pr(z != E(Ii))"]
  rend_prov$significativo <- ifelse(rend_prov$moran_pval < 0.05, "Sí", "No")
  
  cat("\n** MORAN LOCAL (LISA) **\n")
  cat("Provincias con autocorrelación local significativa (α=0.05):",
      sum(rend_prov$significativo == "Sí"), "de", nrow(rend_prov), "\n")
  
  if(sum(rend_prov$significativo == "Sí") > 0) {
    cat("\nProvincias con efectos locales significativos:\n")
    print(rend_prov %>% 
            filter(significativo == "Sí") %>%
            select(provincia, rendimiento_medio, moran_local, moran_pval) %>%
            arrange(desc(abs(moran_local))))
  }
  
  return(list(
    moran = moran_test,
    geary = geary_test,
    local_moran = rend_prov
  ))
}

# ============================================================================
# PASO 8: ANÁLISIS DE HOTSPOTS
# ============================================================================

analizar_hotspots <- function(datos, matriz_espacial) {
  
  cat("\n=== ANÁLISIS DE HOTSPOTS (Getis-Ord Gi*) ===\n")
  
  rend_prov <- datos %>%
    group_by(provincia) %>%
    summarise(
      rendimiento_medio = mean(rendimiento, na.rm = TRUE),
      n_obs = n(),
      prop_heladas = mean(heladas),
      prop_sequias = mean(sequias),
      .groups = "drop"
    ) %>%
    arrange(provincia)
  
  rend_prov <- rend_prov[match(rownames(matriz_espacial$coords), 
                               rend_prov$provincia), ]
  
  localG_result <- localG(rend_prov$rendimiento_medio, matriz_espacial$W)
  
  rend_prov$Gi_star <- as.numeric(localG_result)
  rend_prov$tipo_cluster <- cut(
    rend_prov$Gi_star,
    breaks = c(-Inf, -2.58, -1.96, -1.65, 1.65, 1.96, 2.58, Inf),
    labels = c("Cold spot 99%", "Cold spot 95%", "Cold spot 90%",
               "No significativo",
               "Hot spot 90%", "Hot spot 95%", "Hot spot 99%")
  )
  
  cat("\nDistribución de clusters:\n")
  print(table(rend_prov$tipo_cluster))
  
  cat("\n** PROVINCIAS CRÍTICAS (Cold spots - bajo rendimiento) **\n")
  cold_spots <- rend_prov %>%
    filter(grepl("Cold spot", tipo_cluster)) %>%
    arrange(Gi_star) %>%
    select(provincia, rendimiento_medio, Gi_star, tipo_cluster, 
           prop_heladas, prop_sequias)
  
  if(nrow(cold_spots) > 0) {
    print(cold_spots)
  } else {
    cat("No se detectaron cold spots significativos\n")
  }
  
  cat("\n** PROVINCIAS DESTACADAS (Hot spots - alto rendimiento) **\n")
  hot_spots <- rend_prov %>%
    filter(grepl("Hot spot", tipo_cluster)) %>%
    arrange(desc(Gi_star)) %>%
    select(provincia, rendimiento_medio, Gi_star, tipo_cluster)
  
  if(nrow(hot_spots) > 0) {
    print(hot_spots)
  } else {
    cat("No se detectaron hot spots significativos\n")
  }
  
  rend_prov_coords <- cbind(rend_prov, matriz_espacial$coords)
  
  p_hotspots <- ggplot(rend_prov_coords, 
                       aes(x = longitud, y = latitud, 
                           color = tipo_cluster, 
                           size = rendimiento_medio)) +
    geom_point(alpha = 0.7) +
    scale_color_manual(
      values = c(
        "Cold spot 99%" = "#d7191c",
        "Cold spot 95%" = "#fdae61",
        "Cold spot 90%" = "#fee08b",
        "No significativo" = "gray70",
        "Hot spot 90%" = "#d9ef8b",
        "Hot spot 95%" = "#a6d96a",
        "Hot spot 99%" = "#1a9641"
      ),
      name = "Tipo de Cluster",
      drop = FALSE
    ) +
    scale_size_continuous(range = c(3, 10), name = "Rendimiento\n(kg/ha)") +
    labs(
      title = "Hotspots y Coldspots de Rendimiento Agrícola",
      subtitle = "Getis-Ord Gi* - Sur del Perú (2023-2024)",
      x = "Longitud", y = "Latitud"
    ) +
    theme_minimal() +
    theme(legend.position = "right")
  
  print(p_hotspots)
  ggsave("mapa_hotspots.pdf", p_hotspots, width = 12, height = 8)
  cat("\nMapa de hotspots guardado: mapa_hotspots.pdf\n")
  
  return(rend_prov)
}

# ============================================================================
# PASO 9: ANÁLISIS EXPLORATORIO TEMPORAL
# ============================================================================

analisis_exploratorio_temporal <- function(datos) {
  
  cat("\n=== ANÁLISIS EXPLORATORIO TEMPORAL 2023-2024 ===\n")
  
  comp_años <- datos %>%
    group_by(año) %>%
    summarise(
      n_obs = n(),
      rendimiento_promedio = mean(rendimiento),
      sd_rendimiento = sd(rendimiento),
      prop_heladas = mean(heladas),
      prop_sequias = mean(sequias),
      prop_granizadas = mean(granizadas),
      .groups = "drop"
    )
  
  cat("\n** COMPARACIÓN GENERAL POR AÑO **\n")
  print(comp_años)
  
  cambio_pct <- ((comp_años$rendimiento_promedio[2] - comp_años$rendimiento_promedio[1]) / 
                   comp_años$rendimiento_promedio[1]) * 100
  
  cat("\nCambio en rendimiento 2023→2024:", round(cambio_pct, 2), "%\n")
  
  # GRÁFICO 1: Evolución por departamento
  evol_dept <- datos %>%
    group_by(año, departamento) %>%
    summarise(rendimiento_promedio = mean(rendimiento), .groups = "drop")
  
  p1 <- ggplot(evol_dept, aes(x = año, y = rendimiento_promedio, 
                              color = departamento, group = departamento)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    scale_color_brewer(palette = "Set1", name = "Departamento") +
    labs(
      title = "Evolución del Rendimiento por Departamento",
      subtitle = "Sur del Perú 2023-2024",
      x = "Año", y = "Rendimiento Promedio (kg/ha)"
    ) +
    theme_minimal()
  
  print(p1)
  ggsave("evolucion_departamento.pdf", p1, width = 10, height = 6)
  
  # GRÁFICO 2: Eventos climáticos
  eventos_año <- datos %>%
    group_by(año) %>%
    summarise(
      Heladas = mean(heladas) * 100,
      Sequías = mean(sequias) * 100,
      Granizadas = mean(granizadas) * 100,
      .groups = "drop"
    ) %>%
    pivot_longer(cols = c(Heladas, Sequías, Granizadas),
                 names_to = "Evento", values_to = "Porcentaje")
  
  p2 <- ggplot(eventos_año, aes(x = año, y = Porcentaje, fill = Evento)) +
    geom_col(position = "dodge") +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = "Proporción de Eventos Climáticos Extremos",
      subtitle = "Comparación 2023-2024",
      x = "Año", y = "Porcentaje de Productores Afectados (%)"
    ) +
    theme_minimal()
  
  print(p2)
  ggsave("eventos_climaticos_temporal.pdf", p2, width = 10, height = 6)
  
  # GRÁFICO 3: Cambios por provincia
  cambios_prov <- datos %>%
    group_by(provincia, año) %>%
    summarise(rendimiento_promedio = mean(rendimiento), .groups = "drop") %>%
    pivot_wider(names_from = año, values_from = rendimiento_promedio, 
                names_prefix = "año_") %>%
    filter(!is.na(año_2023), !is.na(año_2024)) %>%
    mutate(
      cambio_absoluto = año_2024 - año_2023,
      cambio_porcentual = (cambio_absoluto / año_2023) * 100,
      tipo_cambio = case_when(
        cambio_porcentual > 10 ~ "Mejora significativa",
        cambio_porcentual > 0 ~ "Mejora leve",
        cambio_porcentual > -10 ~ "Deterioro leve",
        TRUE ~ "Deterioro significativo"
      )
    ) %>%
    arrange(cambio_porcentual)
  
  cat("\n** PROVINCIAS CON MAYOR MEJORA **\n")
  print(head(cambios_prov %>% 
               arrange(desc(cambio_porcentual)) %>%
               select(provincia, año_2023, año_2024, cambio_porcentual), 5))
  
  cat("\n** PROVINCIAS CON MAYOR DETERIORO **\n")
  print(head(cambios_prov %>% 
               arrange(cambio_porcentual) %>%
               select(provincia, año_2023, año_2024, cambio_porcentual), 5))
  
  p3 <- ggplot(cambios_prov, 
               aes(x = reorder(provincia, cambio_porcentual), 
                   y = cambio_porcentual, fill = tipo_cambio)) +
    geom_col() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_fill_manual(
      values = c("Mejora significativa" = "#1a9641",
                 "Mejora leve" = "#a6d96a",
                 "Deterioro leve" = "#fdae61",
                 "Deterioro significativo" = "#d7191c"),
      name = "Tipo de Cambio"
    ) +
    coord_flip() +
    labs(
      title = "Cambio en Rendimiento por Provincia (2023→2024)",
      subtitle = "Sur del Perú",
      x = "Provincia", y = "Cambio Porcentual (%)"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p3)
  ggsave("cambios_provincia_temporal.pdf", p3, width = 12, height = 10)
  
  cat("\nGráficos temporales guardados:\n")
  cat("- evolucion_departamento.pdf\n")
  cat("- eventos_climaticos_temporal.pdf\n")
  cat("- cambios_provincia_temporal.pdf\n\n")
  
  return(list(
    comparacion_años = comp_años,
    cambios_provincia = cambios_prov,
    evolucion_departamento = evol_dept
  ))
}

# ============================================================================
# PASO 10: MODELO ESPACIO-TEMPORAL
# ============================================================================

ajustar_modelo_espaciotemporal <- function(datos) {
  
  cat("\n=== AJUSTANDO MODELO JERÁRQUICO ESPACIO-TEMPORAL ===\n")
  cat("NOTA: Este modelo incluye efectos temporales y espacio-temporales\n")
  cat("Puede tardar 15-30 minutos en ajustarse...\\n\n")
  
  modelo_temporal <- brm(
    rendimiento ~ 
      # Efectos principales e interacciones con año
      heladas * cultivo * año_factor +
      sequias * año_factor +
      granizadas * año_factor +
      
      # Prácticas agrícolas
      materia_organica + terrazas + rotacion_cultivos + manejo_agua +
      semilla_certificada +
      
      # Efectos aleatorios jerárquicos
      (1 | departamento/provincia) +
      
      # Efecto aleatorio temporal
      (1 | año_factor) +
      
      # Interacción espacio-temporal
      (1 | provincia:año_factor),
    
    data = datos,
    family = gaussian(),
    
    prior = c(
      prior(normal(0, 1000), class = Intercept),
      prior(normal(0, 500), class = b),
      prior(exponential(0.01), class = sd)
    ),
    
    chains = 4,
    iter = 2000,
    warmup = 1000,
    cores = 4,
    control = list(adapt_delta = 0.98, max_treedepth = 12)
  )
  
  cat("\n✓ Modelo espacio-temporal ajustado exitosamente\n")
  
  return(modelo_temporal)
}

# ============================================================================
# PASO 11: MODELO ESPACIAL CAR
# ============================================================================

ajustar_modelo_espacial_car <- function(datos, matriz_espacial) {
  
  cat("\n=== AJUSTANDO MODELO CON AUTOCORRELACIÓN ESPACIAL (CAR) ===\n")
  cat("NOTA: Usando datos agregados por provincia y año\n")
  cat("Este proceso puede tardar 10-30 minutos...\n\n")
  
  provincias_orden <- data.frame(
    provincia = rownames(matriz_espacial$coords),
    prov_id = 1:nrow(matriz_espacial$coords)
  )
  
  datos_car <- datos %>%
    left_join(provincias_orden, by = "provincia") %>%
    filter(!is.na(prov_id))
  
  W_nb <- matriz_espacial$nb
  n_prov <- length(W_nb)
  
  W_matrix <- matrix(0, n_prov, n_prov)
  for(i in 1:n_prov) {
    if(length(W_nb[[i]]) > 0) {
      W_matrix[i, W_nb[[i]]] <- 1
    }
  }
  
  W_matrix <- W_matrix + t(W_matrix)
  W_matrix[W_matrix > 0] <- 1
  diag(W_matrix) <- 0
  
  datos_agregados <- datos_car %>%
    group_by(prov_id, provincia, departamento, año) %>%
    summarise(
      rendimiento = mean(rendimiento),
      heladas = mean(heladas),
      sequias = mean(sequias),
      granizadas = mean(granizadas),
      materia_organica = mean(materia_organica),
      terrazas = mean(terrazas),
      rotacion_cultivos = mean(rotacion_cultivos),
      manejo_agua = mean(manejo_agua),
      semilla_certificada = mean(semilla_certificada),
      cultivo_papa = mean(cultivo == "PAPA BLANCA"),
      año_2024 = ifelse(año == 2024, 1, 0),
      .groups = "drop"
    ) %>%
    arrange(año, prov_id)
  
  cat("Datos agregados para modelo CAR:", nrow(datos_agregados), "observaciones\n")
  cat("(", n_prov, "provincias ×", n_distinct(datos_agregados$año), "años )\n\n")
  
  modelo_car <- S.CARleroux(
    formula = rendimiento ~ heladas + sequias + granizadas + 
      materia_organica + terrazas + rotacion_cultivos + 
      manejo_agua + semilla_certificada + cultivo_papa + año_2024,
    data = datos_agregados,
    family = "gaussian",
    W = W_matrix,
    burnin = 10000,
    n.sample = 30000,
    thin = 10,
    verbose = FALSE
  )
  
  cat("\n✓ Modelo espacial CAR ajustado exitosamente\n")
  
  return(list(
    modelo = modelo_car,
    datos_agregados = datos_agregados
  ))
}

# ============================================================================
# PASO 12: DIAGNÓSTICOS
# ============================================================================

diagnosticos_modelo <- function(modelo, nombre_modelo = "Modelo") {
  
  cat("\n=== DIAGNÓSTICOS DEL", nombre_modelo, "===\n")
  
  print(summary(modelo))
  
  modelo_summary <- summary(modelo)
  rhat_values <- modelo_summary$fixed[,"Rhat"]
  cat("\nR-hat máximo:", max(rhat_values, na.rm = TRUE), "\n")
  
  if(max(rhat_values, na.rm = TRUE) > 1.1) {
    cat("⚠ ADVERTENCIA: Problemas de convergencia (R-hat > 1.1)\n")
  } else {
    cat("✓ Convergencia satisfactoria (R-hat < 1.1)\n")
  }
  
  r2 <- bayes_R2(modelo)
  cat("\nR-squared bayesiano:", round(median(r2), 3), "\n")
  
  loo_resultado <- loo(modelo)
  print(loo_resultado)
  
  pdf(paste0("diagnosticos_", gsub(" ", "_", tolower(nombre_modelo)), ".pdf"), 
      width = 12, height = 10)
  plot(modelo, pars = "^b_", ask = FALSE)
  dev.off()
  
  cat("\nDiagnósticos guardados en PDF\n")
  
  return(list(modelo = modelo, loo = loo_resultado))
}

# ============================================================================
# PASO 13: INTERPRETACIÓN TEMPORAL
# ============================================================================

interpretar_resultados_temporales <- function(modelo_temporal, datos) {
  
  cat("\n=== INTERPRETACIÓN DE RESULTADOS ESPACIO-TEMPORALES ===\n")
  
  coef_fijos <- fixef(modelo_temporal)
  
  cat("\n** EVENTOS CLIMÁTICOS (Efecto Principal) **\n")
  cat("- Heladas:", round(coef_fijos["heladas", "Estimate"], 2), "kg/ha",
      "(IC 95%: [", round(coef_fijos["heladas", "Q2.5"], 2), ",",
      round(coef_fijos["heladas", "Q97.5"], 2), "])\n")
  cat("- Sequías:", round(coef_fijos["sequias", "Estimate"], 2), "kg/ha",
      "(IC 95%: [", round(coef_fijos["sequias", "Q2.5"], 2), ",",
      round(coef_fijos["sequias", "Q97.5"], 2), "])\n")
  cat("- Granizadas:", round(coef_fijos["granizadas", "Estimate"], 2), "kg/ha",
      "(IC 95%: [", round(coef_fijos["granizadas", "Q2.5"], 2), ",",
      round(coef_fijos["granizadas", "Q97.5"], 2), "])\n")
  
  cat("\n** CAMBIOS TEMPORALES (Interacciones con Año) **\n")
  coef_nombres <- rownames(coef_fijos)
  interact_año <- grep("año_factor2024", coef_nombres, value = TRUE)
  
  if(length(interact_año) > 0) {
    cat("\nEfectos que cambiaron en 2024 vs 2023:\n")
    for(coef_name in interact_año) {
      cat("-", coef_name, ":", 
          round(coef_fijos[coef_name, "Estimate"], 2), "kg/ha",
          "(IC 95%: [", round(coef_fijos[coef_name, "Q2.5"], 2), ",",
          round(coef_fijos[coef_name, "Q97.5"], 2), "])\n")
    }
  }
  
  cat("\n** PRÁCTICAS AGRÍCOLAS **\n")
  cat("- Materia orgánica:", round(coef_fijos["materia_organica", "Estimate"], 2), "kg/ha\n")
  cat("- Terrazas:", round(coef_fijos["terrazas", "Estimate"], 2), "kg/ha\n")
  cat("- Rotación:", round(coef_fijos["rotacion_cultivos", "Estimate"], 2), "kg/ha\n")
  cat("- Manejo agua:", round(coef_fijos["manejo_agua", "Estimate"], 2), "kg/ha\n")
  
  cat("\n** EFECTOS ALEATORIOS TEMPORALES **\n")
  if("año_factor" %in% names(ranef(modelo_temporal))) {
    efectos_año <- ranef(modelo_temporal)$año_factor[, , "Intercept"]
    cat("\nEfecto aleatorio por año:\n")
    print(efectos_año)
  }
  
  cat("\n** VARIANZA DE EFECTOS ALEATORIOS **\n")
  vc <- VarCorr(modelo_temporal)
  print(vc)
  
  return(coef_fijos)
}

# ============================================================================
# PASO 14: ANÁLISIS DE CAMBIOS PROVINCIALES
# ============================================================================

analizar_cambios_provinciales <- function(datos, modelo_temporal, matriz_espacial, hotspots) {
  
  cat("\n=== ANÁLISIS DE CAMBIOS PROVINCIALES 2023→2024 ===\n")
  
  cambios_obs <- datos %>%
    group_by(provincia, departamento, año) %>%
    summarise(
      rendimiento_medio = mean(rendimiento),
      prop_heladas = mean(heladas),
      prop_sequias = mean(sequias),
      n_obs = n(),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = año,
      values_from = c(rendimiento_medio, prop_heladas, prop_sequias, n_obs),
      names_sep = "_"
    ) %>%
    mutate(
      cambio_rendimiento = rendimiento_medio_2024 - rendimiento_medio_2023,
      cambio_pct = (cambio_rendimiento / rendimiento_medio_2023) * 100,
      cambio_heladas = prop_heladas_2024 - prop_heladas_2023,
      cambio_sequias = prop_sequias_2024 - prop_sequias_2023
    )
  
  cambios_geo <- cambios_obs %>%
    left_join(matriz_espacial$coords_df, by = "provincia") %>%
    left_join(hotspots %>% select(provincia, tipo_cluster, Gi_star), by = "provincia")
  
  cat("\n** ESTADÍSTICAS DE CAMBIO **\n")
  cat("Cambio promedio:", round(mean(cambios_geo$cambio_rendimiento, na.rm = TRUE), 2), "kg/ha\n")
  cat("Cambio porcentual promedio:", round(mean(cambios_geo$cambio_pct, na.rm = TRUE), 2), "%\n")
  cat("Provincias que mejoraron:", sum(cambios_geo$cambio_rendimiento > 0, na.rm = TRUE), "\n")
  cat("Provincias que empeoraron:", sum(cambios_geo$cambio_rendimiento < 0, na.rm = TRUE), "\n")
  
  # MAPA de cambios
  p1 <- ggplot(cambios_geo, 
               aes(x = longitud, y = latitud, 
                   color = cambio_pct, size = abs(cambio_rendimiento))) +
    geom_point(alpha = 0.7) +
    scale_color_gradient2(
      low = "#d7191c", mid = "white", high = "#1a9641",
      midpoint = 0,
      name = "Cambio\n(%)"
    ) +
    scale_size_continuous(range = c(2, 12), name = "Magnitud\nCambio\n(kg/ha)") +
    geom_text(aes(label = provincia), size = 2.5, vjust = -1.5, color = "black") +
    labs(
      title = "Cambios Espaciales en Rendimiento (2023→2024)",
      subtitle = "Sur del Perú",
      x = "Longitud", y = "Latitud"
    ) +
    theme_minimal()
  
  print(p1)
  ggsave("mapa_cambios_temporales.pdf", p1, width = 14, height = 10)
  
  # Mapa interactivo
  cambios_geo_limpio <- cambios_geo %>%
    filter(!is.na(longitud), !is.na(latitud), !is.na(cambio_pct))
  
  pal <- colorNumeric(
    palette = c("#d7191c", "#fdae61", "#ffffbf", "#a6d96a", "#1a9641"),
    domain = cambios_geo_limpio$cambio_pct
  )
  
  mapa_interactivo <- leaflet(cambios_geo_limpio) %>%
    addTiles() %>%
    addCircleMarkers(
      lng = ~longitud,
      lat = ~latitud,
      radius = ~abs(cambio_rendimiento) / 100,
      color = ~pal(cambio_pct),
      fillOpacity = 0.7,
      popup = ~paste0(
        "<b>", provincia, "</b><br>",
        "Departamento: ", departamento, "<br>",
        "Cambio: ", round(cambio_rendimiento, 2), " kg/ha (", 
        round(cambio_pct, 1), "%)<br>",
        "Rend 2023: ", round(rendimiento_medio_2023, 0), " kg/ha<br>",
        "Rend 2024: ", round(rendimiento_medio_2024, 0), " kg/ha<br>",
        "Cluster: ", tipo_cluster
      ),
      label = ~provincia
    ) %>%
    addLegend(
      "bottomright",
      pal = pal,
      values = ~cambio_pct,
      title = "Cambio (%)",
      opacity = 1
    )
  
  saveWidget(mapa_interactivo, "mapa_cambios_interactivo.html", selfcontained = TRUE)
  cat("\nMapa interactivo guardado: mapa_cambios_interactivo.html\n")
  
  # TABLA de cambios
  tabla_cambios <- cambios_geo %>%
    mutate(
      prioridad_intervencion = case_when(
        grepl("Cold spot", tipo_cluster) & cambio_pct < -5 ~ "Urgente",
        cambio_pct < -10 ~ "Alta",
        cambio_pct < 0 ~ "Media",
        cambio_pct < 10 ~ "Baja",
        TRUE ~ "Monitoreo"
      )
    ) %>%
    select(departamento, provincia, 
           rendimiento_medio_2023, rendimiento_medio_2024,
           cambio_rendimiento, cambio_pct,
           tipo_cluster, prioridad_intervencion) %>%
    arrange(cambio_pct)
  
  cat("\n** PROVINCIAS CON MAYOR DETERIORO **\n")
  print(head(tabla_cambios, 10))
  
  write.csv(tabla_cambios, "tabla_cambios_provinciales_2023_2024.csv", row.names = FALSE)
  cat("\nTabla de cambios guardada: tabla_cambios_provinciales_2023_2024.csv\n")
  
  return(list(
    cambios = cambios_geo,
    tabla_cambios = tabla_cambios
  ))
}

# ============================================================================
# PASO 15: VISUALIZACIONES ESPACIALES
# ============================================================================

generar_mapas_espaciales <- function(modelo, datos, matriz_espacial, hotspots) {
  
  cat("\n=== GENERANDO MAPAS ESPACIALES AVANZADOS ===\n")
  
  efectos_dept <- ranef(modelo)$departamento[, , "Intercept"]
  efectos_dept_df <- data.frame(
    departamento = rownames(efectos_dept),
    efecto_dept = efectos_dept[, "Estimate"],
    error_dept = efectos_dept[, "Est.Error"]
  )
  
  resumen_prov <- datos %>%
    group_by(provincia, departamento) %>%
    summarise(
      rendimiento_promedio = mean(rendimiento),
      n_obs = n(),
      prop_heladas = mean(heladas),
      prop_sequias = mean(sequias),
      prop_granizadas = mean(granizadas),
      .groups = "drop"
    ) %>%
    left_join(efectos_dept_df, by = "departamento") %>%
    left_join(matriz_espacial$coords_df %>% select(provincia, longitud, latitud), by = "provincia") %>%
    left_join(hotspots %>% select(provincia, Gi_star, tipo_cluster), by = "provincia")
  
  # MAPA 1: Efectos departamento
  p1 <- ggplot(resumen_prov, 
               aes(x = reorder(departamento, efecto_dept), 
                   y = efecto_dept, fill = efecto_dept)) +
    geom_col() +
    geom_errorbar(aes(ymin = efecto_dept - error_dept, 
                      ymax = efecto_dept + error_dept), width = 0.2) +
    scale_fill_gradient2(low = "#d7191c", mid = "white", high = "#1a9641", 
                         midpoint = 0, name = "Efecto\n(kg/ha)") +
    labs(title = "Efectos Aleatorios por Departamento",
         subtitle = "Rendimiento relativo ajustado (2023-2024)",
         x = "Departamento", y = "Efecto Aleatorio (kg/ha)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p1)
  ggsave("efectos_departamento.pdf", p1, width = 10, height = 6)
  
  # MAPA 2: Rendimiento con hotspots
  p2 <- ggplot(resumen_prov, 
               aes(x = longitud, y = latitud)) +
    geom_point(aes(size = rendimiento_promedio, color = tipo_cluster), 
               alpha = 0.7) +
    scale_color_manual(
      values = c(
        "Cold spot 99%" = "#d7191c",
        "Cold spot 95%" = "#fdae61",
        "Cold spot 90%" = "#fee08b",
        "No significativo" = "gray60",
        "Hot spot 90%" = "#d9ef8b",
        "Hot spot 95%" = "#a6d96a",
        "Hot spot 99%" = "#1a9641"
      ),
      name = "Cluster Espacial"
    ) +
    scale_size_continuous(range = c(3, 12), name = "Rendimiento\n(kg/ha)") +
    geom_text(aes(label = provincia), size = 2.5, vjust = -1.5, alpha = 0.7) +
    labs(title = "Mapa de Rendimiento con Hotspots",
         subtitle = "Sur del Perú - Panel 2023-2024",
         x = "Longitud", y = "Latitud") +
    theme_minimal()
  
  print(p2)
  ggsave("mapa_rendimiento_hotspots.pdf", p2, width = 14, height = 10)
  
  # MAPA 3: Vulnerabilidad
  resumen_prov <- resumen_prov %>%
    mutate(
      vulnerabilidad = prop_heladas + prop_sequias + prop_granizadas,
      nivel_vulnerabilidad = cut(vulnerabilidad,
                                 breaks = c(0, 0.5, 1, 1.5, 3),
                                 labels = c("Baja", "Media", "Alta", "Muy Alta"))
    )
  
  p3 <- ggplot(resumen_prov, 
               aes(x = longitud, y = latitud, 
                   color = nivel_vulnerabilidad, 
                   size = vulnerabilidad)) +
    geom_point(alpha = 0.7) +
    scale_color_manual(
      values = c("Baja" = "#1a9641", "Media" = "#fee08b",
                 "Alta" = "#fdae61", "Muy Alta" = "#d7191c"),
      name = "Nivel de\nVulnerabilidad"
    ) +
    scale_size_continuous(range = c(3, 12), name = "Índice de\nVulnerabilidad") +
    geom_text(aes(label = provincia), size = 2.5, vjust = -1.5) +
    labs(title = "Vulnerabilidad Climática por Provincia",
         subtitle = "Promedio 2023-2024",
         x = "Longitud", y = "Latitud") +
    theme_minimal()
  
  print(p3)
  ggsave("mapa_vulnerabilidad.pdf", p3, width = 14, height = 10)
  
  # MAPA 4: Interactivo principal
  resumen_limpio <- resumen_prov %>%
    filter(!is.na(longitud), !is.na(latitud))
  
  pal_cluster <- colorFactor(
    palette = c("#d7191c", "#fdae61", "#fee08b", "gray60", 
                "#d9ef8b", "#a6d96a", "#1a9641"),
    levels = c("Cold spot 99%", "Cold spot 95%", "Cold spot 90%",
               "No significativo",
               "Hot spot 90%", "Hot spot 95%", "Hot spot 99%")
  )
  
  mapa_principal <- leaflet(resumen_limpio) %>%
    addTiles() %>%
    addCircleMarkers(
      lng = ~longitud,
      lat = ~latitud,
      radius = ~rendimiento_promedio / 500,
      color = ~pal_cluster(tipo_cluster),
      fillOpacity = 0.7,
      popup = ~paste0(
        "<b>", provincia, "</b><br>",
        "Departamento: ", departamento, "<br>",
        "Rendimiento promedio: ", round(rendimiento_promedio, 0), " kg/ha<br>",
        "N observaciones: ", n_obs, "<br>",
        "Cluster espacial: ", tipo_cluster, "<br>",
        "Gi* statistic: ", round(Gi_star, 3), "<br>",
        "% Heladas: ", round(prop_heladas * 100, 1), "%<br>",
        "% Sequías: ", round(prop_sequias * 100, 1), "%<br>",
        "Vulnerabilidad: ", nivel_vulnerabilidad
      ),
      label = ~provincia
    ) %>%
    addLegend(
      "bottomright",
      pal = pal_cluster,
      values = ~tipo_cluster,
      title = "Tipo de Cluster",
      opacity = 1
    )
  
  saveWidget(mapa_principal, "mapa_principal_interactivo.html", selfcontained = TRUE)
  cat("\nMapa interactivo principal guardado: mapa_principal_interactivo.html\n")
  
  # TABLA políticas
  tabla_politicas <- resumen_prov %>%
    mutate(
      prioridad = case_when(
        grepl("Cold spot", tipo_cluster) & vulnerabilidad > 1 ~ "Crítica",
        grepl("Cold spot", tipo_cluster) | vulnerabilidad > 1.5 ~ "Alta",
        efecto_dept < 0 ~ "Media",
        TRUE ~ "Baja"
      )
    ) %>%
    select(departamento, provincia, rendimiento_promedio, efecto_dept,
           vulnerabilidad, tipo_cluster, prioridad) %>%
    arrange(desc(prioridad == "Crítica"), desc(prioridad == "Alta"), vulnerabilidad)
  
  cat("\n=== TABLA DE PRIORIDADES PARA POLÍTICAS PÚBLICAS ===\n")
  print(tabla_politicas)
  
  write.csv(tabla_politicas, "tabla_prioridades_politicas.csv", row.names = FALSE)
  cat("\nTabla guardada: tabla_prioridades_politicas.csv\n")
  
  return(list(
    resumen_espacial = resumen_prov,
    tabla_politicas = tabla_politicas
  ))
}

# ============================================================================
# PASO 16: VISUALIZACIONES TEMPORALES ADICIONALES
# ============================================================================

crear_visualizaciones_temporales <- function(datos, cambios_provinciales) {
  
  cat("\n=== CREANDO VISUALIZACIONES TEMPORALES ADICIONALES ===\n")
  
  # GRÁFICO 1: Boxplot
  p1 <- ggplot(datos, aes(x = departamento, y = rendimiento, fill = año_factor)) +
    geom_boxplot(alpha = 0.7) +
    scale_fill_brewer(palette = "Set2", name = "Año") +
    labs(
      title = "Distribución de Rendimientos por Departamento y Año",
      subtitle = "Sur del Perú",
      x = "Departamento", y = "Rendimiento (kg/ha)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p1)
  ggsave("boxplot_temporal_departamento.pdf", p1, width = 12, height = 7)
  
  # GRÁFICO 2: Violín por cultivo
  p2 <- ggplot(datos, aes(x = cultivo, y = rendimiento, fill = año_factor)) +
    geom_violin(alpha = 0.7, draw_quantiles = c(0.25, 0.5, 0.75)) +
    scale_fill_brewer(palette = "Pastel1", name = "Año") +
    labs(
      title = "Distribución de Rendimientos por Cultivo",
      subtitle = "Comparación 2023-2024",
      x = "Cultivo", y = "Rendimiento (kg/ha)"
    ) +
    theme_minimal()
  
  print(p2)
  ggsave("violin_temporal_cultivo.pdf", p2, width = 10, height = 6)
  
  # GRÁFICO 3: Scatter vulnerabilidad vs cambios
  if(!is.null(cambios_provinciales$cambios)) {
    cambios_vuln <- cambios_provinciales$cambios %>%
      mutate(vulnerabilidad = prop_heladas_2023 + prop_sequias_2023)
    
    p3 <- ggplot(cambios_vuln, 
                 aes(x = vulnerabilidad, y = cambio_pct, 
                     color = tipo_cluster, size = n_obs_2023 + n_obs_2024)) +
      geom_point(alpha = 0.7) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
      scale_color_manual(
        values = c(
          "Cold spot 99%" = "#d7191c",
          "Cold spot 95%" = "#fdae61",
          "Cold spot 90%" = "#fee08b",
          "No significativo" = "gray60",
          "Hot spot 90%" = "#d9ef8b",
          "Hot spot 95%" = "#a6d96a",
          "Hot spot 99%" = "#1a9641"
        ),
        name = "Cluster Espacial"
      ) +
      scale_size_continuous(range = c(2, 10), name = "N obs") +
      labs(
        title = "Relación entre Vulnerabilidad Climática y Cambios en Rendimiento",
        subtitle = "Sur del Perú 2023→2024",
        x = "Índice de Vulnerabilidad (2023)", 
        y = "Cambio en Rendimiento (%)"
      ) +
      theme_minimal()
    
    print(p3)
    ggsave("scatter_vulnerabilidad_cambios.pdf", p3, width = 12, height = 8)
  }
  
  # GRÁFICO 4: Heatmap
  cambios_heatmap <- cambios_provinciales$cambios %>%
    select(departamento, provincia, cambio_pct) %>%
    filter(!is.na(cambio_pct))
  
  p4 <- ggplot(cambios_heatmap, 
               aes(x = 1, y = provincia, fill = cambio_pct)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      low = "#d7191c", mid = "white", high = "#1a9641",
      midpoint = 0,
      name = "Cambio\n(%)"
    ) +
    facet_grid(departamento ~ ., scales = "free_y", space = "free_y") +
    labs(
      title = "Cambios en Rendimiento por Provincia (2023→2024)",
      subtitle = "Organizado por Departamento",
      x = "", y = "Provincia"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      strip.text.y = element_text(angle = 0, hjust = 0)
    )
  
  print(p4)
  ggsave("heatmap_cambios_provincia.pdf", p4, width = 8, height = 14)
  
  cat("\nVisualizaciones temporales guardadas:\n")
  cat("- boxplot_temporal_departamento.pdf\n")
  cat("- violin_temporal_cultivo.pdf\n")
  cat("- scatter_vulnerabilidad_cambios.pdf\n")
  cat("- heatmap_cambios_provincia.pdf\n\n")
  
  return(list(
    plot_boxplot = p1,
    plot_violin = p2,
    plot_scatter = p3,
    plot_heatmap = p4
  ))
}

# ============================================================================
# PASO 17: GENERAR REPORTE EJECUTIVO
# ============================================================================

generar_reporte_ejecutivo <- function(resultados_completos) {
  
  cat("\n=== GENERANDO REPORTE EJECUTIVO ===\n")
  
  reporte <- c(
    "================================================================================",
    "REPORTE EJECUTIVO: ANÁLISIS ESPACIO-TEMPORAL DE RENDIMIENTO AGRÍCOLA",
    "Sur del Perú (2023-2024)",
    "================================================================================",
    "",
    paste("Fecha de análisis:", Sys.Date()),
    "",
    "1. RESUMEN DE DATOS",
    "-------------------",
    paste("Total de observaciones:", nrow(resultados_completos$datos)),
    paste("Provincias analizadas:", n_distinct(resultados_completos$datos$provincia)),
    paste("Departamentos:", n_distinct(resultados_completos$datos$departamento)),
    paste("Años: 2023-2024"),
    paste("Observaciones 2023:", sum(resultados_completos$datos$año == 2023)),
    paste("Observaciones 2024:", sum(resultados_completos$datos$año == 2024)),
    "",
    "2. AUTOCORRELACIÓN ESPACIAL",
    "---------------------------",
    paste("Índice de Moran:", 
          round(resultados_completos$tests_espaciales$moran$estimate[1], 4)),
    paste("P-value:", 
          format.pval(resultados_completos$tests_espaciales$moran$p.value, digits = 4)),
    "",
    "Interpretación: ",
    ifelse(resultados_completos$tests_espaciales$moran$estimate[1] > 0.3,
           "ALTA dependencia espacial - Provincias vecinas tienen rendimientos similares",
           ifelse(resultados_completos$tests_espaciales$moran$estimate[1] > 0.15,
                  "MODERADA dependencia espacial",
                  "BAJA dependencia espacial")),
    "",
    "3. HOTSPOTS Y COLDSPOTS",
    "-----------------------"
  )
  
  n_coldspots <- sum(grepl("Cold spot", resultados_completos$hotspots$tipo_cluster))
  n_hotspots <- sum(grepl("Hot spot", resultados_completos$hotspots$tipo_cluster))
  
  reporte <- c(reporte,
               paste("Cold spots detectados:", n_coldspots),
               paste("Hot spots detectados:", n_hotspots),
               "",
               "Provincias críticas (Cold spots 95%+):"
  )
  
  critical_provinces <- resultados_completos$hotspots %>%
    filter(tipo_cluster %in% c("Cold spot 95%", "Cold spot 99%")) %>%
    arrange(Gi_star) %>%
    select(provincia, rendimiento_medio, tipo_cluster)
  
  if(nrow(critical_provinces) > 0) {
    for(i in 1:min(5, nrow(critical_provinces))) {
      reporte <- c(reporte,
                   paste("  -", critical_provinces$provincia[i], ":",
                         round(critical_provinces$rendimiento_medio[i], 0), "kg/ha"))
    }
  } else {
    reporte <- c(reporte, "  Ninguna provincia en categoría crítica")
  }
  
  reporte <- c(reporte,
               "",
               "4. MODELO ESPACIO-TEMPORAL",
               "--------------------------"
  )
  
  if(!is.null(resultados_completos$comparacion)) {
    reporte <- c(reporte,
                 paste("R² Modelo Simple:", 
                       round(median(resultados_completos$comparacion$r2_simple), 3)),
                 paste("R² Modelo Temporal:", 
                       round(median(resultados_completos$comparacion$r2_temporal), 3)),
                 paste("Mejora R²:", 
                       round(median(resultados_completos$comparacion$r2_temporal) - 
                               median(resultados_completos$comparacion$r2_simple), 4))
    )
  }
  
  if(!is.null(resultados_completos$modelo_car) && !is.null(resultados_completos$modelo_car$modelo)) {
    reporte <- c(reporte,
                 "",
                 "Parámetro espacial (rho):",
                 paste("  Media:", 
                       round(mean(resultados_completos$comparacion$rho), 3)),
                 paste("  Interpretación:",
                       ifelse(mean(resultados_completos$comparacion$rho) > 0.3,
                              "Fuerte dependencia espacial",
                              "Dependencia espacial moderada"))
    )
  }
  
  if(!is.null(resultados_completos$analisis_temporal)) {
    cambio_general <- resultados_completos$analisis_temporal$comparacion_años
    
    if(nrow(cambio_general) >= 2) {
      cambio_pct <- ((cambio_general$rendimiento_promedio[2] - 
                        cambio_general$rendimiento_promedio[1]) / 
                       cambio_general$rendimiento_promedio[1]) * 100
      
      reporte <- c(reporte,
                   "",
                   "5. CAMBIOS TEMPORALES 2023→2024",
                   "-------------------------------",
                   paste("Cambio en rendimiento promedio:", round(cambio_pct, 2), "%"),
                   paste("Rendimiento 2023:", 
                         round(cambio_general$rendimiento_promedio[1], 0), "kg/ha"),
                   paste("Rendimiento 2024:", 
                         round(cambio_general$rendimiento_promedio[2], 0), "kg/ha")
      )
    }
  }
  
  if(!is.null(resultados_completos$cambios_provinciales)) {
    cambios <- resultados_completos$cambios_provinciales$cambios
    
    provincias_mejora <- cambios %>%
      filter(cambio_pct > 0) %>%
      arrange(desc(cambio_pct)) %>%
      head(3)
    
    provincias_deterioro <- cambios %>%
      filter(cambio_pct < 0) %>%
      arrange(cambio_pct) %>%
      head(3)
    
    reporte <- c(reporte,
                 "",
                 "Provincias con mayor mejora:"
    )
    
    if(nrow(provincias_mejora) > 0) {
      for(i in 1:nrow(provincias_mejora)) {
        reporte <- c(reporte,
                     paste("  ", i, ".", provincias_mejora$provincia[i], ":",
                           round(provincias_mejora$cambio_pct[i], 1), "%"))
      }
    }
    
    reporte <- c(reporte,
                 "",
                 "Provincias con mayor deterioro:"
    )
    
    if(nrow(provincias_deterioro) > 0) {
      for(i in 1:nrow(provincias_deterioro)) {
        reporte <- c(reporte,
                     paste("  ", i, ".", provincias_deterioro$provincia[i], ":",
                           round(provincias_deterioro$cambio_pct[i], 1), "%"))
      }
    }
  }
  
  reporte <- c(reporte,
               "",
               "6. RECOMENDACIONES DE POLÍTICA",
               "------------------------------",
               "1. Priorizar intervención en cold spots detectados",
               "2. Estudiar prácticas exitosas en hot spots",
               "3. Focalizar apoyo en provincias con deterioro temporal",
               "4. Considerar dependencia espacial en programas regionales",
               "",
               "================================================================================",
               "Archivos generados disponibles para análisis detallado:",
               "- Mapas PDF (5+ archivos)",
               "- Mapas interactivos HTML (2+ archivos)",
               "- Tablas CSV (2+ archivos)",
               "- Gráficos temporales PDF (4+ archivos)",
               "================================================================================",
               ""
  )
  
  writeLines(reporte, "reporte_ejecutivo.txt")
  
  cat("\n", paste(reporte, collapse = "\n"), "\n")
  cat("\nReporte ejecutivo guardado: reporte_ejecutivo.txt\n\n")
  
  return(reporte)
}

# ============================================================================
# PASO 18: ANÁLISIS POR CULTIVO
# ============================================================================

analisis_por_cultivo <- function(resultados) {
  
  cat("\n=== ANÁLISIS ESPACIO-TEMPORAL POR CULTIVO ===\n")
  
  datos <- resultados$datos
  
  for(cult in c("PAPA BLANCA", "MAIZ AMILACEO")) {
    cat("\n** CULTIVO:", cult, "**\n")
    
    datos_cult <- datos %>% filter(cultivo == cult)
    
    resumen_año <- datos_cult %>%
      group_by(año) %>%
      summarise(
        n_obs = n(),
        rendimiento_medio = mean(rendimiento),
        prop_heladas = mean(heladas),
        prop_sequias = mean(sequias),
        .groups = "drop"
      )
    
    cat("\nResumen por año:\n")
    print(resumen_año)
    
    if(nrow(resumen_año) == 2) {
      cambio <- ((resumen_año$rendimiento_medio[2] - resumen_año$rendimiento_medio[1]) /
                   resumen_año$rendimiento_medio[1]) * 100
      cat("\nCambio 2023→2024:", round(cambio, 2), "%\n")
    }
    
    top_provincias <- datos_cult %>%
      group_by(provincia) %>%
      summarise(
        rendimiento_medio = mean(rendimiento),
        n_obs = n(),
        .groups = "drop"
      ) %>%
      filter(n_obs >= 10) %>%
      arrange(desc(rendimiento_medio)) %>%
      head(5)
    
    cat("\nTop 5 provincias:\n")
    print(top_provincias)
    
    cat("\n", rep("-", 70), "\n")
  }
}

# ============================================================================
# FUNCIÓN PRINCIPAL ESPACIO-TEMPORAL
# ============================================================================

ejecutar_analisis_espaciotemporal_completo <- function(usar_modelo_car = TRUE) {
  
  cat("==========================================================\n")
  cat("ANÁLISIS ESPACIAL BAYESIANO JERÁRQUICO ESPACIO-TEMPORAL\n")
  cat("Eventos Climáticos y Agricultura - Sur del Perú\n")
  cat("PANEL DATA 2023-2024\n")
  cat("==========================================================\n\n")
  
  # 1. Crear panel data
  panel_resultado <- crear_panel_data()
  panel_data <- panel_resultado$panel
  
  # 2. Filtrar provincias viables
  datos_modelo <- filtrar_provincias_viables(panel_data)
  
  # 3. Extraer coordenadas
  resultado_coord <- extraer_coordenadas(
    panel_resultado$caratula_2023,
    panel_resultado$caratula_2024,
    datos_modelo
  )
  datos_con_coord <- resultado_coord$datos
  coordenadas <- resultado_coord$coordenadas
  
  # 4. Crear matriz espacial
  matriz_espacial <- crear_matriz_espacial(coordenadas)
  
  # 5. Tests espaciales
  tests_espaciales <- test_autocorrelacion_espacial(datos_con_coord, matriz_espacial)
  
  # 6. Hotspots
  hotspots <- analizar_hotspots(datos_con_coord, matriz_espacial)
  
  # 7. Análisis temporal exploratorio
  analisis_temporal <- analisis_exploratorio_temporal(datos_con_coord)
  
  # 8. Modelo simple (baseline - solo 2024)
  cat("\n--- Ajustando modelo jerárquico simple (baseline, solo 2024) ---\n")
  datos_2024 <- datos_con_coord %>% filter(año == 2024)
  
  modelo_simple <- brm(
    rendimiento ~ 
      heladas * cultivo + sequias + granizadas +
      materia_organica + terrazas + rotacion_cultivos + manejo_agua +
      semilla_certificada +
      (1 | departamento/provincia),
    
    data = datos_2024,
    family = gaussian(),
    
    prior = c(
      prior(normal(0, 1000), class = Intercept),
      prior(normal(0, 500), class = b),
      prior(exponential(0.01), class = sd)
    ),
    
    chains = 4,
    iter = 2000,
    warmup = 1000,
    cores = 4,
    control = list(adapt_delta = 0.98)
  )
  
  cat("✓ Modelo jerárquico simple ajustado (baseline)\n")
  
  # 9. Modelo espacio-temporal
  cat("\n--- Ajustando modelo espacio-temporal (panel 2023-2024) ---\n")
  modelo_temporal <- ajustar_modelo_espaciotemporal(datos_con_coord)
  
  # 10. Modelo CAR (opcional)
  moran_i <- tests_espaciales$moran$estimate[1]
  
  if(usar_modelo_car && moran_i > 0.15) {
    cat("\n--- Moran's I =", round(moran_i, 3), 
        "→ Ajustando modelo espacial CAR ---\n")
    resultado_car <- ajustar_modelo_espacial_car(datos_con_coord, matriz_espacial)
    usar_car <- TRUE
  } else {
    cat("\n--- Moran's I =", round(moran_i, 3), 
        "→ Saltando modelo CAR (computacionalmente intensivo) ---\n")
    resultado_car <- list(modelo = NULL, datos_agregados = NULL)
    usar_car <- FALSE
  }
  
  # 11. Comparación de modelos
  if(usar_car) {
    # Comparación completa con CAR
    r2_simple <- bayes_R2(modelo_simple)
    r2_temporal <- bayes_R2(modelo_temporal)
    loo_simple <- loo(modelo_simple)
    loo_temporal <- loo(modelo_temporal)
    
    comparacion <- list(
      r2_simple = r2_simple,
      r2_temporal = r2_temporal,
      loo_simple = loo_simple,
      loo_temporal = loo_temporal,
      dic_car = resultado_car$modelo$modelfit[1],
      rho = resultado_car$modelo$samples$rho
    )
    
    cat("\n=== COMPARACIÓN MODELOS ===\n")
    cat("R² Simple:", round(median(r2_simple), 3), "\n")
    cat("R² Temporal:", round(median(r2_temporal), 3), "\n")
    cat("Mejora:", round(median(r2_temporal) - median(r2_simple), 4), "\n")
    cat("DIC CAR:", round(resultado_car$modelo$modelfit[1], 2), "\n")
    cat("Rho medio:", round(mean(resultado_car$modelo$samples$rho), 3), "\n\n")
    
  } else {
    # Comparación sin CAR
    r2_simple <- bayes_R2(modelo_simple)
    r2_temporal <- bayes_R2(modelo_temporal)
    loo_simple <- loo(modelo_simple)
    loo_temporal <- loo(modelo_temporal)
    
    comparacion <- list(
      r2_simple = r2_simple,
      r2_temporal = r2_temporal,
      loo_simple = loo_simple,
      loo_temporal = loo_temporal,
      dic_car = NA,
      rho = NA
    )
    
    cat("\n=== COMPARACIÓN MODELOS (sin CAR) ===\n")
    cat("R² Simple:", round(median(r2_simple), 3), "\n")
    cat("R² Temporal:", round(median(r2_temporal), 3), "\n")
    cat("Mejora:", round(median(r2_temporal) - median(r2_simple), 4), "\n\n")
  }
  
  # 12. Diagnósticos
  diagnosticos_simple <- diagnosticos_modelo(modelo_simple, "Modelo Simple")
  diagnosticos_temporal <- diagnosticos_modelo(modelo_temporal, "Modelo Temporal")
  
  # 13. Interpretación
  coeficientes_temporal <- interpretar_resultados_temporales(modelo_temporal, datos_con_coord)
  
  # 14. Análisis de cambios provinciales
  cambios_provinciales <- analizar_cambios_provinciales(datos_con_coord,
                                                        modelo_temporal,
                                                        matriz_espacial,
                                                        hotspots)
  
  # 15. Visualizaciones espaciales
  mapas <- generar_mapas_espaciales(modelo_temporal, datos_con_coord, 
                                    matriz_espacial, hotspots)
  
  # 16. Visualizaciones temporales
  vis_temporales <- crear_visualizaciones_temporales(datos_con_coord,
                                                     cambios_provinciales)
  
  # 17. Análisis por cultivo
  analisis_por_cultivo(list(datos = datos_con_coord))
  
  # 18. Reporte ejecutivo
  resultados_completos <- list(
    datos = datos_con_coord,
    coordenadas = coordenadas,
    matriz_espacial = matriz_espacial,
    tests_espaciales = tests_espaciales,
    hotspots = hotspots,
    analisis_temporal = analisis_temporal,
    modelo_simple = modelo_simple,
    modelo_temporal = modelo_temporal,
    modelo_car = resultado_car,
    comparacion = comparacion,
    coeficientes = coeficientes_temporal,
    cambios_provinciales = cambios_provinciales,
    mapas = mapas,
    visualizaciones_temporales = vis_temporales
  )
  
  reporte <- generar_reporte_ejecutivo(resultados_completos)
  
  # RESUMEN FINAL
  cat("\n==========================================================\n")
  cat("ANÁLISIS ESPACIO-TEMPORAL COMPLETO FINALIZADO\n")
  cat("==========================================================\n")
  
  cat("\n** RESUMEN DE RESULTADOS **\n")
  cat("- Observaciones totales:", nrow(datos_con_coord), "\n")
  cat("- Obs 2023:", sum(datos_con_coord$año == 2023), "\n")
  cat("- Obs 2024:", sum(datos_con_coord$año == 2024), "\n")
  cat("- Provincias:", n_distinct(datos_con_coord$provincia), "\n")
  cat("- Departamentos:", n_distinct(datos_con_coord$departamento), "\n")
  cat("- Autocorrelación espacial (Moran's I):", round(moran_i, 3), "\n")
  
  if(usar_car && !is.null(resultado_car$modelo)) {
    cat("- Modelo espacial usado: CAR\n")
    cat("- Parámetro espacial (rho):", 
        round(mean(resultado_car$modelo$samples$rho), 3), "\n")
  } else {
    cat("- Modelo espacial usado: Jerárquico espacio-temporal\n")
  }
  
  if(nrow(analisis_temporal$comparacion_años) == 2) {
    cambio_general <- ((analisis_temporal$comparacion_años$rendimiento_promedio[2] - 
                          analisis_temporal$comparacion_años$rendimiento_promedio[1]) /
                         analisis_temporal$comparacion_años$rendimiento_promedio[1]) * 100
    cat("- Cambio rendimiento 2023→2024:", round(cambio_general, 2), "%\n")
  }
  
  provincias_criticas <- cambios_provinciales$tabla_cambios %>%
    filter(prioridad_intervencion %in% c("Urgente", "Alta"))
  
  if(nrow(provincias_criticas) > 0) {
    cat("\n** PROVINCIAS DE ATENCIÓN PRIORITARIA **\n")
    cat("Provincias con prioridad Urgente/Alta:", nrow(provincias_criticas), "\n")
    print(provincias_criticas[1:min(5, nrow(provincias_criticas)), 
                              c("provincia", "departamento", "cambio_pct", "prioridad_intervencion")])
  }
  
  cat("\n** ARCHIVOS GENERADOS **\n")
  cat("\nMapas Espaciales:\n")
  cat("- mapa_vecindad.pdf\n")
  cat("- mapa_hotspots.pdf\n")
  cat("- efectos_departamento.pdf\n")
  cat("- mapa_rendimiento_hotspots.pdf\n")
  cat("- mapa_vulnerabilidad.pdf\n")
  cat("- mapa_principal_interactivo.html\n")
  
  cat("\nGráficos Temporales:\n")
  cat("- evolucion_departamento.pdf\n")
  cat("- eventos_climaticos_temporal.pdf\n")
  cat("- cambios_provincia_temporal.pdf\n")
  cat("- boxplot_temporal_departamento.pdf\n")
  cat("- violin_temporal_cultivo.pdf\n")
  cat("- scatter_vulnerabilidad_cambios.pdf\n")
  cat("- heatmap_cambios_provincia.pdf\n")
  
  cat("\nMapas de Cambios:\n")
  cat("- mapa_cambios_temporales.pdf\n")
  cat("- mapa_cambios_interactivo.html\n")
  
  cat("\nTablas:\n")
  cat("- tabla_prioridades_politicas.csv\n")
  cat("- tabla_cambios_provinciales_2023_2024.csv\n")
  
  cat("\nDiagnósticos:\n")
  cat("- diagnosticos_modelo_simple.pdf\n")
  cat("- diagnosticos_modelo_temporal.pdf\n")
  
  cat("\nReporte:\n")
  cat("- reporte_ejecutivo.txt\n")
  
  cat("\n==========================================================\n\n")
  
  return(resultados_completos)
}

# ============================================================================
# EJECUCIÓN DEL ANÁLISIS COMPLETO
# ============================================================================

# EJECUTAR TODO EL ANÁLISIS ESPACIO-TEMPORAL
resultados <- ejecutar_analisis_espaciotemporal_completo(usar_modelo_car = TRUE)

# Para acceder a resultados específicos:
# - resultados$tests_espaciales$moran           # Test de Moran
# - resultados$hotspots                          # Análisis de hotspots
# - resultados$analisis_temporal                 # Análisis temporal exploratorio
# - resultados$modelo_simple                     # Modelo jerárquico simple
# - resultados$modelo_temporal                   # Modelo espacio-temporal
# - resultados$modelo_car$modelo                 # Modelo espacial CAR
# - resultados$cambios_provinciales              # Cambios por provincia
# - resultados$mapas$tabla_politicas             # Tabla de prioridades
# - resultados$comparacion                       # Comparación de modelos
                                
                                      