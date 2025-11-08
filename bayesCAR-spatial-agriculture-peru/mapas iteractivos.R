# ============================================================================
# MAPAS INTERACTIVOS LEAFLET
# Análisis Espacial - Sur del Perú
# ============================================================================

library(haven)
library(dplyr)
library(sf)
library(leaflet)
library(RColorBrewer)
library(htmlwidgets)

# Asumiendo que ya ejecutaste: resultados <- ejecutar_analisis_espacial_completo(2024)

# ============================================================================
# 1. PREPARAR DATOS ESPACIALES
# ============================================================================

# Usar coordenadas que ya están en resultados$datos (fueron agregadas en el análisis)
# Esto evita el problema de muchos-a-muchos

# Verificar si ya tenemos coordenadas en los datos
if("latitud" %in% names(resultados$datos) & "longitud" %in% names(resultados$datos)) {
  cat("✓ Usando coordenadas ya existentes en los datos\n")
  datos_sf <- st_as_sf(resultados$datos,
                       coords = c("longitud", "latitud"),
                       crs = 4326)
} else {
  cat("⚠ Agregando coordenadas desde carátula...\n")
  
  # Opción alternativa: usar coordenadas promedio por provincia
  caratula <- read_sav("ENA_2024/CARATULA.sav")
  
  coords_promedio <- caratula %>%
    filter(!is.na(LATITUD), !is.na(LONGITUD)) %>%
    group_by(NOMBREDD, NOMBREPV) %>%
    summarise(
      latitud = mean(LATITUD, na.rm = TRUE),
      longitud = mean(LONGITUD, na.rm = TRUE),
      .groups = "drop"
    )
  
  datos_con_coords <- resultados$datos %>%
    left_join(coords_promedio,
              by = c("departamento" = "NOMBREDD", "provincia" = "NOMBREPV")) %>%
    filter(!is.na(latitud), !is.na(longitud))
  
  datos_sf <- st_as_sf(datos_con_coords,
                       coords = c("longitud", "latitud"),
                       crs = 4326)
}

cat("Datos espaciales preparados:", nrow(datos_sf), "observaciones\n")

# ============================================================================
# 2. MAPA 1: DISTRIBUCIÓN DE CULTIVOS
# ============================================================================

# Tomar muestra balanceada por cultivo
set.seed(123)  # Para reproducibilidad

# Contar cuántos hay de cada cultivo
n_papa <- sum(datos_sf$cultivo == "PAPA BLANCA")
n_maiz <- sum(datos_sf$cultivo == "MAIZ AMILACEO")

cat("Total por cultivo - Papa:", n_papa, "| Maíz:", n_maiz, "\n")

# Tomar 400 de cada cultivo (o todos si hay menos)
muestra_papa <- datos_sf %>%
  filter(cultivo == "PAPA BLANCA") %>%
  slice_head(n = min(400, n_papa))

muestra_maiz <- datos_sf %>%
  filter(cultivo == "MAIZ AMILACEO") %>%
  slice_head(n = min(400, n_maiz))

# Combinar
muestra_cultivos <- rbind(muestra_papa, muestra_maiz)

cat("Muestra para mapa - Papa:", sum(muestra_cultivos$cultivo == "PAPA BLANCA"),
    "| Maíz:", sum(muestra_cultivos$cultivo == "MAIZ AMILACEO"), "\n")

# Paleta de colores para cultivos
pal_cultivo <- colorFactor(
  palette = c("PAPA BLANCA" = "#E69F00", "MAIZ AMILACEO" = "#56B4E9"),
  domain = c("PAPA BLANCA", "MAIZ AMILACEO")  # ← Especificar dominios explícitamente
)

# Crear mapa
mapa_cultivos <- leaflet(muestra_cultivos) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    radius = 4,
    fillColor = ~pal_cultivo(cultivo),
    color = "white",
    weight = 0.5,
    opacity = 0.8,
    fillOpacity = 0.6,
    popup = ~paste0(
      "<b>Cultivo:</b> ", cultivo, "<br>",
      "<b>Rendimiento:</b> ", round(rendimiento, 0), " kg/ha<br>",
      "<b>Provincia:</b> ", provincia, "<br>",
      "<b>Departamento:</b> ", departamento
    )
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal_cultivo,
    values = c("PAPA BLANCA", "MAIZ AMILACEO"),  # ← Valores explícitos
    title = "Tipo de Cultivo",
    opacity = 1
  ) %>%
  addMiniMap(position = "bottomleft", width = 100, height = 100)

# Guardar mapa
saveWidget(mapa_cultivos, "mapa_interactivo_cultivos.html", selfcontained = TRUE)
cat("✓ Mapa de cultivos guardado: mapa_interactivo_cultivos.html\n")

# Mostrar en RStudio
mapa_cultivos

# ============================================================================
# 3. MAPA 2: RENDIMIENTO (gradiente de colores)
# ============================================================================

# Tomar muestra estratificada por departamento
indices <- seq(1, nrow(datos_sf), by = 3)  # Tomar cada 3er registro
muestra_rendimiento <- datos_sf[indices, ]

# Paleta de colores para rendimiento (continua)
pal_rendimiento <- colorNumeric(
  palette = "RdYlGn",  # Rojo (bajo) → Amarillo → Verde (alto)
  domain = muestra_rendimiento$rendimiento
)

mapa_rendimiento <- leaflet(muestra_rendimiento) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    radius = 5,
    fillColor = ~pal_rendimiento(rendimiento),
    color = "white",
    weight = 0.8,
    opacity = 0.9,
    fillOpacity = 0.7,
    popup = ~paste0(
      "<b>Rendimiento:</b> ", round(rendimiento, 0), " kg/ha<br>",
      "<b>Cultivo:</b> ", cultivo, "<br>",
      "<b>Provincia:</b> ", provincia, "<br>",
      "<b>Departamento:</b> ", departamento, "<br>",
      "<b>Eventos climáticos:</b><br>",
      "  - Heladas: ", ifelse(heladas == 1, "Sí", "No"), "<br>",
      "  - Sequías: ", ifelse(sequias == 1, "Sí", "No"), "<br>",
      "  - Granizadas: ", ifelse(granizadas == 1, "Sí", "No")
    )
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal_rendimiento,
    values = ~rendimiento,
    title = "Rendimiento<br>(kg/ha)",
    opacity = 1,
    labFormat = labelFormat(suffix = " kg/ha")
  ) %>%
  addMiniMap(position = "bottomleft")

saveWidget(mapa_rendimiento, "mapa_interactivo_rendimiento.html", selfcontained = TRUE)
cat("✓ Mapa de rendimiento guardado: mapa_interactivo_rendimiento.html\n")

mapa_rendimiento

# ============================================================================
# 4. MAPA 3: EVENTOS CLIMÁTICOS (múltiples capas)
# ============================================================================

# Crear subconjuntos por evento
heladas_idx <- which(datos_sf$heladas == 1)
heladas_sf <- datos_sf[heladas_idx[1:min(300, length(heladas_idx))], ]

sequias_idx <- which(datos_sf$sequias == 1)
sequias_sf <- datos_sf[sequias_idx[1:min(300, length(sequias_idx))], ]

granizadas_idx <- which(datos_sf$granizadas == 1)
granizadas_sf <- datos_sf[granizadas_idx[1:min(300, length(granizadas_idx))], ]

mapa_eventos <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  # Capa de heladas
  addCircleMarkers(
    data = heladas_sf,
    radius = 4,
    fillColor = "#4575b4",  # Azul
    color = "white",
    weight = 0.5,
    opacity = 0.8,
    fillOpacity = 0.5,
    group = "Heladas",
    popup = ~paste0(
      "<b>Evento:</b> Heladas<br>",
      "<b>Rendimiento:</b> ", round(rendimiento, 0), " kg/ha<br>",
      "<b>Provincia:</b> ", provincia
    )
  ) %>%
  
  # Capa de sequías
  addCircleMarkers(
    data = sequias_sf,
    radius = 4,
    fillColor = "#d73027",  # Rojo
    color = "white",
    weight = 0.5,
    opacity = 0.8,
    fillOpacity = 0.5,
    group = "Sequías",
    popup = ~paste0(
      "<b>Evento:</b> Sequías<br>",
      "<b>Rendimiento:</b> ", round(rendimiento, 0), " kg/ha<br>",
      "<b>Provincia:</b> ", provincia
    )
  ) %>%
  
  # Capa de granizadas
  addCircleMarkers(
    data = granizadas_sf,
    radius = 4,
    fillColor = "#fee090",  # Amarillo
    color = "white",
    weight = 0.5,
    opacity = 0.8,
    fillOpacity = 0.5,
    group = "Granizadas",
    popup = ~paste0(
      "<b>Evento:</b> Granizadas<br>",
      "<b>Rendimiento:</b> ", round(rendimiento, 0), " kg/ha<br>",
      "<b>Provincia:</b> ", provincia
    )
  ) %>%
  
  # Control de capas
  addLayersControl(
    overlayGroups = c("Heladas", "Sequías", "Granizadas"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  # Leyenda manual
  addLegend(
    position = "bottomright",
    colors = c("#4575b4", "#d73027", "#fee090"),
    labels = c("Heladas", "Sequías", "Granizadas"),
    title = "Eventos Climáticos",
    opacity = 1
  ) %>%
  
  addMiniMap(position = "bottomleft")

saveWidget(mapa_eventos, "mapa_interactivo_eventos.html", selfcontained = TRUE)
cat("✓ Mapa de eventos guardado: mapa_interactivo_eventos.html\n")

mapa_eventos

# ============================================================================
# 5. MAPA 4: HOTSPOTS/COLDSPOTS (centroides provinciales)
# ============================================================================

# Calcular centroides de provincias con información de hotspots
hotspots_sf <- resultados$hotspots %>%
  left_join(
    resultados$coordenadas %>% 
      select(provincia = NOMBREPV, latitud, longitud),
    by = "provincia"
  ) %>%
  filter(!is.na(latitud), !is.na(longitud)) %>%  # ← AGREGADO
  st_as_sf(coords = c("longitud", "latitud"), crs = 4326)

# Paleta para hotspots
pal_hotspots <- colorFactor(
  palette = c(
    "Cold spot 99%" = "#d7191c",
    "Cold spot 95%" = "#fdae61",
    "Cold spot 90%" = "#fee08b",
    "No significativo" = "gray70",
    "Hot spot 90%" = "#d9ef8b",
    "Hot spot 95%" = "#a6d96a",
    "Hot spot 99%" = "#1a9641"
  ),
  domain = hotspots_sf$tipo_cluster
)

mapa_hotspots <- leaflet(hotspots_sf) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    radius = ~scales::rescale(rendimiento_medio, to = c(5, 20)),  # Tamaño proporcional
    fillColor = ~pal_hotspots(tipo_cluster),
    color = "white",
    weight = 2,
    opacity = 1,
    fillOpacity = 0.8,
    popup = ~paste0(
      "<b>Provincia:</b> ", provincia, "<br>",
      "<b>Tipo de cluster:</b> ", tipo_cluster, "<br>",
      "<b>Rendimiento medio:</b> ", round(rendimiento_medio, 0), " kg/ha<br>",
      "<b>Estadístico Gi*:</b> ", round(Gi_star, 3), "<br>",
      "<b>Observaciones:</b> ", n_obs, "<br>",
      "<b>Proporción heladas:</b> ", round(prop_heladas * 100, 1), "%<br>",
      "<b>Proporción sequías:</b> ", round(prop_sequias * 100, 1), "%"
    ),
    label = ~provincia
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal_hotspots,
    values = ~tipo_cluster,
    title = "Cluster Espacial<br>(Getis-Ord Gi*)",
    opacity = 1
  ) %>%
  addMiniMap(position = "bottomleft")

saveWidget(mapa_hotspots, "mapa_interactivo_hotspots.html", selfcontained = TRUE)
cat("✓ Mapa de hotspots guardado: mapa_interactivo_hotspots.html\n")

mapa_hotspots

# ============================================================================
# 6. MAPA 5: VULNERABILIDAD CLIMÁTICA (índice compuesto)
# ============================================================================

# Calcular índice de vulnerabilidad por provincia
vulnerabilidad_prov <- datos_sf %>%
  st_drop_geometry() %>%
  group_by(provincia, departamento) %>%
  summarise(
    rendimiento_medio = mean(rendimiento),
    indice_vulnerabilidad = mean(heladas) + mean(sequias) + mean(granizadas),
    prop_heladas = mean(heladas),
    prop_sequias = mean(sequias),
    prop_granizadas = mean(granizadas),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  left_join(
    resultados$coordenadas %>% 
      select(provincia = NOMBREPV, latitud, longitud),
    by = "provincia"
  ) %>%
  filter(!is.na(latitud), !is.na(longitud)) %>%  # ← AGREGADO
  st_as_sf(coords = c("longitud", "latitud"), crs = 4326)

# Paleta para vulnerabilidad
pal_vulnerabilidad <- colorBin(
  palette = "YlOrRd",  # Amarillo → Naranja → Rojo
  domain = vulnerabilidad_prov$indice_vulnerabilidad,
  bins = c(0, 0.5, 1.0, 1.5, 2.0, 3.0),
  pretty = FALSE
)

mapa_vulnerabilidad <- leaflet(vulnerabilidad_prov) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    radius = ~scales::rescale(indice_vulnerabilidad, to = c(5, 20)),
    fillColor = ~pal_vulnerabilidad(indice_vulnerabilidad),
    color = "white",
    weight = 2,
    opacity = 1,
    fillOpacity = 0.8,
    popup = ~paste0(
      "<b>Provincia:</b> ", provincia, "<br>",
      "<b>Departamento:</b> ", departamento, "<br>",
      "<b>Índice de vulnerabilidad:</b> ", round(indice_vulnerabilidad, 2), "<br>",
      "<b>Rendimiento medio:</b> ", round(rendimiento_medio, 0), " kg/ha<br>",
      "<hr>",
      "<b>Desglose de eventos:</b><br>",
      "  • Heladas: ", round(prop_heladas * 100, 1), "%<br>",
      "  • Sequías: ", round(prop_sequias * 100, 1), "%<br>",
      "  • Granizadas: ", round(prop_granizadas * 100, 1), "%<br>",
      "<b>Productores:</b> ", n_obs
    ),
    label = ~provincia
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal_vulnerabilidad,
    values = ~indice_vulnerabilidad,
    title = "Índice de<br>Vulnerabilidad<br>Climática",
    opacity = 1,
    labFormat = labelFormat(
      prefix = "",
      between = " - "
    )
  ) %>%
  addMiniMap(position = "bottomleft")

saveWidget(mapa_vulnerabilidad, "mapa_interactivo_vulnerabilidad.html", selfcontained = TRUE)
cat("✓ Mapa de vulnerabilidad guardado: mapa_interactivo_vulnerabilidad.html\n")

mapa_vulnerabilidad

# ============================================================================
# 7. MAPA 6: COMPARACIÓN LADO A LADO (Split map)
# ============================================================================

# Mapa lado izquierdo: Rendimiento
mapa_izq <- leaflet(muestra_rendimiento) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    radius = 4,
    fillColor = ~pal_rendimiento(rendimiento),
    color = "white",
    weight = 0.5,
    opacity = 0.8,
    fillOpacity = 0.6,
    popup = ~paste0("<b>Rendimiento:</b> ", round(rendimiento, 0), " kg/ha")
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal_rendimiento,
    values = ~rendimiento,
    title = "Rendimiento<br>(kg/ha)"
  )

# ============================================================================
# 8. MAPA 7: TODAS LAS PROVINCIAS CON ESTADÍSTICAS
# ============================================================================

# Resumen completo por provincia
resumen_provincias <- datos_sf %>%
  st_drop_geometry() %>%
  group_by(provincia, departamento) %>%
  summarise(
    rendimiento_medio = mean(rendimiento),
    rendimiento_sd = sd(rendimiento),
    n_obs = n(),
    prop_papa = mean(cultivo == "PAPA BLANCA"),
    prop_heladas = mean(heladas),
    prop_sequias = mean(sequias),
    prop_granizadas = mean(granizadas),
    prop_materia_org = mean(materia_organica),
    prop_terrazas = mean(terrazas),
    .groups = "drop"
  ) %>%
  left_join(
    resultados$coordenadas %>% 
      select(provincia = NOMBREPV, latitud, longitud),
    by = "provincia"
  ) %>%
  left_join(
    resultados$hotspots %>% select(provincia, tipo_cluster, Gi_star),
    by = "provincia"
  ) %>%
  filter(!is.na(latitud), !is.na(longitud)) %>%  # ← AGREGADO
  st_as_sf(coords = c("longitud", "latitud"), crs = 4326)

# Crear iconos personalizados según departamento
iconos_dept <- awesomeIconList(
  "PUNO" = makeAwesomeIcon(icon = "star", markerColor = "green", library = "fa"),
  "CUSCO" = makeAwesomeIcon(icon = "triangle", markerColor = "red", library = "fa"),
  "AREQUIPA" = makeAwesomeIcon(icon = "circle", markerColor = "orange", library = "fa"),
  "TACNA" = makeAwesomeIcon(icon = "square", markerColor = "blue", library = "fa"),
  "MOQUEGUA" = makeAwesomeIcon(icon = "diamond", markerColor = "purple", library = "fa")
)

mapa_completo <- leaflet(resumen_provincias) %>%
  addProviderTiles("CartoDB.Positron", group = "Base clara") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satélite") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetMap") %>%
  
  addAwesomeMarkers(
    icon = ~iconos_dept[departamento],
    popup = ~paste0(
      "<div style='width:250px'>",
      "<h4 style='margin-bottom:5px; color:#2c3e50'>", provincia, "</h4>",
      "<p style='margin:2px; color:#7f8c8d'><b>Departamento:</b> ", departamento, "</p>",
      "<hr style='margin:5px 0'>",
      "<p style='margin:2px'><b>📊 Rendimiento medio:</b> ", 
      round(rendimiento_medio, 0), " ± ", round(rendimiento_sd, 0), " kg/ha</p>",
      "<p style='margin:2px'><b>📍 Cluster espacial:</b> ", tipo_cluster, "</p>",
      "<p style='margin:2px'><b>🔢 Productores:</b> ", n_obs, "</p>",
      "<hr style='margin:5px 0'>",
      "<p style='margin:2px'><b>🌾 Cultivos:</b></p>",
      "<p style='margin:2px; margin-left:10px'>Papa: ", round(prop_papa * 100, 1), "%</p>",
      "<p style='margin:2px; margin-left:10px'>Maíz: ", round((1-prop_papa) * 100, 1), "%</p>",
      "<hr style='margin:5px 0'>",
      "<p style='margin:2px'><b>🌡️ Eventos climáticos:</b></p>",
      "<p style='margin:2px; margin-left:10px'>❄️ Heladas: ", round(prop_heladas * 100, 1), "%</p>",
      "<p style='margin:2px; margin-left:10px'>☀️ Sequías: ", round(prop_sequias * 100, 1), "%</p>",
      "<p style='margin:2px; margin-left:10px'>🌨️ Granizadas: ", round(prop_granizadas * 100, 1), "%</p>",
      "<hr style='margin:5px 0'>",
      "<p style='margin:2px'><b>🌱 Prácticas agrícolas:</b></p>",
      "<p style='margin:2px; margin-left:10px'>Materia orgánica: ", round(prop_materia_org * 100, 1), "%</p>",
      "<p style='margin:2px; margin-left:10px'>Terrazas: ", round(prop_terrazas * 100, 1), "%</p>",
      "</div>"
    ),
    label = ~provincia,
    labelOptions = labelOptions(
      style = list("font-weight" = "bold", "font-size" = "12px")
    )
  ) %>%
  
  addLayersControl(
    baseGroups = c("Base clara", "Satélite", "OpenStreetMap"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  addLegend(
    position = "bottomright",
    colors = c("#28a745", "#dc3545", "#fd7e14", "#007bff", "#6f42c1"),
    labels = c("Puno", "Cusco", "Arequipa", "Tacna", "Moquegua"),
    title = "Departamento",
    opacity = 1
  ) %>%
  
  addScaleBar(position = "bottomleft") %>%
  addMiniMap(position = "topleft", width = 120, height = 120)

saveWidget(mapa_completo, "mapa_interactivo_completo.html", selfcontained = TRUE)
cat("✓ Mapa completo guardado: mapa_interactivo_completo.html\n")

mapa_completo

# ============================================================================
# MAPA 8 ALTERNATIVO: DENSIDAD CON CÍRCULOS (sin leaflet.extras)
# ============================================================================

indices_densidad <- seq(1, nrow(datos_sf), by = 2)
muestra_densidad <- datos_sf[indices_densidad, ]

# Paleta de colores según rendimiento
pal_densidad <- colorNumeric(
  palette = c("#313695", "#4575b4", "#fee090", "#d73027"),
  domain = muestra_densidad$rendimiento
)

mapa_densidad <- leaflet(muestra_densidad) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addCircleMarkers(
    radius = 3,
    fillColor = ~pal_densidad(rendimiento),
    color = NA,  # Sin borde
    fillOpacity = 0.4,  # Transparente para efecto de densidad
    popup = ~paste0(
      "<b>Rendimiento:</b> ", round(rendimiento, 0), " kg/ha<br>",
      "<b>Provincia:</b> ", provincia
    )
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal_densidad,
    values = ~rendimiento,
    title = "Rendimiento<br>(kg/ha)",
    opacity = 1
  ) %>%
  addMiniMap(position = "bottomleft")

saveWidget(mapa_densidad, "mapa_densidad_alternativo.html", selfcontained = TRUE)
cat("✓ Mapa de densidad guardado: mapa_densidad_alternativo.html\n")
mapa_densidad
# ============================================================================
# RESUMEN FINAL
# ============================================================================
# ============================================================================
# MAPA HÍBRIDO: HEATMAP + PUNTOS INDIVIDUALES
# ============================================================================

library(sf)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)

# Preparar datos
indices_densidad <- seq(1, nrow(datos_sf), by = 2)
muestra_densidad <- datos_sf[indices_densidad, ]

# Extraer coordenadas
coords <- st_coordinates(muestra_densidad)
muestra_densidad$lng <- coords[, 1]
muestra_densidad$lat <- coords[, 2]

# Paleta para puntos
pal_densidad <- colorNumeric(
  palette = c("#313695", "#4575b4", "#fee090", "#d73027"),
  domain = muestra_densidad$rendimiento
)

# MAPA HÍBRIDO: Heatmap + Puntos con control de capas
mapa_completo <- leaflet(muestra_densidad) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  
  # CAPA 1: Heatmap (mapa de calor)
  addHeatmap(
    lng = ~lng,
    lat = ~lat,
    intensity = ~rendimiento/max(rendimiento),
    blur = 25,
    max = 0.6,
    radius = 20,
    group = "Heatmap"  # ← Asignar a grupo
  ) %>%
  
  # CAPA 2: Puntos individuales
  addCircleMarkers(
    radius = 3,
    fillColor = ~pal_densidad(rendimiento),
    color = "white",
    weight = 0.3,
    fillOpacity = 0.7,
    group = "Puntos",  # ← Asignar a grupo
    popup = ~paste0(
      "<b>Rendimiento:</b> ", round(rendimiento, 0), " kg/ha<br>",
      "<b>Cultivo:</b> ", cultivo, "<br>",
      "<b>Provincia:</b> ", provincia, "<br>",
      "<b>Departamento:</b> ", departamento
    )
  ) %>%
  
  # Control para activar/desactivar capas
  addLayersControl(
    overlayGroups = c("Heatmap", "Puntos"),
    options = layersControlOptions(collapsed = FALSE),
    position = "topleft"
  ) %>%
  
  # Leyenda
  addLegend(
    position = "bottomright",
    pal = pal_densidad,
    values = ~rendimiento,
    title = "Rendimiento<br>(kg/ha)",
    opacity = 1
  ) %>%
  
  addMiniMap(position = "bottomleft")

# Guardar
saveWidget(mapa_completo, "mapa_hibrido_heatmap_puntos.html", selfcontained = TRUE)
cat("✓ Mapa híbrido guardado: mapa_hibrido_heatmap_puntos.html\n")

mapa_completo
