title: "El lenguaje de programación R - Tarea 03 Leaflet"
author: "Dania Zúñiga"
date: "2020-11-04"
output:
  html_document:
  theme: readable    
toc: true
toc_depth: 3
toc_float:
  collapsed: false
smooth_scroll: false  

# Paquetes

# Paquete para manejo de datos vectoriales
library(sf)

# Paquete de Tidyverse para manipulación de datos
library(dplyr)

# Paquete con conjuntos de datos geoespaciales
library(spData)

library(tidyr)
# Paquete para mapas en la Web
library(leaflet)

# Capa de cantones del SNIT

# URL base del servicio WFS IGN 1:5mil
url_base_wfs_ign_5mil <- "http://geos.snitcr.go.cr/be/IGN_5/wfs?"

# URL base del servicio WFS IGN 1:200mil
url_base_wfs_ign_200mil <- "http://geos.snitcr.go.cr/be/IGN_200/wfs?"

# URL de las solicitudes de las capas

solicitud_cantones_wfs <- 
  "request=GetFeature&service=WFS&version=2.0.0&typeName=IGN_5:limitecantonal_5k&outputFormat=application/json"

# Recuperación y simplificación de las capas
# Capa de cantones

cr_cantones <-
  st_read(paste0(url_base_wfs_ign_5mil, solicitud_cantones_wfs)) %>%
  st_simplify(dTolerance = 1000)

# Capas del covid al 31 de octubre del 2020

library(readr)

casospositivos <- read_delim("10_31_CSV_POSITIVOS.csv", 
                             ";", escape_double = FALSE, trim_ws = TRUE)
View(casospositivos)

casosactivos <- read_delim("10_31_CSV_ACTIVOS.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)
View(casosactivos)

casosrecuperados <- read_delim("10_31_CSV_RECUP.csv", 
                               ";", escape_double = FALSE, trim_ws = TRUE)
View(casosrecuperados)

casosfallecidos <- read_delim("10_31_CSV_FALLECIDOS.csv", 
                              ";", escape_double = FALSE, trim_ws = TRUE)
View(casosfallecidos)




# Data frame con fechas en las filas
casospositivos_cantones <-
  casospositivos %>%
  pivot_longer(
    cols = c(-cod_provin, -provincia, -cod_canton, -canton), 
    names_to = "fecha", 
    values_to = "activos"
  )

casosactivos_cantones <-
  casosactivos %>%
  pivot_longer(
    cols = c(-cod_provin, -provincia, -cod_canton, -canton), 
    names_to = "fecha", 
    values_to = "activos"
  )

casosrecuperados_cantones <-
  casosrecuperados %>%
  pivot_longer(
    cols = c(-cod_provin, -provincia, -cod_canton, -canton), 
    names_to = "fecha", 
    values_to = "activos"
  )

casosfallecidos_cantones <-
  casosfallecidos %>%
  pivot_longer(
    cols = c(-cod_provin, -provincia, -cod_canton, -canton), 
    names_to = "fecha", 
    values_to = "activos"
  )


# Cambio de tipo de la columna "fecha"
#casospositivos_cantones <- as.Date(casospositivos_cantones$fecha, "X%d.%m.%Y")


# Data frame de casos positivos por cantón en la última fecha
casospositivos_cantones_ultima_fecha <- 
  casospositivos_cantones %>%
  filter(fecha == max(fecha, na.rm = TRUE)) %>%
  select(cod_canton, activos)


# Objeto sf de casos positivos en cantones en la última fecha
casospositivos_cantones_ultima_fecha <-
  left_join(cr_cantones, casospositivos_cantones_ultima_fecha, by = c('cod_canton')) %>%
  arrange(desc(activos))


# Mapas de corpletas de casos positivos en cantones

  bins <- c(0, 100, 500, 1000, 2000, Inf)
  paleta_azul <- colorBin("YlOrRd", domain = casospositivos_cantones_ultima_fecha$activos, bins = bins)
  
  leaflet(casospositivos_cantones_ultima_fecha) %>% 
    fitBounds(lng1 = -86, lng2 = -82, lat1 = 8, lat2 = 11) %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap") %>%
    addPolygons(fillColor = ~paleta_azul(activos), stroke=T, fillOpacity = 1,
                color="black", weight=0.2, opacity= 0.5,
                group = "Cantones",
                popup = paste(
                  "Provincia: ", casospositivos_cantones_ultima_fecha$provincia, "<br>",
                  "Cantón: ",casospositivos_cantones_ultima_fecha$canton, "<br>",
                  "activos: ", casospositivos_cantones_ultima_fecha$activos
                )
    ) %>%
    addLegend("bottomright", pal = paleta_azul, values = ~activos,
              title = "Casos activos",
              opacity = 1
    ) %>%  
    addLayersControl(
      baseGroups = c("OpenStreetMap"),
      overlayGroups = c("cantones"),
      options = layersControlOptions(collapsed = TRUE)    
    ) %>%  
    addMiniMap(
      toggleDisplay = TRUE,
      position = "bottomleft",
      tiles = providers$OpenStreetMap.Mapnik
    )


# casos activos
# Cambio de tipo de la columna "fecha"
#casosactivos_cantones <- as.Date(casosactivos_cantones$fecha, "X%d.%m.%Y")


# Data frame de casos activos por cantón en la última fecha
casosactivos_cantones_ultima_fecha <- 
  casosactivos_cantones %>%
  filter(fecha == max(fecha, na.rm = TRUE)) %>%
  select(cod_canton, activos)

library(leaflet)
# Objeto sf de casos activos en cantones en la última fecha
casosactivos_cantones_ultima_fecha <-
  left_join(cr_cantones, casosactivos_cantones_ultima_fecha, by = c('cod_canton')) %>%
  arrange(desc(activos))


# Mapas de corpletas de casos activos en cantones

bins <- c(0, 100, 500, 1000, 2000, Inf)
paleta_azul <- colorBin("YlOrRd", domain = casosactivos_cantones_ultima_fecha$activos, bins = bins)

leaflet(casosactivos_cantones_ultima_fecha) %>% 
  fitBounds(lng1 = -86, lng2 = -82, lat1 = 8, lat2 = 11) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap") %>%
  addPolygons(fillColor = ~paleta_azul(activos), stroke=T, fillOpacity = 1,
              color="black", weight=0.2, opacity= 0.5,
              group = "Cantones",
              popup = paste(
                "Provincia: ", casosactivos_cantones_ultima_fecha$provincia, "<br>",
                "Cantón: ",casosactivos_cantones_ultima_fecha$canton, "<br>",
                "activos: ", casosactivos_cantones_ultima_fecha$activos
              )
  ) %>%
  addLegend("bottomright", pal = paleta_azul, values = ~activos,
            title = "Casos activos",
            opacity = 1
  ) %>%  
  addLayersControl(
    baseGroups = c("OpenStreetMap"),
    overlayGroups = c("Cantones"),
    options = layersControlOptions(collapsed = TRUE)    
  ) %>%  
  addMiniMap(
    toggleDisplay = TRUE,
    position = "bottomleft",
    tiles = providers$OpenStreetMap.Mapnik
  )



# casos Recuperados
# Cambio de tipo de la columna "fecha"
#casosrecuperadoss_cantones <- as.Date(casosrecuperados_cantones$fecha, "X%d.%m.%Y")


# Data frame de casos recuperados por cantón en la última fecha
casosrecuperados_cantones_ultima_fecha <- 
  casosrecuperados_cantones %>%
  filter(fecha == max(fecha, na.rm = TRUE)) %>%
  select(cod_canton, activos)

library(leaflet)
# Objeto sf de casos recuperados en cantones en la última fecha
casosrecuperados_cantones_ultima_fecha <-
  left_join(cr_cantones, casosrecuperados_cantones_ultima_fecha, by = c('cod_canton')) %>%
  arrange(desc(activos))


# Mapas de corpletas de casos recuperados en cantones

bins <- c(0, 100, 500, 1000, 2000, Inf)
paleta_azul <- colorBin("YlOrRd", domain = casosrecuperados_cantones_ultima_fecha$activos, bins = bins)

leaflet(casosrecuperados_cantones_ultima_fecha) %>% 
  fitBounds(lng1 = -86, lng2 = -82, lat1 = 8, lat2 = 11) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap") %>%
  addPolygons(fillColor = ~paleta_azul(activos), stroke=T, fillOpacity = 1,
              color="black", weight=0.2, opacity= 0.5,
              group = "Cantones",
              popup = paste(
                "Provincia: ", casosrecuperados_cantones_ultima_fecha$provincia, "<br>",
                "Cantón: ",casosrecuperados_cantones_ultima_fecha$canton, "<br>",
                "activos: ", casosrecuperados_cantones_ultima_fecha$activos
              )
  ) %>%
  addLegend("bottomright", pal = paleta_azul, values = ~activos,
            title = "Casos activos",
            opacity = 1
  ) %>%  
  addLayersControl(
    baseGroups = c("OpenStreetMap"),
    overlayGroups = c("Cantones"),
    options = layersControlOptions(collapsed = TRUE)    
  ) %>%  
  addMiniMap(
    toggleDisplay = TRUE,
    position = "bottomleft",
    tiles = providers$OpenStreetMap.Mapnik
  )


# casos FALLECIDOS
# Cambio de tipo de la columna "fecha"
#casosfallecifos_cantones <- as.Date(casosfallecidos_cantones$fecha, "X%d.%m.%Y")

# Data frame de casos fallecidos por cantón en la última fecha
casosfallecidos_cantones_ultima_fecha <- 
  casosfallecidos_cantones %>%
  filter(fecha == max(fecha, na.rm = TRUE)) %>%
  select(cod_canton, activos)

library(leaflet)
# Objeto sf de casos fallecidos en cantones en la última fecha
casosfallecidos_cantones_ultima_fecha <-
  left_join(cr_cantones, casosfallecidos_cantones_ultima_fecha, by = c('cod_canton')) %>%
  arrange(desc(activos))


# Mapas de corpletas de casos recuperados en cantones

bins <- c(0, 100, 500, 1000, 2000, Inf)
paleta_azul <- colorBin("YlOrRd", domain = casosfallecidos_cantones_ultima_fecha$activos, bins = bins)

leaflet(casosfallecidos_cantones_ultima_fecha) %>% 
  fitBounds(lng1 = -86, lng2 = -82, lat1 = 8, lat2 = 11) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap") %>%
  addPolygons(fillColor = ~paleta_azul(activos), stroke=T, fillOpacity = 1,
              color="black", weight=0.2, opacity= 0.5,
              group = "Cantones",
              popup = paste(
                "Provincia: ", casosfallecidos_cantones_ultima_fecha$provincia, "<br>",
                "Cantón: ",casosfallecidos_cantones_ultima_fecha$canton, "<br>",
                "activos: ", casosfallecidos_cantones_ultima_fecha$activos
              )
  ) %>%
  addLegend("bottomright", pal = paleta_azul, values = ~activos,
            title = "Casos activos",
            opacity = 1
  ) %>%  
  addLayersControl(
    baseGroups = c("OpenStreetMap"),
    overlayGroups = c("Cantones"),
    options = layersControlOptions(collapsed = TRUE)    
  ) %>%  
  addMiniMap(
    toggleDisplay = TRUE,
    position = "bottomleft",
    tiles = providers$OpenStreetMap.Mapnik
  )
