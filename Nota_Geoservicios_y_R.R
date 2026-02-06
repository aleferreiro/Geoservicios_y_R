
# 1. Conexión a los geoservicios WFS de IDECOR desde R --------------------

# Paquetes necesarios ---------------------------------------------------
install.packages(sf)
library(sf)
install.packages(httr)
library(httr)
install.packages(ows4R)
library(ows4R)  # Cliente para servicios OGC


# URL del servicio WFS de IDECOR
wfs_url <- "https://idecor-ws.mapascordoba.gob.ar/geoserver/idecor/wfs"


# Crear el cliente WFS
wfs_client <- WFSClient$new(wfs_url, serviceVersion = "2.0.0")


# Listar capas disponibles (GetCapabilities)
feature_types <- wfs_client$getFeatureTypes(pretty = TRUE)
print(feature_types)


# 2. Descargar capas de interés -------------------------------------------

# Límite provincial de Córdoba -----------------------------------------
provincia_url <- parse_url(wfs_url)
provincia_url$query <- list(
  service = "WFS",
  version = "2.0.0",
  request = "GetFeature",
  typeNames = "idecor:provincia",
  outputFormat = "application/json"
)
provincia <- read_sf(build_url(provincia_url))


# Áreas quemadas 2024 ---------------------------------------------------
incendios_2024_url <- parse_url(wfs_url)
incendios_2024_url$query <- list(
  service = "WFS",
  version = "2.0.0",
  request = "GetFeature",
  typeNames = "idecor:area_quemada_2024",
  outputFormat = "application/json"
)
incendios_2024 <- read_sf(build_url(incendios_2024_url))


# 3. Visualización interactiva con leaflet --------------------------------
install.packages(leaflet)
library(leaflet)


# Reproyección a WGS84 (requerida por leaflet)
provincia_ll <- st_transform(provincia, 4326)
incendios_2024_ll <- st_transform(incendios_2024, 4326)


leaflet() %>%
  addTiles() %>%
  addPolygons(data = provincia_ll,
              color = "black",
              weight = 1,
              fill = FALSE) %>%
  addPolygons(data = incendios_2024_ll,
              color = "red",
              fillOpacity = 0.5,
              popup = "Área quemada 2024")


# 4. Visualizacion con tmap -----------------------------------------------
install.packages(tmap)
library(tmap)

tmap_mode("view")  # usar "plot" para salida estática

tm_shape(provincia) +
  tm_borders(col = "black") +
  tm_shape(incendios_2024) +
  tm_fill(col = "red", alpha = 0.5) +
  tm_basemap("OpenStreetMap")



# 5. Grafico simple con ggplot2 -------------------------------------------
install.packages(dplyr)
library(dplyr)
install.packages(ggplot2)
library(ggplot2)

# Grafico de area quemada por departamento
incendios_2024 %>%
  mutate(area_ha = as.numeric(st_area(geometry)) / 10000) %>%
  group_by(departamento) %>%
  summarise(area_quemada_ha = sum(area_ha, na.rm = TRUE)) %>%
  ggplot(aes(reorder(departamento, area_quemada_ha),
             area_quemada_ha)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(
    title = "Superficie quemada por departamento (Córdoba, 2024)",
    x = "Departamento",
    y = "Área quemada (ha)"
  ) +
  theme_minimal()


