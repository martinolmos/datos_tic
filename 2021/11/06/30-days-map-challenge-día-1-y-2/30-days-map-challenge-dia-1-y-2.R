library(tidyverse)
library(sf)
sf_use_s2(FALSE)
library(httr)

# Dia 1 - Puntos

provincias_ign <- read_sf("https://wms.ign.gob.ar/geoserver/ows?service=wfs&version=1.1.0&request=GetFeature&typeNames=ign:provincia&outputFormat=application/json")

if(sum(!st_is_valid(provincias_ign)) > 0) {
  provincias_ign <- st_make_valid(provincias_ign)
}

provincias_ign <- st_crop(x = provincias_ign,
                          y = st_bbox(obj = c(xmin=-76.36532,
                                              ymin=-56.75009,
                                              xmax=-51.20850,
                                              ymax=-20.91625)))

nodos_refefo <- read.csv2("https://datos.arsat.com.ar/dataset/8f0b4da0-a40d-4b2b-8fe0-dac06d64152a/resource/15713af0-f384-44c5-8397-c8050162312d/download/puntos-conexion-red-federal-de-fibra-optica-2021-12-01_v1.csv", fileEncoding = "LATIN1")

nodos_refefo <- nodos_refefo %>%
  na.omit() %>%
  st_as_sf(crs = 4326, coords=c("Longitud", "Latitud"), remove=FALSE)

nodos_refefo <- st_crop(x = nodos_refefo,
                        y = st_bbox(obj = c(xmin=-76.36532,
                                            ymin=-56.75009,
                                            xmax=-51.20850,
                                            ymax=-20.91625)))

refefo_nodos_map <- provincias_ign %>% 
  ggplot() + 
  geom_sf() +
  geom_sf(data = nodos_refefo, color = "#2ca25f", size = 0.5) +
  coord_sf(datum = NA) +
  theme_bw() +
  labs(caption = "Fuente: www.datos.gob.ar")

ggsave("content/spanish/post/2021-11-06-30-days-map-challenge-dia-1-y-2/refefo_nodos_map.png", refefo_nodos_map)

# Dia 2 - Lineas

idecom_base_url <- "https://www.idecom.gob.ar/geoserver/ows"
refefo_query <- list(service="wfs",
                     version="1.3.0",
                     request="GetFeature",
                     typeNames="publico:FO118-TZFO-REDFIBRAOPTICA-5-2",
                     CQL_FILTER="OBSERV='ARSAT - ReFeFO'",
                     outputFormat="application/json")

idecom_url <- modify_url(url = idecom_base_url, 
                         query = refefo_query)

traza_refefo_idecom <- read_sf(idecom_url)

refefo_traza_map <- provincias_ign %>% 
  ggplot() +
  geom_sf() +
  geom_sf(data = traza_refefo_idecom, colour="#2b8cbe") +
  coord_sf(datum = NA) +
  theme_bw() +
  labs(caption = "Fuente: www.idecom.gob.ar")

ggsave("refefo_traza_map.png", refefo_traza_map)
