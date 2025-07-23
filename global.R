# global.R

library(shiny)
library(sf)
library(dplyr)
library(classInt)
library(leaflet)

# Cargar datos
estado <- st_read("Data/19ent.shp", options="ENCODING=WINDOWS-1252")%>%
  st_transform(4326) %>%
  st_make_valid() %>%
  filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON"))
mun    <- st_read("Data/19mun.shp", options="ENCODING=WINDOWS-1252")%>%
  st_transform(4326) %>%
  st_make_valid() %>%
  filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON"))
ageb   <- st_read("Data/19a.shp", options="ENCODING=WINDOWS-1252")%>%
  st_transform(4326) %>%
  st_make_valid() %>%
  filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON"))


# Lista única de nombres de municipios
municipios_lista <- unique(mun$NOMGEO)  # Cambia si tu campo de nombre es distinto
