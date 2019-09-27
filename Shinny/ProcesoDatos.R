# Paquetes
library(tidyverse)
library(readxl)
library(lubridate)
library(sf)
library(leaflet)
library(RColorBrewer)

# Leer la DB
DB_file <- "CamFotomultas.xlsx"

DB <- DB_file %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_xlsx, path = DB_file)

# Ubicación Camaras
Camaras <- DB$Hoja1 %>%
  separate(Direccion,
           c("Ub1", "Ub2"),
           sep = " - ", extra = "merge") %>%
  arrange(Ub1) %>%
  mutate(Cod = 1:n()) %>%
  st_as_sf(coords = c("Point_X",
                      "Point_X")) %>%
  st_set_crs(4326)
# Guardar los datos para alimentar la WebApp
saveRDS(Camaras, file.path("Camaras.rds"))

